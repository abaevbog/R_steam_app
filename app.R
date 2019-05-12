# --------- packages ---------
library(shiny)
library(udpipe)
library(textrank)
library(wordcloud)
library(dplyr)
library(factoextra)
library(stringr)
# --------- loading data ---------
data = read.delim("/Users/bogdanabaev/College/R_code/FinalProj/games.txt")
# changing types of data
data$date = format(as.Date(data$date), "%Y")
data$date = as.numeric(data$date)
data$tags = as.character(data$tags)
data$title = as.character(data$title)
data$description = as.character(data$description)
data$reviews = str_replace_all(data$reviews,
                               "game|review|player|time|level|thing|new|many|[Ee]arly|more|same|such|own|other",
                               "")
data$reviews = as.character(data$reviews)
data$price = as.numeric(data$price)

df = data[-which(data$review == ""),]

df1 = data[,c(3,4)]
df1 = df1[-which(is.na(df1$date)),]
df1 = df1[-which(is.na(df1$price)),]

k2 = kmeans(scale(df1), centers = 3, nstart = 25)
print("A")
# loading the model
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
# --------- ui code ---------
ui <- fluidPage(
  
  titlePanel("Steam games"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("Steam data", value = 1, ""),
                  tabPanel("Game lookup", value = 2, "")
      ),

      conditionalPanel(condition = "input.tabs == '1'", 
                       checkboxGroupInput("stuff", 
                                          h3("Some additional options"), 
                                          choices = list("Blah" = "Blah", 
                                                         "Blah" = "Blah", 
                                                         "Blah" = "Blah",
                                                         "Blah"= "Blah",
                                                         "Blah" = "Blah"),
                                          selected = c("Blah","Blah")),
                       selectInput("genre", h3("Choose genre"), 
                                   choices = list("FPS" = "FPS", 
                                                  "RPG" = "RPG", 
                                                  "Action" = "Action",
                                                  "Strategy"=  "Strategy",
                                                  "Racing" = "Racing",
                                                  "Horror" = "Horror"), selected = "FPS"),
                       
                       sliderInput("price_slider", h3("Price range in $"),
                                   min = 0, max = 60, value = c(0, 60)),
                       
                       sliderInput("date_slider", h3("Release dates"),
                                   min = 1990, max = 2018, value = c(1990,2018)) 
      ), 
      conditionalPanel(condition = "input.tabs == '2'", 
                       h3("Describe what game you want"),
                       textAreaInput("game_description", "20 words max", value = "", width = '100%', height = '200%',
                                     cols = NULL, rows = 4, placeholder = NULL, resize = NULL),
                        actionButton("find_games",value = 0, "Submit"))
    ),
    
    mainPanel(
      conditionalPanel(condition = "input.tabs == '1'",
                       tabsetPanel(id="tab_games",
                                   tabPanel("Adjectives",h2("Most frequently used adjectives in reviews of the selected games"), plotOutput("adj_plot")), 
                                   tabPanel("Nouns",h2("Most frequently used nouns in reviews of the selected games"), plotOutput("noun_plot")), 
                                   tabPanel("Some other graph maybe",h2("something else"), plotOutput("something_plot"))
                       )),
      conditionalPanel(condition = "input.tabs == '2'",
                       tableOutput("matched_games"))
      
    )
  )
)

# --------- Server code ---------
server <- function(input, output) {
  reactVal = reactiveValues()
  

  
  Dataset <- reactive({
    Data = df$reviews[grep(input$genre, df$tags[which(df$date > input$date_slider[1]
                                                                       & df$date < input$date_slider[2]
                                                                       & df$price > input$price_slider[1]
                                                                       & df$price < input$price_slider[2])])
                                     ]
    return(Data)
  })
  
  
  english_model = reactive({
    # load english model for annotation from working dir
    english_model = ud_model  # file_model only needed
    return(english_model)
  })
  
  annot.obj = reactive({
    x <- udpipe_annotate(english_model(),x = as.character(Dataset()))
    x <- as.data.frame(x)
    return(x)
  })
  
  gameTable <- eventReactive(input$find_games, {
    return(input$game_description)
  })
  
  
  
  output$adj_plot = renderPlot({
    all_adj = annot.obj() %>% subset(., upos %in% "ADJ") 
    top_adj = txt_freq(all_adj$lemma)  # txt_freq() calcs noun freqs in desc order
    wc_adj =  wordcloud(words = top_adj$key, freq = top_adj$freq,
              min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
    
  })
  
  output$noun_plot = renderPlot({
    all_nouns = annot.obj() %>% subset(., upos %in% "NOUN") 
    top_nouns = txt_freq(all_nouns$lemma)  # txt_freq() calcs noun freqs in desc order
    wc_noun =  wordcloud(words = top_nouns$key, freq = top_nouns$freq,
                   min.freq = 1,
                   max.words=200, random.order=FALSE, rot.per=0.35, 
                   colors=brewer.pal(8, "Dark2"))
  })
  
  output$something_plot= renderPlot({
    fviz_cluster(k2, data = df1)
  })
  
  do_table = function(find_games){
    print(find_games)
    if (find_games == 1){
      return(iris)
    } else {
      return(NULL)
    }
  }
  
  output$matched_games = renderTable({
    x = gameTable()
    print(x)
    return(iris)
  })
  
  
}
shinyApp(ui, server)
