# --------- packages ---------
library(shiny)
library(udpipe)
library(textrank)
library(wordcloud)
library(dplyr)
library(factoextra)
library(stringr)
library(igraph)
library(ggraph)
library(ggplot2)
library(dplyr)
# --------- loading data ---------
data = read.csv("~/Downloads/gamesfinalish.csv")
data$reviews = as.character(data$reviews)
data$description = as.character(data$description)
data = data[-which(data$description==""),]
data = data[-which(data$reviews==""),]
data = data[-which(is.na(data$price)),]
data = data[-which(is.na(data$date)),]
data = data[-which(str_length(data$reviews) > 2500),]
# changing types of data
data$date = format(as.Date(data$date), "%Y")
data$date = as.numeric(data$date)
data$tags = as.character(data$tags)
data$title = as.character(data$title)
data$description = as.character(data$description)
data$price = as.numeric(data$price)

data_fps = data[grep("FPS", data$tags),]
data_rpg = data[grep("RPG", data$tags),]
data_rpg = head(data_rpg, 80)
data_action = data[grep("Action", data$tags),]
data_action = head(data_action, 80)
data_strategy= data[grep("Strategy", data$tags),]
data_strategy = head(data_strategy, 80)
data_racing = data[grep("Racing", data$tags),]
data_racing = head(data_racing, 80)
data_horror = data[grep("Horror", data$tags),]
data_horror = head(data_horror, 80)
data  = rbind(data_fps,data_rpg,data_action,data_strategy,data_racing,data_horror)
data$reviews = as.character(data$reviews)
# for the word cloud 
df = data
df$reviews = str_replace_all(df$reviews,
                             "game|review|player|time|level|thing|new|many|[Ee]arly|more|same|such|own|other|edtion",
                             "")
df$reviews = as.character(df$reviews)

# for the word distance map
df2 = data
df2$reviews = gsub("(f|ht)tp\\S+\\s*", "", df2$reviews)

# loading the model
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)

##preprocess prediction stuff
corpus = Corpus(VectorSource(paste(data$description))) # description or reviews or all of them?
ndocs = length(corpus)
minDocFreq <- ndocs * 0.01
maxDocFreq <- ndocs * 0.7 #(not too rare, not too frequent words)
clean_corpus <- tm_map(corpus, tolower)
clean_corpus = tm_map(clean_corpus, removePunctuation)
clean_corpus = tm_map(clean_corpus, stripWhitespace)
clean_corpus = tm_map(clean_corpus, stemDocument)
clean_corpus <- tm_map(clean_corpus, removeWords, 
                              c(stopwords("english"))) # stop_vec - vector of additional words we don't want
dtm_no_stops<- DocumentTermMatrix(clean_corpus, control = list(bounds = list(global = c(minDocFreq, maxDocFreq))))
row.names(dtm_no_stops) = data$title
df = as.data.frame(as.matrix(dtm_no_stops))

##trying to do fancy frequency stuff
freq_inverse = 1/colSums(as.matrix(df))
##

for (col in colnames(df)){
  df[,col] = df[,col] > 0
  df[df[,col],col] = freq_inverse[col]
}
# prediction stuff preprocessing done


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
                       radioButtons("stuff", 
                                    h3("Some additional options"), 
                                    choices = list("reviews" = "reviews", 
                                                   "description" = "description"),
                                    selected = c("reviews")),
                       selectInput("genre", h3("Choose genre"), 
                                   choices = list("FPS" = "FPS", 
                                                  "RPG" = "RPG", 
                                                  "Action" = "Action",
                                                  "Strategy"=  "Strategy",
                                                  "Racing" = "Racing",
                                                  "Horror" = "Horror"), selected = "Horror"),
                       
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
                                   tabPanel("COOC",h2("The most related words"), plotOutput("distance_plot"))
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
    if(input$stuff == "reviews"){
      Data = df$reviews[grep(input$genre, df$tags[which(df$date > input$date_slider[1]
                                                        & df$date < input$date_slider[2]
                                                        & df$price > input$price_slider[1]
                                                        & df$price < input$price_slider[2])])
                        ]
    }
    else{
      Data = df$description[grep(input$genre, df$tags[which(df$date > input$date_slider[1]
                                                            & df$date < input$date_slider[2]
                                                            & df$price > input$price_slider[1]
                                                            & df$price < input$price_slider[2])])
                            ]
    }
    
    return(Data)
  })
  
  Dataset2 <- reactive({
    if(input$stuff == "reviews"){
      Data2 = df2$reviews[grep(input$genre, df$tags[which(df2$date > input$date_slider[1]
                                                          & df2$date < input$date_slider[2]
                                                          & df2$price > input$price_slider[1]
                                                          & df2$price < input$price_slider[2])])
                          ]
    }
    else{
      Data2 = df2$description[grep(input$genre, df$tags[which(df2$date > input$date_slider[1]
                                                              & df2$date < input$date_slider[2]
                                                              & df2$price > input$price_slider[1]
                                                              & df2$price < input$price_slider[2])])
                              ]
    }
    
    return(Data2)
  })
  
  english_model = reactive({
    # load english model for annotation from working dir
    english_model = ud_model  # file_model only needed
    return(english_model)
  })
  
  annot.obj = reactive({
    
    if(identical(Dataset(), character(0)))
    {x <- udpipe_annotate(english_model(),x = "empty nothing")
    x <- as.data.frame(x)}
    else
    {x <- udpipe_annotate(english_model(),x = as.character(Dataset()))
    x <- as.data.frame(x)}
    
    return(x)
  })
  
  annot.obj2 = reactive({
    
    if(identical(Dataset2(), character(0)))
    {x <- udpipe_annotate(english_model(),x = "empty nothing")
    x <- as.data.frame(x)}
    else
    {x <- udpipe_annotate(english_model(),x = as.character(Dataset2()))
    x <- as.data.frame(x)}
    
    return(x)
  })
  
  annot.stats = reactive({
    stats1 <- keywords_collocation(x = annot.obj2(), 
                                   term = "token", group = c("doc_id", "paragraph_id", "sentence_id"),
                                   ngram_max = 4)
    stats1 <- cooccurrence(x = subset(annot.obj2(), upos %in% c("NOUN", "ADJ", "VERB")), 
                           term = "lemma", group = c("doc_id", "paragraph_id", "sentence_id"))
    stats1 <- cooccurrence(x = annot.obj2()$lemma, 
                           relevant = annot.obj2()$upos %in% c("NOUN", "ADJ","VERB"))
    # wordnetwork <- head(stats, 20)
    # wordnetwork <- graph_from_data_frame(wordnetwork)
    return(stats1)
  })
  
  gameTable <- eventReactive(input$find_games, {
    return(input$game_description)
  })
  
  
  
  output$adj_plot = renderPlot({
    all_adj = annot.obj() %>% subset(., upos %in% "ADJ") 
    top_adj = txt_freq(all_adj$lemma)  # txt_freq() calcs noun freqs in desc order
    wc_adj =  wordcloud(words = top_adj$key, freq = top_adj$freq,
                        min.freq = 5,
                        max.words=100, random.order=FALSE, rot.per=0.35, 
                        colors=brewer.pal(8, "Dark2"))
    
  })
  
  output$noun_plot = renderPlot({
    all_nouns = annot.obj() %>% subset(., upos %in% "NOUN") 
    top_nouns = txt_freq(all_nouns$lemma)  # txt_freq() calcs noun freqs in desc order
    wc_noun =  wordcloud(words = top_nouns$key, freq = top_nouns$freq,
                         min.freq = 5,
                         max.words=100, random.order=FALSE, rot.per=0.35, 
                         colors=brewer.pal(8, "Dark2"))
  })
  
  output$distance_plot = renderPlot({
    if(identical(is.na(annot.stats()$cooc),logical(0)))
    {wordcloud("empty set")}
    else
    {
      wordnetwork <- head(annot.stats(), 50)
      wordnetwork <- graph_from_data_frame(wordnetwork)
      ggraph(wordnetwork, layout = "fr") +
        geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
        geom_node_text(aes(label = name), col = "blue", size = 4) +
        theme_graph(base_family = "Arial Narrow") +
        theme(legend.position = "none") +
        labs(title = "Cooccurrences within 3 words distance", subtitle = "Nouns & Adjective & Verb")
    }
    
    
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
    input = unlist(strsplit(x," "))
    input_corp = Corpus(VectorSource(input))
    input_corp = tm_map(input_corp,tolower)
    input_corp = tm_map(input_corp,removePunctuation)
    input_corp = tm_map(input_corp,stemDocument)
    input_corp = tm_map(input_corp, removeWords, c(stopwords("english"))) 
    input = as.vector(unlist(input_corp))
    input = input[input %in% colnames(df)]
    result = df[,input]
    if (dim(df[,input])[2] > 1){
      result = rowSums(result, na.rm = TRUE)
    } else {
      result = sum(result, na.rm = TRUE)
    }
    result = sort(-result)
    result = result[1:10]
    answer = attr(result,"names")
    table_data = data[data$title %in% answer,c("title","date","price","tags")]
    return(table_data)
  })
  
  
}
shinyApp(ui, server)
