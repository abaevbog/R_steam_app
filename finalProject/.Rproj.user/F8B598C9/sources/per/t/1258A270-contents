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
                                                  "Fighting" = "Fighting",
                                                  "Strategy"=  "Strategy",
                                                  "Racing" = "Racing",
                                                  "Horror" = "Horror"), selected = "FPS"),
                       
                       sliderInput("price_slider", h3("Price range in $"),
                                   min = 0, max = 60, value = c(0, 1)),
                       
                       sliderInput("date_slider", h3("Release dates"),
                                   min = 1990, max = 2018, value = c(0, 1)) 
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


