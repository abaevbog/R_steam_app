server <- function(input, output) {
  reactVal = reactiveValues()
  
  observeEvent(input$price_slider, {
    reactVal$price = input$price_slider
  }, ignoreNULL = FALSE)
  observeEvent(input$date_slider, {
    reactVal$date = input$date_slider
  }, ignoreNULL = FALSE)
  observeEvent(input$genre, {
    reactVal$genre = input$genre
  }, ignoreNULL = FALSE)
  
  gameTable <- eventReactive(input$find_games, {
    return(input$game_description)
  })
  
  
  
  output$adj_plot = renderPlot({

    #the lines below will leave
    dummy = c(1,2,3)
    barplot(dummy, col = NA, border= NA, axes = FALSE)
    
  })
  
  output$noun= renderPlot({
    
    #the lines below will leave
    dummy = c(1,2,3)
    barplot(dummy, col = NA, border= NA, axes = FALSE)
    
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