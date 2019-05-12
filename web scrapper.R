install.packages(c("rvest", "stringr", "readr", "xml2", "purrr", "backports", "dplyr"))

library(rvest)
library(stringr)
library(readr)
library(xml2)
library(purrr)
library(backports)
library(dplyr)

steam.all.games <- read.csv("~/Downloads/steam-all-games.csv", comment.char="#")

test <- "https://store.steampowered.com/app/881060/Fortune__Gloria/?snr=1_7_7_230_150_2355" 

steam.all.games$store <- mapply( function(x) sub(".*com/(.*?)/.*", "\\1", x),steam.all.games$main.link.href )
steam.all.games$id <- mapply( function(x) sub(".*app/(.*?)/.*", "\\1", x),steam.all.games$main.link.href )
steam.all.games$id <- mapply( function(x) sub(".*sub/(.*?)/.*", "\\1", x),steam.all.games$id )

appID <- '293160'

ids <- c('293160', '245730', '1031090')

# popParse <- read_html( paste ('https://store.steampowered.com/app/',appID,'/', sep = ""))


map_df(1:900, function(i) {
  
  # simple but effective progress indicator
  cat(".")
  
  appID <- steam.all.games$id[8000 + i]
  store <- steam.all.games$store[8000  + i]
    
  popParse <- read_html(paste ('https://store.steampowered.com/',store,'/',appID,'/', sep = ""))
  
  game.title <-popParse %>%
    html_nodes(".apphub_AppName") %>%
    html_text()
  
  if(is.character(game.title)){
  
  game.date <-popParse %>%
    html_nodes(".date") %>%
    html_text()  %>%
    as.Date(format='%b %d, %Y')
  
  game.tags <- popParse %>%
    html_nodes(".app_tag") %>%
    html_text()
  
  game.tags <- game.tags[-length(game.tags)]  %>%
    str_replace_all( "[\r\n\t]" , "") %>%
    paste( collapse = ',')
  
  game.discount_price <-popParse %>%
    html_nodes(".discount_original_price")  %>%
    html_text()  %>%
    parse_number() 
  
  NonNAindex <- which(!is.na(game.discount_price))
  firstNonNA <- min(NonNAindex)
  
  if(is.numeric(firstNonNA)){
    game.discount_price <- game.discount_price[firstNonNA]
  }else{
    game.discount_price <- 'NA'
  }
  

  game.price <-popParse %>%
    html_nodes(".game_purchase_price ")  %>%
    html_text()  %>%
    parse_number()
  
  NonNAindex <- which(!is.na(game.price))
  firstNonNA <- min(NonNAindex)
  
  if(is.numeric(firstNonNA)){
    game.price <- game.price[firstNonNA]
  }else{
    game.price <- 'NA'
  }
  
  game.description <-popParse %>%
    html_nodes(".game_area_description") %>%
    html_text() %>%
    str_replace_all( "[\r\n\t]" , "")  %>%
    paste( collapse = ',')
  
  p <- if (is.numeric(game.price)) {
    game.price
  } else {
    game.discount_price
  }
  
  # data.frame(id=appID,
  #            title=game.title,
  #            date=game.date,
  #            price = p,
  #            tags = game.tags,
  #            description = game.description,
  #            reviews = game.reviews,
  #            stringsAsFactors=FALSE)

  
  tryCatch({
    
    reviewParse <-read_html(paste('https://steamcommunity.com/app/',appID,'/reviews/?p=1&browsefilter=toprated',sep=""))
    
    game.reviews <-reviewParse %>%
      html_nodes(".apphub_CardTextContent") %>%
      html_text() %>%
      str_replace_all( "[\r\n\t]" , "")  %>%
      paste( collapse = ',')
    
    data.frame(id=appID,
               title=game.title,
               date=game.date,
               price = p,
               tags = game.tags,
               description = game.description,
               reviews = game.reviews,
               stringsAsFactors=FALSE)
    
  }, warning = function(war) {
    
    # warning handler picks up where error was generated
    print(paste("MY_WARNING:  ",war))
    return(f)
    
  }, error = function(err) {
    
    # error handler picks up where error was generated
    return(data.frame())
    print(paste("MY_ERROR:  ",err))
    
  }, finally = {
    
  }) # END tryCatch

  }else{
    data.frame()
  }
  
}) -> games

# games2 <- games
# games10$X <- NULL
# games11$X <- NULL
# games12$X <- NULL
# games13$X <- NULL
# games7$X <- NULL
# games8$X <- NULL
# games9$X <- NULL
# games3$X <- NULL

# games_final = do.call("rbind", list(games, games2, games10, games11, games12, games13, games7, games8, games9, games3))

# games_final = rbind(games, games2, games10, games11, games12, games13, games7, games8, games9, games3,)

write.csv(games, file = "~/Downloads/gamesfinalish.csv", quote = TRUE )

# write.table(games, file = "~/Downloads/games3.txt",row.names=FALSE, na="",col.names=TRUE, sep="\t")

# write an object  
dput(games, "~/Downloads/out")
# read an object
df  <- dget("out")

# gamesfinalish$reviews <- str_replace_all(gamesfinalish$reviews, "[^[:alnum:]]", " ")
# gamesfinalish$description <- str_replace_all(gamesfinalish$description, " [^[:alnum:]]", " ")
# gamesfinalish$X <- NULL

