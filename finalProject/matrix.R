library(tm)
# make the huge matrix with frequencies
data = read.delim("/Users/bogdanabaev/College/R_code/FinalProj/games.txt")
test_str = Corpus(VectorSource(data$description))

ndocs = length(test_str)
minDocFreq <- ndocs * 0.1
maxDocFreq <- ndocs * 0.5 # tweek these to get most relevant (not too rare, not too frequent words)
dtm<- DocumentTermMatrix(test_str, control = list(bounds = list(global = c(minDocFreq, maxDocFreq))))
clean_tweets_corpus <- tm_map(test_str, tolower)

clean_tweets_corpus = tm_map(clean_tweets_corpus, removePunctuation)
clean_tweets_corpus = tm_map(clean_tweets_corpus, stemDocument)

clean_tweets_corpus <- tm_map(clean_tweets_corpus, removeWords, 
                              c(stopwords("english"))) # stop_vec - vector of additional words we don't want
dtm_no_stops<- DocumentTermMatrix(clean_tweets_corpus, control = list(bounds = list(global = c(minDocFreq, maxDocFreq))))
row.names(dtm_no_stops) = data$title
df = as.data.frame(as.matrix(dtm_no_stops))
# first, make sure all words in input at in the function
input = c("find", "reason", "including", "many")
input_corp = Corpus(VectorSource(input))
input_corp = tm_map(input_corp,tolower)
input_corp = tm_map(input_corp,removePunctuation)
input_corp = tm_map(input_corp,stemDocument)
input_corp = tm_map(input_corp, removeWords, c(stopwords("english"))) 
input = as.vector(unlist(input_corp))
input = input[input %in% colnames(df)]
result = df[,input]


for (col in colnames(result)){
  result[,col] = result[,col] > 1
  
}
if (dim(df[,input])[2] > 1){
  result = rowSums(result, na.rm = TRUE)
} else {
    result = sum(result, na.rm = TRUE)
  }
result = sort(-result)
result = result[1:10]
answer = attr(result,"names")



