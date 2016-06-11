# dataFuncs
# functions for reading, parsing prince data
library(tm)
library(stringr)

transformCorpus <- function(docs, smart=FALSE, custom=NULL) {
  # docs - corpus
  # smart - use SMART stoplist, otherwise tm english stoplist
  # custom - vector of additional stopwords  
  if (smart)
    stoplist <- c(stopwords("SMART"), custom)
  else
    stoplist <- c(stopwords("en"), custom)
  toSpace <- content_transformer(function(x, pattern){gsub(pattern, " ", x)})
  docs <- tm_map(docs, content_transformer(tolower), lazy=T)
  docs <- tm_map(docs, toSpace, "-|/")
  docs <- tm_map(docs, removeNumbers, lazy=T)
  docs <- tm_map(docs, removePunctuation)
  docs <- tm_map(docs, removeWords, stoplist, lazy=T)
  docs <- tm_map(docs, stemDocument, lazy=T)
  docs <- tm_map(docs, stripWhitespace, lazy=T)
  return(docs)
}

readArticles <- function(filename) {
  # readArticles
  # return data frame with articles
  # filename - string
  articles <- readLines(filename)
  articles <- Map(function(x){strsplit(paste(x, " "), '|~|', fixed=T)}, articles)
  articles <- Reduce(rbind, Map(unlist, articles))
  colnames(articles) <- c("id", "title", "date", "kicker", "author", "au.title", "article", "italic")
  articles <- data.frame(articles, stringsAsFactors = FALSE)
  #   articles$date <- as.Date(as.character(articles$date), format="\t%B %d %Y")
  #   articles$article <- as.character(articles$article)
  #   articles$id <- as.numeric(as.character(articles$id))
  #   articles$title <- as.character(articles$title)
  #   articles$kicker <- as.character(articles$kicker)
  #   articles$author <- as.character(articles$author)
  #   articles$au.title <- as.character(articles$au.title)
  articles$date <- as.Date(articles$date, format="\t%B %d %Y")
  articles$id <- as.numeric(articles$id)
  
  articles <- articles[with(articles, order(date)),]
  return(articles)
}

readComments <- function(filename) {
  # readComments
  # return data frame with comments
  # filename - string
  comments <- readLines(filename)
  comments <- Map(function(x){strsplit(x,'|~|', fixed=T)}, comments)
  comments <- Reduce(rbind, Map(unlist, comments))
  colnames(comments) <- c("id", "username", "comment", "upvotes", "time", "cid", "pid", "disquser")
  comments <- data.frame(comments, stringsAsFactors=FALSE)
  comments$upvotes <- as.numeric(comments$upvotes)
  comments$id <- as.numeric(comments$id)
  comments$time <- as.POSIXlt(comments$time, format="%A, %B %d, %Y %I:%M %p")
  #comments$comment <- as.character(comments$comment)
  
  return(comments)
}

commCount <- function(articles, comments) {
  # count number of comments for each article
  # return article data frame with counts
  articles$num.comm <- 0
  for (i in 1:nrow(articles)) {
    article.id <- articles[i,]$id
    articles[i,]$num.comm <- length(which(comments$id == article.id))
  }
  return(articles)
}
