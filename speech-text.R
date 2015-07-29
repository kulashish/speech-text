source('speech-head.R')
library(plyr)
file.speech_text <- 'D:\\Personal\\PhD\\Speech to text\\speech-text-data\\Speech-text data - English.tsv'
data.speech_text <- read.delim(file.speech_text, quote="", stringsAsFactors=F)
data.speech_text <- as.data.frame(sapply(data.speech_text, function(x) gsub("[[:punct:]]", "", x)), stringsAsFactors=F)
data.speech_text <- as.data.frame(sapply(data.speech_text, tolower), stringsAsFactors=F)

counter <- function(data.row) {
  data.actual       <- data.row[1,1]
  data.actual.words <- strsplit(data.actual, " ")[[1]]
  data.frame(lapply(data.row[1,-1], function(x) sum(data.actual.words %in% strsplit(x, " ")[[1]])))
}

howmany <- function(word, row) {
  sum(unlist(lapply(strsplit(row, " "), function(x) word %in% x)))
}

counter2 <- function(data.row) {
  data.actual       <- data.row[1,1]
  data.actual.words <- strsplit(data.actual, " ")[[1]]
  result.df <- as.data.frame(lapply(data.actual.words, howmany, as.character(data.row[1,-1])))
  colnames(result.df)=data.actual.words
  return(result.df)
}

count.word <- adply(data.speech_text, 1, counter)
#*********How many users got a word right?**********
for(i in 1:25) {
  count.users <- adply(data.speech_text[i,], 1, counter2, .expand=F)
  write.table(count.users, file='count.users', append=T, quote=F, row.names=F, sep=",")
}
######################################################
library(ngram)
howmany_ngram <- function(word, row, n=1) {
#  print(word)
#  sum(unlist(lapply(row, function(x) grepl(word, x))))
  result <- sum(unlist(lapply(row, function(x) grepl(word, x))))/length(row)*100
  trunc(result)
}

counter_ngram <- function(data.row, n=1) {
  data.actual       <- data.row[1,1]
  ng                <- ngram(data.actual, n=n)
  data.actual.words <- get.ngrams(ng)
  result.df <- as.data.frame(lapply(data.actual.words, howmany_ngram, data.row[, -1], n))
  colnames(result.df)=data.actual.words
  return(result.df)
}
for(i in 1:25) {
#  count.users <- counter_ngram(data.speech_text[i,], n=2)
  count.users <- adply(data.speech_text[i,], 1, counter_ngram, n=2, .expand=F)
#  print(count.users)
  write.table(count.users, file='count.users.per.2', append=T, quote=F, row.names=F, sep=",")
}
##########################################################
library(readr)
library(dplyr)
for(i in 1:5){
  data.all   <- read_lines(paste0('count.users.per.',i))
  data.all.p <- data.all[c(TRUE,FALSE)]
  data.all.c <- data.all[c(FALSE,TRUE)]
  df         <- data.frame(p=unlist(lapply(data.all.p, strsplit, ',')), 
                         c=unlist(lapply(data.all.c, strsplit, ',')), stringsAsFactors=F) %>% 
    mutate_each(funs(as.numeric), c) %>%
    arrange(c)
  write.csv(df, paste0('df.per.',i), quote=F, row.names=F)
}
##########################################################
library(ggplot2)
library(grid)
library(gridExtra)
plots <- list()

df <- read.csv(paste0('df.per.1'), stringsAsFactors=F)
qplot(df$c, geom='histogram', main='1-gram', xlab='% Speakers', ylab='#1-grams', binwidth=.4)

df <- read.csv(paste0('df.per.2'), stringsAsFactors=F)
qplot(df$c, geom='histogram', main='2-gram', xlab='% Speakers', ylab='#2-grams', binwidth=.4)  

df <- read.csv(paste0('df.per.3'), stringsAsFactors=F)
qplot(df$c, geom='histogram', main='3-gram', xlab='% Speakers', ylab='#3-grams', binwidth=.4)  

df <- read.csv(paste0('df.per.4'), stringsAsFactors=F)
qplot(df$c, geom='histogram', main='4-gram', xlab='% Speakers', ylab='#4-grams', binwidth=.4)  

df <- read.csv(paste0('df.per.5'), stringsAsFactors=F)
qplot(df$c, geom='histogram', main='5-gram', xlab='% Speakers', ylab='#5-grams', binwidth=.4)  

multiplot(plotlist = plots, cols = 2)
#plots <- lapply(count.word[,-1], function(x) qplot(as.numeric(x), geom='histogram', ylim=c(0,20)))
#grid.arrange(plots, ncol=2)
qplot(as.numeric(count.word[1, -1]), geom='histogram', ylim=c(0,20))
