#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ngram)

setwd("~/Coursera/Capstone/NextWordPred")

## load ngrams
load("~/Coursera/Capstone/NextWordPred/unigram.Rda")
load("~/Coursera/Capstone/NextWordPred/bigram.Rda")
load("~/Coursera/Capstone/NextWordPred/trigram.Rda")
load("~/Coursera/Capstone/NextWordPred/quadgram.Rda")
load("~/Coursera/Capstone/NextWordPred/quintgram.Rda")

## load profanity file
bad_words <- read.table("~/Coursera/Capstone/NextWordPred/bad-words.txt", header=FALSE, strip.white=TRUE, skipNul = TRUE)

## Stop words
myStopWords <- c("at", "be", "on", "of", "in", "so","the", "and", "with", "that", "from", "into", 
                 "for", "about", "but", "this", "a", "sothe")

testAnswer <- NULL

## nextWord function
nextWord <- function(text) {
        
        ## clean input text
        text <- tolower(text)
        text <- gsub("[[:punct:]]", "", text)
        text <- gsub(pattern="[^[:alpha:]]", " ", text)
        
        ## remove bad words
        pattern <- paste0("\\b(?:", paste(bad_words, collapse = "|"), ")\\b ?")
        text <- gsub(pattern, " fooo4s ", text, perl = TRUE)
        text <- text[!grepl("fooo4s", text)]
        
        ## remove stopwords
        pattern <- paste0("\\b(?:", paste(myStopWords, collapse = "|"), ")\\b ?")
        text <- gsub(pattern, "", text, perl = TRUE)
        
        ## cleanup spaces
        text <- gsub(pattern="\\s+", " ", text)
        testString <- gsub("^\\s+|\\s+$", "", text)
        
        test_words <- wordcount(testString)
        
        if(test_words > 4) {
                testString <- tail(strsplit(testString, split=" ")[[1]],4)
                testString <- concatenate(testString)
                test_words <- wordcount(testString)
        }
        
        if(test_words == 4) {
                testString <- paste0("^",testString, " ")
                testMatch <- grepl(testString, quintgram$ngrams)
                testMatchDF <- quintgram[testMatch,]
                match <- nrow(testMatchDF)
                
                if (match >= 1) { 
                        testAnswer <- as.character(testMatchDF$ngrams[1])
                        testAnswer <- tail(strsplit(testAnswer, split=" ")[[1]],1)
                }
                if (match < 1) {
                        testString <- tail(strsplit(testString, split=" ")[[1]],3)
                        testString <- concatenate(testString)
                        test_words <- wordcount(testString)
                }
        }
        
        
        if(test_words == 3) {
                testString <- paste0("^",testString, " ")
                testMatch <- grepl(testString, quadgram$ngrams)
                testMatchDF <- quadgram[testMatch,]
                match <- nrow(testMatchDF)
                
                if (match >= 1) { 
                        testAnswer <- as.character(testMatchDF$ngrams[1])
                        testAnswer <- tail(strsplit(testAnswer, split=" ")[[1]],1)
                }
                if (match < 1) {
                        testString <- tail(strsplit(testString, split=" ")[[1]],2)
                        testString <- concatenate(testString)
                        test_words <- wordcount(testString)
                }
        }
        
        if(test_words == 2) {
                testString <- paste0("^",testString, " ")
                testMatch <- grepl(testString, trigram$ngrams)
                testMatchDF <- trigram[testMatch,]
                match <- nrow(testMatchDF)
                
                if (match >= 1) { 
                        testAnswer <- as.character(testMatchDF$ngrams[1])
                        testAnswer <- tail(strsplit(testAnswer, split=" ")[[1]],1)
                }        
                
                if (match < 1) {
                        testString <- tail(strsplit(testString, split=" ")[[1]],1)
                        test_words <- wordcount(testString)
                }
        }
        
        if(test_words == 1) {
                testString <- paste0("^",testString, " ")
                testMatch <- grepl(testString, bigram$ngrams)
                testMatchDF <- bigram[testMatch,]
                match <- nrow(testMatchDF)
                if (match >= 1) { 
                        testAnswer <- as.character(testMatchDF$ngrams)
                        testAnswer <- tail(strsplit(testAnswer, split=" ")[[1]],1)
                }
                
                if (match < 1) {
                        testAnswer <- unigram[order(unigram$freq,decreasing = T),1]
                        testAnswer <- head(testAnswer,1)
                }
        }   
        
        wordPred <- gsub("^\\s+|\\s+$", "", testAnswer)
        return(wordPred)
}

# Shiny server next word prediction
shinyServer(function(input, output) {
   
        output$wordPred <- renderText({
                nextWord(input$wordString)
        })
        
}) 
        
      
        
   
  

