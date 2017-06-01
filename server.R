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

setwd("~/Coursera/Capstone/nextword")

## load ngrams
unigram <- load("unigram.Rda")
bigram <- load("bigram.Rda")
trigram <- load("trigram.Rda")
quadgram <- load("quadgram.Rda")
quintgram <- load("quintgram.Rda")

## load profanity file
bad_words <- read.table("bad-words.txt", header=FALSE, strip.white=TRUE, skipNul = TRUE)

## Stop words
myStopWords <- c("at", "be", "on", "of", "in", "so","the", "and", "with", "that", "from", "into", 
                 "for", "about", "but", "this", "sothe")

## textClean function
textClean <- function(text) {
        
        ## clean input text
        text <- tolower(text)
        text <- gsub("[[:punct:]]", "", text)
        text <- gsub(pattern="[^[:alpha:]]", " ", text)

        ## remove bad words
        bad_words<- as.character(bad_words$V1)
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

        return(testString)
}

## nextWord function
nextWord <- function(testString) {
        
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
        
        testAnswer <- gsub("^\\s+|\\s+$", "", testAnswer)
        return(testAnswer)
                       
                       
}

# Shiny server next word prediction
shinyServer(function(input, output) {
   
        ## Chect for input
        ## if none, ask for input

        
        ## Clean input
        textClean <- reactive({
        
                text <- input$wordString
                cleanText(text)
                output$nextWord <- renderText(testString)
                
        }) 
        
        ## predict words
        ##predictWord <- reactive({nextWord(testString)        })
        
      
        
          })
  

