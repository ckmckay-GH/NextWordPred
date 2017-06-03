#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ngram)


# Define UI for application that returns a next word prediction
shinyUI(fluidPage(
        titlePanel("Next Word Prediction Tool"),
        sidebarLayout(
                sidebarPanel(
                        h4("Please input a phrase of two or more words"),
                        textInput("wordString", "Input a Phrase"),
                        
                        h4("Click for Prediction"),
                        submitButton("PREDICT"),
                        
                        h4("Predicted next word:"),
                        textOutput("wordPred")
                        ),
        
        mainPanel(
                h3("Instructions"),
                h5("1. Enter a phrase of at least two words."),
                h5("2. Press the PREDICT button."),
                h5("3. Wait about 10 seconds for a prediction of the next word in the phrase.")
                )
        )
))
