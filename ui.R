#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(png)
suppressWarnings(library(markdown))

# Define UI for application

shinyUI(fluidPage(
  
  # Application title
  img(src='banner.PNG', align = "center"),
  br(),
  titlePanel("Word Predictor App"),
  h4("This app let you know the next probably word in a expression."),
  h6(strong("Author: franmarq@gmail.com")),
  br(),
  br(),
  sidebarLayout(
    sidebarPanel(
      textInput("inputText", "Expression:",value = " "),
      submitButton("Submit"),
      br(),
      h5('Instructions'),
      helpText("This app was built to better results in English languaje and when you use three or less words."),
      helpText("Follow the next steps:"),
      helpText("1. Write some words in the 'Expression:' box"),
      helpText("2. Press 'Submit' button to obtain the result"),
      helpText("3. The 'next predicted word' will appear on the right side panel"),
      br(),
      helpText("*If the app can't obtain a result, it will return 'Try_Other_Word!'"),
      br(),
      br(),
      br(),
      br()
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h4("Next Word Predicted:"),
      h2(strong(textOutput('nw')))
    )
  )
))
