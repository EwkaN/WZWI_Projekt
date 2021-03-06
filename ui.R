library(shiny)
library(tm);
library(memoise);
library(tidyRSS);
library(httr);
library(XML);
library(wordcloud2);
library(stringi);
library(solrium);

shapes<<- list("Circle" = "circle",
               "Cardioid"= "cardioid",
               "Pentagon" = "pentagon",
               "Star"="star",
               "Triangle-forward" = "triangle-forward");
# Define UI for displaying current time ----
ui <- fluidPage(
  # Application title
  titlePanel("Word Cloud"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      selectInput("selection", "Wybierz ksztalt:",
                  choices = shapes),
      hr(),
      sliderInput("freq",
                  "Czestosc wystepowania:",
                  min = 1,  max = 50, value = 5),
      sliderInput("size",
                  "Rozmiar:",
                  min = 0.1,  max = 1, value = 0.3)
    ),
    
    # Show Word Cloud
    mainPanel(
      wordcloud2Output('cloud'),
      plotOutput('cluster')
    )
  )
)