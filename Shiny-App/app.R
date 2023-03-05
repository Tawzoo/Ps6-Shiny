
library(shiny)
library(tidyverse)
library(plotly)

Nfl <-  read_delim("Basic_Stats.csv")

ui <- fluidPage(

    titlePanel("NFL player statistics "),
    h6("Tawsif Ahmed - Info 201 Winter"),
    tabsetPanel(
      tabPanel("About",
              p("This apps uses the data collected about NFL players 
                throughtout the league", strong("Kaggle"), "which ranges form 
                1920 to 2016"),
              p("The weights are measured in lbs and the height are in inches"),
              p("The dataset contains", em(nrow(Nfl)), "observations", em(ncol(Nfl)), " variables"),
      ),
      mainPanel(
        p("Here are some samples from the data "),
        dataTableOutput("sample")
      ),
      tabPanel("Plot"),
      
)
)

server <- function(input, output) {

    output$sample <- renderDataTable({
      Nfl %>% 
        sample_n(5)
    })
}

shinyApp(ui = ui, server = server)
