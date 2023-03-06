
library(shiny)
library(tidyverse)
library(plotly)

Nfl <-  read_delim("Basic_Stats.csv")

ui <- fluidPage(

    titlePanel("NFL player statistics "),
    h6("Tawsif Ahmed - Info 201 Winter"),
    mainPanel(
    tabsetPanel(
      tabPanel("About",
              p("This apps uses the data collected about NFL players 
                throughtout the league", strong("Kaggle"), "which ranges form 
                1920 to 2016 \n"),
              p("The weights are measured in lbs and the height are in inches \n"),
              p("The dataset contains", em(nrow(Nfl)), "observations", em(ncol(Nfl)), 
                " variables \n"),
              p("Here are some random samples from the data \n"),
              mainPanel(
                    tableOutput("about"),
                    )
              ),
      tabPanel("Plot",
                 sidebarPanel(
                   p("This pages shows a graph of", em("Height vs Average Weight"), "for players 
                     in particular position. Please select a", strong("position"), "to analyze
                     and a", strong("color"), "of your pleasing."),
                   fluidRow(
                     column(6,
                            radioButtons("color", "Choose color",
                                         choices = setNames(c("deeppink", "forestgreen", "orangered",
                                                              "royalblue3", "goldenrod"),
                                                            c("Pink", "Green", "Orange",
                                                              "Blue", "Gold"))
                            )
                     ),
                     column(6, 
                            uiOutput("CheckboxPosition")
                     )
                   )
                 ),
                mainPanel(
                  plotOutput("plot", width = "800px", height = "600px"),
                  textOutput("Info1")
                  )
      ),
      tabPanel("Table",
               sidebarPanel(
                 p("This page shows the", strong("amount of players"), "that were drafted from the college
                 thoughtout the league, their", em("position, average height and weight"), 
                 "of those position. Please, select college/colleges to analyze. \n"),
                 p(em("In order to decrease size of the list, colleges needed have at least 20 players 
                 to be drafted. \n")),
                 uiOutput("CheckboxCollege")
               ),
               mainPanel(
                 textOutput("Info2"),
                 tableOutput("Table"),
                 textOutput("Info3")
                 
               )
               )
      
  )
)
)

server <- function(input, output) {

    #Creating a sample of the data
    output$about <- renderTable({
      Nfl %>% 
        sample_n(5)
    })
    
    #Reactive for the position checkbox 
    position_data <- reactive({
      s1 <- Nfl %>% 
        filter(Position %in% input$Position_Select)
      
    })
    
    #The position check box 
    output$CheckboxPosition <- renderUI({
      s2 <- Nfl %>% 
        filter(!is.na(Position))
      checkboxGroupInput("Position_Select", "Choose position",
                         choices = unique(s2$Position))
    })
    
    #The position plot 
    output$plot <- renderPlot({
      p <- position_data() %>% 
        filter(!is.na(`Weight (lbs)`),
               !is.na(`Height (inches)`)) %>% 
        mutate(Weight = `Weight (lbs)`, Height = `Height (inches)`) %>%
        group_by(Position, Height) %>% 
        mutate(mWeight = mean(Weight)) %>% 
        ggplot(aes(Height, mWeight, size = 2))+
        geom_point(col = input$color)+
        labs(title = "Height vs. Average Weight",
             x = "Height (ins)", y = "Average Weight (lbs)")+
        theme(plot.title = element_text(hjust = 0.5))
      if(nrow(position_data()) == 0){
        p <- p + 
          labs(title = "Please select a position")
      }
        p

    })
    
    #The text under the plot 
    output$Info1 <- renderText({
      if(nrow(position_data()) > 0){
      avg_Weight <- position_data() %>%
        filter(!is.na(`Weight (lbs)`)) %>% 
        summarize(mWeight = mean(`Weight (lbs)`))
      avg_Height <- position_data() %>%
        filter(!is.na(`Height (inches)`)) %>% 
        summarize(mHeight = mean(`Height (inches)`))
      paste("This positions' average weight was", round(avg_Weight, 2), " lbs and
            the average height was", round(avg_Height, 2), "in")
      }
    })
    
    #The College check box 
    output$CheckboxCollege <- renderUI({
      s3 <- Nfl %>% 
        filter(!is.na(College), College != "No College",
               !is.na(Position)) %>% 
        group_by(College) %>% 
        mutate(count = n()) %>% 
        filter(count > 20)
      
      checkboxGroupInput("College_Select", "Choose College",
                         choices = unique(s3$College),
                         selected = "Clemson")
    })
    
    #Reactive for the college check box
    College_data <- reactive({
      s4 <- Nfl %>% 
        filter(College %in% input$College_Select)
    })
    
    
    #The college table 
    output$Table <- renderTable({
      t <- College_data() %>% 
        filter(!is.na(Position)) %>%
        group_by(Position) %>% 
        summarize(count = n(), Height = mean(`Height (inches)`),
                  Weight = mean(`Weight (lbs)`)) 
      
    })
    
    #The texts above the table
    output$Info2 <- renderText({
      total <- College_data() %>% 
        filter(!is.na(Position)) %>%
        summarize(count = n())
      paste("The college/colleges had total", total, "amount of players
            drafted into the NFl from 1920-2016 \n")
        
    })
    
    output$Info3 <- renderText({
      avg_Weight <- College_data() %>%
        filter(!is.na(`Weight (lbs)`)) %>% 
        summarize(mWeight = mean(`Weight (lbs)`))
      avg_Height <- College_data() %>%
        filter(!is.na(`Height (inches)`)) %>% 
        summarize(mHeight = mean(`Height (inches)`))
      paste("This colleges' players average weight was", round(avg_Weight, 2), 
      " lbs and the average height was", round(avg_Height, 2), "in \n")
    })

}

shinyApp(ui = ui, server = server)
