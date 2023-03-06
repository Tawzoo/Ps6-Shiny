
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
              sidebarPanel(
                fluidRow(
                  column(10,
                    tableOutput("about")
                  ),
                  column()
              )
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
                 
               ),
               mainPanel(
                 
               )
               )
      
  )
)
)

server <- function(input, output) {

    output$about <- renderTable({
      Nfl %>% 
        sample_n(5)
    })
    
    position_data <- reactive({
      s1 <- Nfl %>% 
        filter(Position %in% input$Position_Select)
      
    })
    output$CheckboxPosition <- renderUI({
      s2 <- Nfl %>% 
        filter(!is.na(Position))
      checkboxGroupInput("Position_Select", "Choose position",
                         choices = unique(s2$Position), selected = "QB"
                        )
    })
    output$plot <- renderPlot({
      p <- position_data() %>% 
        filter(!is.na(`Weight (lbs)`),
               !is.na(`Height (inches)`)) %>% 
        mutate(Weight = `Weight (lbs)`, Height = `Height (inches)`) %>%
        group_by(Position,Height) %>% 
        mutate(mWeight = mean(Weight)) %>% 
        ggplot(aes(Height, mWeight, position = "dodge"))+
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
    
    output$Info1 <- renderText({
      avg_Weight <- position_data() %>%
        filter(!is.na(`Weight (lbs)`)) %>% 
        summarize(mWeight = mean(`Weight (lbs)`))
      avg_Height <- position_data() %>%
        filter(!is.na(`Height (inches)`)) %>% 
        summarize(mHeight = mean(`Height (inches)`))
      paste("This positions' average weight was", round(avg_Weight, 2), " lbs and
            the average height was", round(avg_Height, 2), "in")
    })
    
    output$data_table <- renderTable({
      
    })
    

}

shinyApp(ui = ui, server = server)
