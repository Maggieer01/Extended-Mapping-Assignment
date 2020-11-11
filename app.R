library(tidyverse)
library(shiny)
library(usmap)
library(ggplot2)


cTable <- read.csv("cTable.csv",header=TRUE)


# Define UI for application that draws a histogram
ui <- navbarPage("My Map",
    tabPanel("Interactive Map",
  
      # Sidebar with a slider input for number of bins 
      sidebarLayout(
          sidebarPanel(
              selectInput("state",
                          "Choose a State:",
                          c(unique(df$region.y)))
          ),
        
          # Show a plot of the generated distribution
          mainPanel(
             plotOutput("map"),
             plotOutput("map2")
          )
      )
    ),
    tabPanel("Interactive Table",
       # Sidebar with a slider input for number of bins 
       fluidRow(
         wellPanel(
           selectInput("state2",
                       "Choose a State:",
                       c(unique(df$region.x)))
         ),
         
         # Show a plot of the generated distribution
         
           tableOutput("table")
         )
       )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
    dat = reactive({
      selected_state = input$state
      filtered <- cTable%>%filter(region.y == selected_state)
    })
    
    main = reactive({
      MainStates <- map_data("state",input$state)
    })
    
    county = reactive({
      AllCounty <- map_data("county",input$state)
    })
    

  
    output$map <- renderPlot({
      ggplot() +
        geom_polygon(data=county(), aes(x=long, y=lat, group=group),
                     color="gray", fill="white", size = .1 ) +
        geom_polygon(data = dat(), aes(x = long, y = lat, group = group, 
                                        fill = `projectSize`),
                     color = "grey", size = 0.2, alpha = 1.6) +
        geom_polygon(data = main(), aes(x = long, y = lat, group = group),
                     color="black", fill="white", size = 0.2, alpha = 0.3) +
        scale_fill_manual(values = c("gold", "blue"))+ 
        ggtitle("Project Size") 
    })
    
    output$map2 <- renderPlot({
    ggplot() +
      geom_polygon(data=county(), aes(x=long, y=lat, group=group),
                   color="gray", fill="white", size = .1 ) +
      geom_polygon(data = dat(), aes(x = long, y = lat, group = group, 
                                      fill = `projectAmount...100000000`),
                   color = "grey", size = 0.2, alpha = 1.6) +
      geom_polygon(data = main(), aes(x = long, y = lat, group = group),
                   color="black", fill="white", size = 0.2, alpha = 0.3) +
      scale_fill_manual(values = c("darkblue", "red"))+ 
      ggtitle("County Based Total Project Amount greater than 100 million") +
      
      theme(plot.title = element_text(hjust = 0.5))
    })
    
    dat2 = reactive({
      selected_state = input$state2
      filtered <- cTable%>%filter(region.y == selected_state)
    })
    
    output$table <- renderTable({
      head(dat2(),n=20)
    }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
