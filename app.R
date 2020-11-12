library(tidyverse)
library(shiny)
library(usmap)
library(ggplot2)


cTable <- read.csv("cTable.csv",header=TRUE)


# Define UI for application that draws the maps
ui <- navbarPage("My Map",
    tabPanel("Interactive Map",
  
      # Sidebar with a slider input for States 
      sidebarLayout(
          sidebarPanel(
              selectInput("state",
                          "Choose a State:",
                          c(unique(df$region.y)))
          ),
        
          # Show the maps
          mainPanel(
             plotOutput("map"),
             plotOutput("map2")
          )
      )
    ),
    tabPanel("Interactive Table",
       # Sidebar with a slider input for States
       fluidRow(
         wellPanel(
           selectInput("state2",
                       "Choose a State:",
                       c(unique(df$region.x)))
         ),
         
         # Show the resulting table
         
           tableOutput("table")
         )
       )
)


# Define server logic required to draw maps and tables
server <- function(input, output) {
    # reactive original data, States data and county data
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
    

    # draw Project Size map
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
    
    # draw ProjectAmount > 100 million map
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
    
    # reactive original data
    dat2 = reactive({
      selected_state = input$state2
      filtered <- cTable%>%filter(region.y == selected_state)
    })
    
    # draw the resulting table
    output$table <- renderTable({
      head(dat2(),n=20)
    }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
