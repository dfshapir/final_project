library(tidyverse)
library(shiny)
library(shinythemes)
library(plotly)

birthdat <- read_rds("birthdata.rds")

ui <- fluidPage(theme = shinytheme("slate"),
    
    navbarPage("Specifics of Russian Population Decline",
               tabPanel("About",
                        h5("Text will go here.")),
               tabPanel("Broad Trends",
                        h5("Text will go here.")),
               tabPanel("Birthrate",
                        sidebarLayout(
                            sidebarPanel(
                                textInput("title", "Title", "Regional Birthrate by Year, 1992-2010"),
                                checkboxInput("fit", "Add Line of Best Fit?", value = FALSE),
                               selectInput("region", "Region",
                                           choices = levels(birthdat$name),
                                           multiple = TRUE,
                                           selected = "Republic of North Ossetia-Alania"),
                            ),
                            mainPanel(
                                plotlyOutput("plotly")
                            )
                        )),
               tabPanel("Mortality Rate",
                        h5("Text will go here.")),
               tabPanel("Migration",
                        h5("Text will go here.")))
)


server <- function(input, output) {
    output$plotly <- renderPlotly({
        data <- subset(birthdat,
                       name %in% input$region)
        
        p <- ggplot(birthdat, aes(x = year, y = birth, color = name)) + 
            geom_point(show.legend = FALSE) + 
            ggtitle(input$title)
        
        ggplotly(p)
        
        if(input$fit == TRUE) {
            p <- p + geom_smooth(method = "lm")
        }
    p
    })
}


shinyApp(ui = ui, server = server)
