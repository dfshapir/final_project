library(tidyverse)
library(shiny)
library(shinythemes)

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
                               sliderInput("slider", "Choose a year",
                                           value = c(1992, 2010), min = 1992, max = 2010),
                               selectInput("region", "Region",
                                           choices = levels(birthdat$name),
                                           multiple = FALSE),
                            ),
                            mainPanel(
                                plotOutput("plot")
                            )
                        )),
               tabPanel("Mortality Rate",
                        h5("Text will go here.")),
               tabPanel("Migration",
                        h5("Text will go here.")))
)


server <- function(input, output) {
    output$plot <- renderPlot({
        data <- subset(birthdat,
                       name %in% input$region)
        
        p <- ggplot(rudata, aes(x = year, y = birth)) + 
            geom_jitter() + 
            ggtitle(input$title)
        
        if(input$fit == TRUE) {
            p <- p + geom_smooth(method = "lm")
        }
    p
    })
}


shinyApp(ui = ui, server = server)
