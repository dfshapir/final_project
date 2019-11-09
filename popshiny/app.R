library(tidyverse)
library(shiny)
library(shinythemes)
library(plotly)

birthdat <- read_rds("birthdata.rds")
mortdat <- read_rds("mortdata.rds")

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
                                           selected = "Republic of Adygea                 ")
                            ),
                            mainPanel(
                                plotlyOutput("plotly")
                            )
                        )),
               tabPanel("Mortality Rate",
                        sidebarLayout(
                            sidebarPanel(
                                textInput("titl", "Title", "Regional Mortality Rate by Year, 1992-2010"),
                                checkboxInput("fi", "Add Line of Best Fit?", value = FALSE),
                                selectInput("regio", "Region",
                                            choices = levels(mortdat$name),
                                            multiple = TRUE,
                                            selected = "Republic of Adygea                 ")
                            ),
                            mainPanel(
                                plotlyOutput("plotl")
                            )
                        )),
               tabPanel("Migration",
                        h5("Text will go here.")))
)


server <- function(input, output) {
    output$plotly <- renderPlotly({
        data <- subset(birthdat,
                       name %in% input$region)
        
        p <- ggplot(data, aes(x = year, y = birth, color = name)) + 
            geom_point(show.legend = FALSE) + 
            ggtitle(input$title)
        
        ggplotly(p)
        
        if(input$fit == TRUE) {
            p <- p + geom_smooth(method = "lm")}
            p
        })
    output$plotl <- renderPlotly({
        dat <- subset(mortdat,
                       name %in% input$region)
        
        q <- ggplot(dat, aes(x = year, y = mort, color = name)) + 
            geom_point(show.legend = FALSE) + 
            ggtitle(input$titl)
        
        ggplotly(q)
        
        if(input$fi == TRUE) {
            q <- q + geom_smooth(method = "lm")}
        q
    })
    }


shinyApp(ui = ui, server = server)
