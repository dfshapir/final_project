library(tidyverse)
library(shiny)
library(shinythemes)
library(plotly)

birthdat <- read_rds("birthdata.rds")
mortdat <- read_rds("mortdata.rds")
migrdat <- read_rds("migrdata.rds")

# Run the final_project.Rmd before running the Shiny; that's where I wrote the rds's that are above.

ui <- fluidPage(theme = shinytheme("slate"),
    
    navbarPage("Specifics of Russian Population Decline",
               tabPanel("About",
                        h5("Text will go here.")),
               tabPanel("Broad Trends",
                        h5("Text will go here.")),
               tabPanel("Birthrate",
                        h4("This section shows trends in birthrates by Russian region over time. Use the sidebar tools to narrow down regions of interest. Multiple regions can be selected at once, should you wish to compare."),
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
                                
# I much prefer using plotly to ggplot. Datacamp showed me this secret. I watched the entire Shiny 
# Datacamp, and it was quite helpful. I would definitely recommend putting it as a requirement next year.
                            
                                )
                        )),
               tabPanel("Mortality Rate",
                        h4("This section shows trends in mortality rates by Russian region over time. Use the sidebar tools to narrow down regions of interest. Multiple regions can be selected at once, should you wish to compare."),
                        sidebarLayout(
                            sidebarPanel(
                                textInput("titl", "Title", "Regional Mortality Rate by Year, 1992-2010"),
                                checkboxInput("fi", "Add Line of Best Fit?", value = FALSE),
                                selectInput("regio", "Region",
                                            choices = levels(mortdat$name),
                                            multiple = TRUE,
                                            selected = "Republic of Adygea                 ")

# For a while, I couldn't get the "selected" to work. But then I realized it was because the data contained
# spaces that needed to be taken into account. I was able to paste the exact name in to get what I wanted
# to show up.
                            
                            ),
                            mainPanel(
                                plotlyOutput("plotl")
                            )
                        )),
               tabPanel("Arrivals and Departures",
                        h4("This section tracks foreign migration to and from Russian regions by year. Data starts at 1996."),
                        sidebarLayout(
                            sidebarPanel(
                                textInput("tit", "Title", "Regional Arrivals and Departures, 1995-2010"),
                                checkboxInput("f", "Add Line of Best Fit?", value = FALSE),
                                selectInput("regi", "Region",
                                            choices = levels(migrdat$name),
                                            multiple = FALSE,
                                            
# I had to put that multiple = FALSE here, because the graph is structured in such a way that no more 
# than one region can be viewed at a time.
                                            
                                            selected = "Republic of Adygea                 ")
                        ),
                        mainPanel(
                            plotlyOutput("plo")
                                  )
                        ))
))


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
                       name %in% input$regio)
        
        q <- ggplot(dat, aes(x = year, y = mort, color = name)) + 
            geom_point(show.legend = FALSE) + 
            ggtitle(input$titl)
        
        ggplotly(q)
        
        if(input$fi == TRUE) {
            q <- q + geom_smooth(method = "lm")}
        q

# For a while, I had difficulty even getting this chunk in, because I was trying to do it in the same 
# parentheses as the birth data. However, I messed with some parentheses and brackets and got it to work.
    })
    output$plo <- renderPlotly({
        da <- subset(migrdat,
                     name %in% input$regi)
        
        r <- ggplot(da, aes(x = year, y = value)) +
            geom_col(aes(fill = variable), position = "dodge") +
            ggtitle(input$tit)

        ggplotly(r)
        
        if(input$f == TRUE) {
            r <- r + geom_smooth(method = "lm")}
        r
    })
    }


shinyApp(ui = ui, server = server)
