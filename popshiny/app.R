library(tidyverse)
library(shiny)
library(shinythemes)
library(plotly)
library(sf)

birthdat <- read_rds("birthdata.rds")
mortdat <- read_rds("mortdata.rds")
migrdat <- read_rds("migrdata.rds")
shapedat <- read_rds("Shape.rds")
oldshape <- read_rds("oldshape.rds")

# Run the final_project.Rmd before running the Shiny; that's where I wrote the rds's that are above. I
# don't think it'll work otherwise.

ui <- fluidPage(theme = shinytheme("slate"),
                
# Above: set the color scheme, below: beginning the navbar.
    
    navbarPage("Russian Regional Demographic Change",
               tabPanel("About",
                        h2("Overview"),
                        br(),
                        
# I put breaks between all of my paragraphs to avoid overly dense text.
                        
                        p("Population decline in Eastern Europe is a well-known phenomenon. Bulgaria, for example, has lost two million people from its peak of nine million in around 1990; Ukraine’s population, meanwhile, has declined by an average of 300,000 people per year since the fall of the Soviet Union. Russia has also been implicated in this “demographic crisis:” in early 2019, the U.N. Commission on Population and Development concluded that Russia was projected to lose over 10 million people by 2050, shrinking by about 7 percent from 145.9 million in 2019 to 135.8 million in 2050, according to the U.N.’s World Population Prospects."),
                        br(),
                        p("However, while it is tempting to lump Russia in with the rest of Eastern Europe in terms of demographic trends, Russia is its own unique case. For one, Russia is the world’s second-most popular destination for migrants, most of whom come from Central Asia or the Caucasus. Also, Russia has not had nearly the same issues with emigration as have other Eastern European countries — although the countries has seen problems resulting from broadly low birth rates in the 1990s and mortality rates that differ wildly from region to region."),
                        br(),
                        p("These data and graphs explore Russia’s demographic trends on a regional level. Russian regional differences are fascinating: comparing, for example, the mortality rates of Chechnya and Tver Oblast, we see vast differentiation. Many of Russia’s minority-ethnic “republics” have had birth rates that are significantly higher than ethnically-Russian-dominated regions. Foreign emigration from Sakha/Yakutia was much higher than immigration in the 1990s — whereas Penza Oblast is just the opposite. The data here range only from 1992-2010; however, it is through these data that we can uncover the origins of today's demographic crisis."),
                        br(),
                        p("There are several tabs with interactive data: if you wish to learn about regional aspects of the Russian demographic crisis, feel free to look around. The data runs until 2010. However, one can nonetheless gain a broader understanding of Russia’s demographic problem: demographic issues compound generationally, meaning that effects we see from these data are quite relevant for present and future data for Russian policymakers and international observers."),
                        br(),
                        p('For more information on depopulation, check out ', 
                          a(href = 'https://russiamatters.org/blog/russian-population-decline-spotlight-again', 'this link'), 'which also links to a lot of other interesting sources on the subject.')
                        
# I had to look up how to make a hyperlink. Originally, using the above code, I tried to put the hyperlink
# at the end. But for whatever reason these calls automatically put a space after the hyperlink. So I
# just extended the sentence, instead of following the lengthy corrective course that Stack Overflow
# suggested.

),

# I put breaks in between all of my paragraphs to make things look nice.
                        
               tabPanel("Average Natural Growth",
                        h2("Average Natural Growth"),
                        br(),
                        p("In 1991, the Soviet Union (USSR) fell and splintered into fifteen successor states. While many in the West see the fall of the Soviet Union as long-awaited deliverance for the people for the former USSR from the yoke of totalitarian communism, the truth is much more nuanced. While the 1990s did bring citizens of some post-Soviet states a more open political and economic system, citizens also had to entirely reconstruct their society, their economy, and their political system. The economic situation was especially difficult. In the Soviet Union, constituent republics were kept under tight control from Moscow and were heavily centralized. When the USSR fell, formerly domestic supply chains instantly became spread across a multitude of different countries with different political systems, geopolitical goals, and national consciousnesses. Think of it this way: imagine if suddenly, Texas, Florida, Georgia, and the Carolinas became independent countries tomorrow. Everyone’s economy would take a huge hit – contracts would have to be reworked, trade relations would have to be established, and intrastate economies would have to be constructed that are not based on the center. For Russia, the fall of the Soviet Union was an immediate economic disaster, as shown by this World Bank graph (not my creation):"),
                        br(),
                        imageOutput("myImage"),
                        p("What does this all have to do with demographics? For one, Russian birthrates cratered. Young couples became less certain about their futures, and many thought twice before having kids or simply did not have them at all. In many cases, wages were not even getting paid, making it difficult to support a child even in the short run. Mortality rates also broadly rose throughout the 1990s: although there was a fair amount of variation, alcoholism rates went up, life expectancy fell, and standards of living became broadly worse. Below are graphs of the birthrate and mortality rate in 1995, the heart of the 1990s:"),
                        br(),
                        h4("Fig. 1"),
                        imageOutput("plotoldshape"),
                        
# As an overall rule in this project, I use plotly. I'd like for people to be able to interact with
# my maps, and it's very little extra effort on my part. The only issue is that it slows down the 
# "Run App" very significantly. Thus, I saved my ggplots as images for this section instead -- otherwise,
# nothing would pull up. 

                        br(),
                        h4("Fig. 2"),
                        imageOutput("plotoldshap"),
                        p("Data by 2010 looks a lot different. Economic prosperity, political stability and the government’s 2007 «Материнский капитал» (Maternity Capital) law helped an already rising birth rate. This is reflected by the fact that the percentages in the legend on this map are significantly higher than the percentages in the first birth rate map."),
                        br(),
                        h4("Fig. 3"),
                        imageOutput("plotshape"),
                        p("Mortality rates, meanwhile, have stayed relatively level:"),
                        h4("Fig. 4"),
                        imageOutput("plotshap"),
                        p("As is clear, the general trend for Russian natural population growth in the 1990s-2000s can be summed up thusly: birthrates plummeted at the same time as socioeconomic conditions got significantly worse across the country. However, as a result of improved economic conditions, political stability and government programs encouraging large families, birthrates rose prodigiously throughout the 2000s to balance out only slightly growing mortality rates. In today’s Russia, however, we see the aftershock of the demographic crisis of the 1990s, as less kids born in the 1990s = less women of childbearing age in the present day. The following tabs offer interactive graphs on regional aspects of natural growth factors.")),
              
                 tabPanel("Birthrate",
                        h4("This section shows trends in birthrates by Russian region over time. Use the sidebar tools to narrow down regions of interest. Multiple regions can be selected at once, should you wish to compare."),
                        h5("Some variation can be observed in birthrate. As a broad trend, birthrates tend to go slightly down over the 1990s, corresponding with abysmal economic conditions and uncertainty for the future. But they then experience a steady rise across the Putin presidency and the economic success of the early 2000s, leading to a slight mean increase over time. Rates also vary significantly amongst themselves: in North Caucasian republics such as Chechnya, for example, birthrates are significantly higher than in other parts of the country. Today, however, we see the aftershock of the 1990s problem: kids born in the 1990s are now at child-bearing age. Since there are less of them than in other generations, births have plateaued/started coming down in many areas."),

# This code is very similar to that of the next panel, mortality rate. I've put comments for the
# commands that I'm calling here in the next panel.

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
                        h5("Mortality rates show a slight increase over time, with some variation over region."),
                        sidebarLayout(
                            sidebarPanel(
                                textInput("titl", "Title", "Regional Mortality Rate by Year, 1992-2010"),
                                
# Add title (above), add checkbox for line of best fit (below)
                                
                                checkboxInput("fi", "Add Line of Best Fit?", value = FALSE),

# Add the option to toggle between regions, multiple = TRUE gives the option to select multiple regions

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
                                
# Note that I call all of my inputs slightly different things -- adding a letter, subtracting a letter
# here and there. I want to differentiate between different parts of my code so that things don't
# get mixed up. 
                            )
                        )),
               tabPanel("Arrivals and Departures",
                        h4("This section tracks foreign migration to and from Russian regions by year. Data starts at 1997."),
                        h5("Despite the stereotype that Eastern Europe is hemorrhaging people, Russia is actually a quite popular immigrant destination -- by some data, the second most popular in the world. This is a vestige of the Soviet era, in which citizens of other Soviet republics would come into Russia, the economic center of the Union, in search of work. While the Soviet Union has fallen, these corridors have not, and many migrants from Central Asia and the Caucasus flock to Russia's regions for better pay. This interactive data shows foreign arrivals versus foreign departures over a 15-year period by region."),
                        sidebarLayout(
                            sidebarPanel(
                                textInput("tit", "Title", "Regional Arrivals and Departures, 1997-2010"),
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
    output$myImage <- renderImage({

#  I had to put output$image here so it would match with my imageOutput that I had put in my input.
        
        list(src = "worldbank.png",
             contentType = 'image/png',
             width = 600,
             length = 350,
             style = "display: block; margin-left: auto; margin-right: auto;")
        
# Set the width and length to reasonable numbers so that the file wouldn't take up the whole page. Also
# centered the image.
        
    }, deleteFile = FALSE)
    
# Didn't want to delete the file right after I created it.
    
    output$plotoldshape <- renderImage({
        
        list(src = "l.png",
             contentType = 'image/png',
             width = 800,

# Since the height/weight ratio has been set in my Rmd, I only have to mess with the width here. 

             style = "display: block; margin-left: auto; margin-right: auto;")
        
    }, deleteFile = FALSE)
    
    output$plotoldshap <- renderImage({
        
        list(src = "m.png",
             contentType = 'image/png',
             width = 800,
             style = "display: block; margin-left: auto; margin-right: auto;")

    }, deleteFile = FALSE)
    
    output$plotshape <- renderImage({
        
        list(src = "n.png",
             contentType = 'image/png',
             width = 800,
             style = "display: block; margin-left: auto; margin-right: auto;")

    }, deleteFile = FALSE)
    
    output$plotshap <- renderImage({
        
        list(src = "o.png",
             contentType = 'image/png',
             width = 800,
             style = "display: block; margin-left: auto; margin-right: auto;")
    }, deleteFile = FALSE)
        
    output$plotly <- renderPlotly({
        
# Have to put everything with plotly right here instead of plot. I prefer plotly because it's basically
# the same as ggplot but there are more options for the reader with minimal additional code.

        data <- subset(birthdat,
                       name %in% input$region)
        
# Subsetting the data here ensures that the reader can actually toggle between regions. 
        
        p <- ggplot(data, aes(x = year, y = birth, color = name)) + 
            geom_point() + 
            ggtitle(input$title)
        
        ggplotly(p)
        
        if(input$fit == TRUE) {
            p <- p + geom_smooth(method = "lm")}
        
# The above if/then function works with the checkbox: if the box is checked, a regression line is run
# on the plot. Below, the single "p" (or another name, as you'll see below) prints the plot.
        
            p
        })
    output$plotl <- renderPlotly({
        dat <- subset(mortdat,
                       name %in% input$regio)
        
# Note that my subset looks a bit different -- I'm subsetting a different data set and including a 
# different input. This happens for the next graph as well.
        
        q <- ggplot(dat, aes(x = year, y = mort, color = name)) + 
            geom_point() + 
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
            ggtitle(input$tit) +
            scale_fill_discrete(name = "Legend")
        
# Geom_col() is a better graph for this sort of data than a line graph, because I'm trying to compare
# two separate variables -- namely, international arrivals and departures. In my opinion, it looks 
# better and is easier for the viewer to interpret.

        ggplotly(r)
        
        if(input$f == TRUE) {
            r <- r + geom_smooth(method = "lm")}
        r
    })
    }


shinyApp(ui = ui, server = server)
