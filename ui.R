#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel(h1("US Population Analysis by County", align = "center")),
    HTML('<center><img src="https://www.altadenalibrary.org/wp-content/uploads/2020/02/Document-768x523.png", width="352", height="240"></center>'),
    tags$br(),
    print("The study of population over time is a very important topic that can have regular meaning in our everyday lives. Beyond the fact that this year is a census year where we can get a more accurate count of our populations, county population can be very interesting and reveal a lot of patterns in our society today. Where are jobs moving towards? If I am buying a house, where are house prices rising? How have things changed since I've been to an area? These are all things that we can study by looking at population over counties over time."),
    tags$br(),
        br(),
    fluidRow(
        column(2,
            sliderInput("dates",
                        "Dates to compare:",
                        min = 1970,
                        max = 2019,
                        value = c(1970,2019),
                        sep=""),
        ),
        column(3,
               selectizeInput("state", "State:", choices = unique(popdata$state_name), selected = "Alabama")
               
               )
    ),

    conditionalPanel(
        condition = "output.ndc",
        plotlyOutput("popPlot5"),
        plotlyOutput("popPlot6"),
    ),
            plotlyOutput("popPlot4"),
            plotlyOutput("USPlot4"),
            plotlyOutput("USPlot5"),
    conditionalPanel(
        condition = "output.ndc",
        plotlyOutput("popPlot3"),
        plotlyOutput("popPlot1"),
        plotlyOutput("popPlot2"),
    ),
            plotlyOutput("StatePlot1"),
            plotlyOutput("USPlot3")
)
)
