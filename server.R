#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(rlang)
library(reshape2)
library(withr)
library(plotly)
library(usmap)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    
    output$ndc <- reactive({
        input$state!="District of Columbia"
    })
    outputOptions(output, 'ndc', suspendWhenHidden = FALSE)
    
    output$USPlot3 <- renderPlotly({
        
        
        usdf = filter(popdata, areaname == "United States")
        
        df1 <- data.frame("year1"=strtoi(usdf[[(input$dates[1]-1970)+11]]), "year2"=strtoi(usdf[[(input$dates[2]-1970)+11]]), "US"=usdf$county_name)
        df2 <- melt(df1, id.vars='US')
        
        p = ggplot(df2, aes(x=US, y=value, fill=variable)) +
            geom_bar(stat='identity', position='dodge') + ylab("Population") + ggtitle(paste("United States", input$dates[1], "-", input$dates[2])) 
        ggplotly(p)
    })
    output$StatePlot1 <- renderPlotly({
        
        
        statedf = filter(popdata, state_name == input$state & county_name == input$state)
        
        df1 <- data.frame("year1"=strtoi(statedf[[(input$dates[1]-1970)+11]]), "year2"=strtoi(statedf[[(input$dates[2]-1970)+11]]), "State"=statedf$county_name)
        df2 <- melt(df1, id.vars='State')
        
        p = ggplot(df2, aes(x=State, y=value, fill=variable)) +
            geom_bar(stat='identity', position='dodge') + ggtitle(paste(input$state, input$dates[1], "-", input$dates[2])) + ylab("Population")
        ggplotly(p)
    })
    
    output$popPlot1 <- renderPlotly({

        df = filter(popdata, state_name == input$state & county_name != input$state)
        
#        x <- df[,6]
 #       barplot(x, )
        
        p = ggplot(data=df, aes(x=county_name, y=strtoi(df[[(input$dates[1]-1970)+11]]))) + geom_bar(stat="identity") + ggtitle(paste("Population of", input$state, input$dates[1])) + xlab("Counties") + ylab("Population")    
        # generate bins based on input$bins from ui.R
        #x    <- faithful[, 2]
        #bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
 #       hist(x, breaks = bins, col = 'darkgray', border = 'white')
        ggplotly(p)
    })
    output$popPlot2 <- renderPlotly({

        df = filter(popdata, state_name == input$state & county_name != input$state)
        
        p = ggplot(data=df, aes(x=county_name, y=strtoi(df[[(input$dates[2]-1970)+11]]))) + geom_bar(stat="identity") + xlab("Counties") + ylab("Population") + ggtitle(paste("Population of", input$state, input$dates[2]))     
        ggplotly(p)
    })
    output$popPlot3 <- renderPlotly({
        
        
        df = filter(popdata, state_name == input$state & county_name != input$state)
        
        df1 <- data.frame("year1"=strtoi(df[[(input$dates[1]-1970)+11]]), "year2"=strtoi(df[[(input$dates[2]-1970)+11]]), "County"=df$county_name)
        df2 <- melt(df1, id.vars='County')

        p = ggplot(df2, aes(x=County, y=value, fill=variable)) +
            geom_bar(stat='identity', position='dodge') + xlab("Counties") + ylab("Population")+ggtitle(paste("Population Comparison of", input$state, input$dates[1], "-", input$dates[2]))
        ggplotly(p)
    })
    output$popPlot4 <- renderPlotly({
        
        
        df = filter(popdata, state_name == input$state | areaname == "United States")
        df2 = data.frame("Rate"=(df[[(input$dates[2]-1970)+11]]-df[[(input$dates[1]-1970)+11]])/df[[(input$dates[1]-1970)+11]]*100, "Region"=df$county_name)
        df2 = mutate(df2, Highlight = ifelse(Region == "United States", "US", ifelse(Region==input$state, "STATE", "COUNTY")))
        
        p = ggplot(data=df2, aes(x=reorder(Region, -Rate), y=Rate, fill=Highlight)) + geom_bar(stat="identity") + xlab("Counties") + ggtitle(paste("Rate of Population change in", input$state, "from", input$dates[1], "-", input$dates[2], "in percent")) + scale_fill_manual( values = c( "US"="red", "STATE"="blue", "COUNTY"="gray" ) )
        ggplotly(p)
})
    output$popPlot5 <- renderPlotly({

        df = filter(popdata, state_name == input$state | areaname == "United States")
        df2 = data.frame("Rate"=(df[[(input$dates[2]-1970)+11]]-df[[(input$dates[1]-1970)+11]])/df[[(input$dates[1]-1970)+11]]*100, "Region"=df$county_name, "fips"=sprintf("%05d", df$fips))
        df2 = mutate(df2, Highlight = ifelse(Region == "United States", "US", ifelse(Region==input$state, "STATE", "COUNTY")))
        
        df2=filter(df2, Region != input$state & Region != "United States")
#        usmap::plot_usmap(data = df2, values="Rate", include=df2$fips) + scale_fill_continuous(low="red", high="green")
        p = usmap::plot_usmap("counties", data = df2, values="Rate", include=df2$fips, labels=TRUE) + scale_fill_gradient2(low="red", mid="white", high="green", name="Rate of Population Change (%)")+ ggtitle(paste("Rate of Population change in", input$state, "from", input$dates[1], "-", input$dates[2], "in percent"))
        p$layers[[2]]$aes_params$size <- 2
        ggplotly(p)
    })
    output$popPlot6 <- renderPlotly({
        
        df = filter(popdata, state_name == input$state | areaname == "United States")
        df2 = data.frame("Rate"=(df[[(input$dates[2]-1970)+11]]-df[[(input$dates[1]-1970)+11]])/df[[(input$dates[1]-1970)+11]]*100, "Region"=df$county_name, "fips"=sprintf("%05d", df$fips))
        df2 = mutate(df2, Highlight = ifelse(Region == "United States", "US", ifelse(Region==input$state, "STATE", "COUNTY")))
        df2 = mutate(df2, Rate = Rate-df2$Rate[1])
        
        df2=filter(df2, Region != input$state & Region != "United States")
        #        usmap::plot_usmap(data = df2, values="Rate", include=df2$fips) + scale_fill_continuous(low="red", high="green")
        p = usmap::plot_usmap("counties", data = df2, values="Rate", include=df2$fips, labels=TRUE) + scale_fill_gradient2(low="red", mid="white", high="green", name="Rate of Population Change (%)")+ggtitle(paste("Rate of Population change in", input$state, "relative to the US from", input$dates[1], "-", input$dates[2], "in percent"))
        p$layers[[2]]$aes_params$size <- 2
        ggplotly(p)
    })
    output$USPlot4 <- renderPlotly({
        
        df = filter(popdata, county_fips == 0 & areaname != "United States")
        df2 = data.frame("Rate"=(df[[(input$dates[2]-1970)+11]]-df[[(input$dates[1]-1970)+11]])/df[[(input$dates[1]-1970)+11]]*100, "state"=df$state_name)
#        df2 = mutate(df2, Rate = Rate-df2$Rate[1])
        
#        df2=filter(df2, Region != input$state & Region != "United States")
        #        usmap::plot_usmap(data = df2, values="Rate", include=df2$fips) + scale_fill_continuous(low="red", high="green")
        p = usmap::plot_usmap(data = df2, values="Rate", labels=TRUE) + scale_fill_gradient2(low="red", mid="white", high="green", name="Rate of Population Change (%)")+ggtitle(paste("Rate of Population change in the US by state from", input$dates[1], "-", input$dates[2], "in percent"))
        p$layers[[2]]$aes_params$size <- 2
        ggplotly(p)
    })
    output$USPlot5 <- renderPlotly({
        
        df = filter(popdata, county_fips == 0 | areaname == "United States")
        df2 = data.frame("Rate"=(df[[(input$dates[2]-1970)+11]]-df[[(input$dates[1]-1970)+11]])/df[[(input$dates[1]-1970)+11]]*100, "state"=df$state_name)
        df2 = mutate(df2, Rate2 = Rate-df2$Rate[1])
        
        df2=filter(df2, state != "")
        #        usmap::plot_usmap(data = df2, values="Rate", include=df2$fips) + scale_fill_continuous(low="red", high="green")
        p = usmap::plot_usmap(data = df2, values="Rate2", labels=TRUE) + scale_fill_gradient2(low="red", mid="white", high="green", name="Rate of Population Change (%)")+ggtitle(paste("Rate of Population change in the US by state relative to the US average from", input$dates[1], "-", input$dates[2], "in percent"))
        p$layers[[2]]$aes_params$size <- 2
        ggplotly(p)
    })
    
    
})
