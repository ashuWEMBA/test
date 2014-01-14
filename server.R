# server.R

# http://www.rstudio.com/shiny/lessons/Lesson-8/

library(shiny)
library(quantmod)

source("helper.R")

shinyServer(function(input, output) {
  
    dataInput = reactive({
      if(input$get == 0) return (NULL)
      
      isolate({                
        myStock = getSymbols(input$symb, src = "yahoo", 
                   from = input$dates[1],
                   to = input$dates[2],
                   auto.assign = FALSE)      

        myStock = adjustOHLC(myStock, use.Adjusted=TRUE); 
      })
    })
    
    output$plot <- renderPlot({

      if(input$get == 0) return (NULL)
      
      #chartSeries(dataInput(), theme = chartTheme("white"), 
      #          type = "line", log.scale = input$log, TA = NULL)
      
      SMA50 = LastSMAClosePrice(dataInput(), 50)
      SMA200 = LastSMAClosePrice(dataInput(), 200)
      
      nameLine = paste(input$symb, 
                       "50:", as.character(SMA50), 
                       "200:", as.character(SMA200), sep=" ");
      
      chartSeries(dataInput(), name = nameLine,
                  log.scale = input$log, theme=chartTheme("white"))
      plot(addSMA(n=50, col="red", with.col = Cl, on = 1))
      plot(addSMA(n=200, col="green", with.col = Cl, on = 1))
      
  })
  
})

