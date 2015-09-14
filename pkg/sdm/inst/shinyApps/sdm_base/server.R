#setwd('/Users/bnaimi/Documents/shiny_test/')
library(shiny)
library(shinyBS)
library(shinyFiles)
library(shinythemes)
library(maptools)
library(leaflet)
#df <- read.csv('1sp_2continous.csv')
#d <- sdmData(df)
# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  observe({
    if (input$format == 'CSV') {
      updateSelectizeInput(session,"train",choices=list.files(pattern='.csv$'))
    } else if (input$format == 'SHP') {
      updateSelectizeInput(session,"train",choices=list.files(pattern='.shp$'))
    }
    updateSelectizeInput(session,"predictors",choices=list.files(pattern='grd$'),selected='')
    #if (input$run != 0) updateButton(session,'run',value=0)
   
  })
  train <- reactive({
#     if (is.null(input$train)) retrun(NULL)
#     else {
#       ext <- strsplit(input$train,'[.]')[[1]]
#       ext <- ext[length(ext)]
#       if (ext == 'csv') {
#         return(read.csv(input$train))
#       } else if (ext == 'shp') {
#         return(readShapeSpatial(input$train))
#       } else return (NULL)
#     }
    if (input$format == 'CSV') {
      return(read.csv(input$train))
    } else if (input$format == 'SHP') {
      return(readShapeSpatial(input$train))
    }
    
  })
  preds <- reactive({
    if (input$predictors != '') {
      return(brick(input$predictors))
    } else return (NULL)
  })
  
  method <- reactive({
    paste(input$methods,collapse=', ')
    })
  shinyFileChoose(input, 'files', session=session,roots=c(wd='.'), filetypes=c('', '.txt'))
  ss <- reactive({
    if (!input$run) return (NULL)
    if (!is.null(train)) {
      if (is.null(preds())) d <- sdmData(train())
      else d <- sdmData(train=train(),predictors = preds())
    }
    f <- as.formula(paste(d@train@Occurrence@species.name,'~.'))
    sdm(f,d,methods = input$methods,test.perc=input$test.percent,replicate.method=input$repMethod,replicates=as.numeric(input$replicates),cv.folds=as.numeric(input$cv.folds))
    
    
  })
  output$caption <- renderText({
    if (!is.null(ss())) method()
  })
  
  output$summary <- renderPrint({
    if (!is.null(ss())) show(ss())
  })
  
  
  output$mpgPlot <- renderPlot({
    #s <- sdm(Occurrence~.,d,methods = input$methods,test.perc=input$test.percent,replicate.method=input$repMethod)
    #roc(s)
    if (!is.null(ss())) roc(ss())
  })
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      #addMarkers(data = points())
      addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
  })
  
})


#runApp('../shiny_test/', display.mode = "showcase")

#runApp('../shiny_test/')
