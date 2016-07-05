# Author: Babak Naimi, naimi.b@gmail.com
# Date :  July 2016
# Version 2.2
# Licence GPL v3



.wellP <- function(...,style="background:#27242E;color:#fff") {
  tags$div(class='well',style=style,...)
}

if (!isGeneric("gui")) {
  setGeneric("gui", function(x,...)
    standardGeneric("gui"))
}


setMethod('gui', signature('sdmModels'), 
          function(x,...) {
            l <- c(.is.installed('shiny'),.is.installed('shinyBS'))
            if (!all(l)) stop(paste(paste(c('shiny','shinyBS')[l],collapse=', '),'are not installed; install them or use installAll() function to install all the functions that may be required by some functions in the package...'))
            
            if (!.sdmOptions$getOption('sdmLoaded')) .addMethods()
            
            .css <- readRDS(system.file("shinyApps/css.rds", package="sdm"))
            
            wt <- sapply(x@run.info[,7:9],function(x) any(x))
            wt <- names(wt)[wt]
            
            wtSel <- c()
            
            thStats <- names(getEvaluation(x,w=x@run.info[which(x@run.info[,7])[1],1],stat=2,wtest='training'))
            
            
            ui <- fluidPage(
              shiny::tags$head(.css),
              
              tabsetPanel(position='above',
                          tabPanel("Summary", .wellP(verbatimTextOutput("summary"))),
                          tabPanel("Model Runs Details", .wellP(tableOutput('run_info'))),
                          tabPanel("Evaluation",
                                   .wellP(
                                     navbarPage('Evaluation results',
                                                tabPanel('Plot',
                                                         fluidRow(
                                                           .wellP(
                                                             shinyBS::bsCollapse(
                                                               shinyBS::bsCollapsePanel("ROC-AUC",
                                                                               .wellP(
                                                                                 fluidRow(
                                                                                   column(3,selectInput("species", label = "Species Names",choices = as.character(unique(x@run.info$species)), selected = 1)),
                                                                                   column(3,selectizeInput("models","Modeling Methods",choices=unique(as.character(x@run.info$method)),multiple=TRUE)),
                                                                                   column(3,selectizeInput("replications","Replications",choices=as.character(unique(x@run.info$replication)),multiple=TRUE)),
                                                                                   column(3,selectizeInput("modelID", label = "Model IDs", choices = as.character(x@run.info$modelID), multiple = TRUE))
                                                                                   #column(3,selectizeInput("modelID", label = "Model IDs", choices = modelID(), multiple = TRUE))
                                                                                 ),
                                                                                 fluidRow(
                                                                                   column(6,checkboxGroupInput('wtest','Which Test Dataset?',choices=wt,selected = if(length(wt) > 2) wt[1:2] else wt)),
                                                                                   column(6,radioButtons('smooth','Should Be Smoothed?',choices=c('TRUE','FALSE'),selected ='TRUE'))
                                                                                 ),
                                                                                 fluidRow(.wellP(plotOutput('evalPlot1')))
                                                                               ),style='primary'
                                                               ),
                                                               shinyBS::bsCollapsePanel("Calibration",
                                                                               .wellP(
                                                                                 fluidRow(
                                                                                   column(3,selectInput("modelID2", label = "Model IDs", choices = as.character(x@run.info$modelID), selectize = TRUE)),
                                                                                   column(3,textInput("species_name", label = "Species Names")),
                                                                                   column(3,textInput("model_name", label = "Method")),
                                                                                   column(3,textInput("replication_name", label = "Replication"))
                                                                                 ),
                                                                                 fluidRow(
                                                                                   column(3,selectInput('wtest2','Which Test Dataset?',wt,selected=wt[length(wt)]))
                                                                                 ),
                                                                                 fluidRow(.wellP(plotOutput('evalPlot2')))
                                                                               ),style='primary'
                                                                               
                                                               ),
                                                               
                                                               shinyBS::bsCollapsePanel("Threshold Optimisations",
                                                                               .wellP(
                                                                                 fluidRow(
                                                                                   column(3,selectInput("modelID3", label = "Model IDs", choices = as.character(x@run.info$modelID), selectize = TRUE)),
                                                                                   column(3,textInput("species_name2", label = "Species Names")),
                                                                                   column(3,textInput("model_name2", label = "Method")),
                                                                                   column(3,textInput("replication_name2", label = "Replication"))
                                                                                 ),
                                                                                 fluidRow(
                                                                                   column(3,selectInput('wtest3','Which Test Dataset?',wt,selected=wt[length(wt)])),
                                                                                   column(6,selectInput('thStat','Select Statistics',thStats,selected='TSS'))
                                                                                 ),
                                                                                 fluidRow(.wellP(plotOutput('evalPlot3')))
                                                                               ),style='primary'
                                                                               
                                                                               
                                                               ),
                                                               shinyBS::bsCollapsePanel("Density",
                                                                               .wellP(
                                                                                 fluidRow(
                                                                                   column(3,selectInput("modelID4", label = "Model IDs", choices = as.character(x@run.info$modelID), selectize = TRUE)),
                                                                                   column(3,textInput("species_name3", label = "Species Names")),
                                                                                   column(3,textInput("model_name3", label = "Method")),
                                                                                   column(3,textInput("replication_name3", label = "Replication"))
                                                                                 ),
                                                                                 fluidRow(
                                                                                   column(3,selectInput('wtest4','Which Test Dataset?',wt,selected=wt[length(wt)]))
                                                                                 ),
                                                                                 fluidRow(.wellP(plotOutput('evalPlot4')))
                                                                               ),style='primary'
                                                               ),
                                                               shinyBS::bsCollapsePanel("Boxplot",
                                                                               .wellP(
                                                                                 fluidRow(
                                                                                   column(3,selectInput("modelID5", label = "Model IDs", choices = as.character(x@run.info$modelID), selectize = TRUE)),
                                                                                   column(3,textInput("species_name4", label = "Species Names")),
                                                                                   column(3,textInput("model_name4", label = "Method")),
                                                                                   column(3,textInput("replication_name4", label = "Replication"))
                                                                                 ),
                                                                                 fluidRow(
                                                                                   column(3,selectInput('wtest5','Which Test Dataset?',wt,selected=wt[length(wt)]))
                                                                                 ),
                                                                                 fluidRow(.wellP(plotOutput('evalPlot5')))
                                                                                 
                                                                               ),style='primary'
                                                               ), id='plotType'
                                                             )
                                                             
                                                           )
                                                         ),icon=icon('line-chart',"fa-2x")
                                                ),
                                                tabPanel('threshold-based',
                                                         .wellP(
                                                           fluidRow(
                                                             column(3,selectInput("modelID6", label = "Model IDs", choices = as.character(x@run.info$modelID), selectize = TRUE)),
                                                             column(3,textInput("species_name5", label = "Species Names")),
                                                             column(3,textInput("model_name5", label = "Method")),
                                                             column(3,textInput("replication_name5", label = "Replication"))
                                                           ),
                                                           fluidRow(
                                                             column(3,selectInput('wtest6','Which Test Dataset?',wt,selected=wt[length(wt)]))
                                                           )
                                                         ),
                                                         .wellP(tableOutput('thTable')),
                                                         icon=icon('arrow-circle-down',"fa-2x")
                                                ),
                                                tabPanel('threshold-independent',
                                                         .wellP(
                                                           fluidRow(
                                                             column(3,selectInput("modelID7", label = "Model IDs", choices = as.character(x@run.info$modelID), selectize = TRUE)),
                                                             column(3,textInput("species_name6", label = "Species Names")),
                                                             column(3,textInput("model_name6", label = "Method")),
                                                             column(3,textInput("replication_name6", label = "Replication"))
                                                           ),
                                                           fluidRow(
                                                             column(3,selectInput('wtest7','Which Test Dataset?',wt,selected=wt[length(wt)]))
                                                           )
                                                         ),
                                                         .wellP(tableOutput('thTable2')),icon=icon('arrows-h',"fa-2x")
                                                )
                                                
                                                ,id='Statistic'
                                                
                                     )
                                   ))
              )
            )
            
            #------------------------
            server <- function(input,output,session) {
              
              models <- reactive({
                if (!is.null(input$models))
                  unlist(lapply(unlist(strsplit(input$models,',')),.trim))
              })
              
              single_model_info <- reactive({
                w <- NULL
                if (!is.null(input$plotType)) {
                  if (input$plotType == 'Calibration') {
                    if (!is.null(input$modelID2)) w <- which(x@run.info$modelID == input$modelID2)
                  } else if (input$plotType == 'Threshold Optimisations') {
                    if (!is.null(input$modelID3)) w <- which(x@run.info$modelID == input$modelID3)
                  } else if (input$plotType == 'Density') {
                    if (!is.null(input$modelID4)) w <- which(x@run.info$modelID == input$modelID4)
                  } else if (input$plotType == 'Boxplot') {
                    if (!is.null(input$modelID5)) w <- which(x@run.info$modelID == input$modelID5)
                  } 
                } else if (input$Statistic == 'threshold-based') {
                  if (!is.null(input$modelID6)) w <- which(x@run.info$modelID == input$modelID6)
                } else if (input$Statistic == 'threshold-independent') {
                  if (!is.null(input$modelID7)) w <- which(x@run.info$modelID == input$modelID7)
                }
                
                
                if (!is.null(w)) c(species=as.character(x@run.info[w,"species"]), method=as.character(x@run.info[w,"method"]),replication=as.character(x@run.info[w,"replication"]))
                
              })
              
              species <- reactive({
                if (!is.null(input$species))
                  input$species
              })
              
              replic <- reactive({
                if (!is.null(input$replications))
                  input$replications
              })
              
              modelID <- reactive({
                getModelInfo(x,species=species(),method=models(),replication=replic())$modelID
              })
              
              observe({
                
                output$summary <- renderPrint({
                  show(x)
                })
                
                
                
                if (input$Statistic == 'Plot') {
                  
                  if (!is.null(input$plotType)) {
                    
                    if (input$plotType == 'ROC-AUC') {
                      
                      updateSelectizeInput(session,"modelID",choices=modelID())
                      
                      output$evalPlot1 <- renderPlot({
                        if (!is.null(input$modelID)) {
                          #cat('\n with modelID: ',input$modelID)
                          roc(x,p=input$modelID,wtest=input$wtest,smooth=as.logical(input$smooth))
                        } else {
                          #cat('\n without modelID: ',modelID())
                          roc(x,p=modelID(),wtest=input$wtest,smooth=as.logical(input$smooth))
                        } 
                      })
                      
                    } else if (input$plotType == 'Calibration') {
                      updateTextInput(session,'species_name',value=as.character(single_model_info()[1]))
                      updateTextInput(session,'model_name',value=as.character(single_model_info()[2]))
                      updateTextInput(session,'replication_name',value=as.character(single_model_info()[3]))
                      updateSelectInput(session,'modelID2',selected = input$modelID2)
                      
                      output$evalPlot2 <- renderPlot({
                        sinfo <- as.character(single_model_info())
                        if (length(sinfo) != 0) {
                          ev <- x@models[[sinfo[1]]][[sinfo[2]]][[as.character(input$modelID2)]]@evaluation[[as.character(input$wtest2)]]
                          plot(calibration(ev))
                        }
                        
                      })
                      
                    } else if (input$plotType == 'Threshold Optimisations') {
                      
                      
                      updateTextInput(session,'species_name2',value=as.character(single_model_info()[1]))
                      updateTextInput(session,'model_name2',value=as.character(single_model_info()[2]))
                      updateTextInput(session,'replication_name2',value=as.character(single_model_info()[3]))
                      updateSelectInput(session,'modelID3',selected = input$modelID3)
                      
                      
                      output$evalPlot3 <- renderPlot({
                        sinfo <- as.character(single_model_info())
                        if (length(sinfo) != 0) plot(x@models[[sinfo[1]]][[sinfo[2]]][[as.character(input$modelID3)]]@evaluation[[as.character(input$wtest3)]],as.character(input$thStat))
                      })
                      
                    } else if (input$plotType == 'Density') {
                      updateTextInput(session,'species_name3',value=as.character(single_model_info()[1]))
                      updateTextInput(session,'model_name3',value=as.character(single_model_info()[2]))
                      updateTextInput(session,'replication_name3',value=as.character(single_model_info()[3]))
                      updateSelectInput(session,'modelID4',selected = input$modelID4)
                      
                      output$evalPlot4 <- renderPlot({
                        sinfo <- as.character(single_model_info())
                        if (length(sinfo) != 0) {
                          ev <- x@models[[sinfo[1]]][[sinfo[2]]][[as.character(input$modelID4)]]@evaluation[[as.character(input$wtest4)]]
                          density(ev)
                        }
                        
                      })
                      
                    } else if (input$plotType == 'Boxplot') {
                      updateTextInput(session,'species_name4',value=as.character(single_model_info()[1]))
                      updateTextInput(session,'model_name4',value=as.character(single_model_info()[2]))
                      updateTextInput(session,'replication_name4',value=as.character(single_model_info()[3]))
                      updateSelectInput(session,'modelID5',selected = input$modelID5)
                      
                      
                      output$evalPlot5 <- renderPlot({
                        sinfo <- as.character(single_model_info())
                        if (length(sinfo) != 0) {
                          ev <- x@models[[sinfo[1]]][[sinfo[2]]][[as.character(input$modelID5)]]@evaluation[[as.character(input$wtest5)]]
                          boxplot(ev)
                        }
                        
                      })
                      
                    }
                  }
                  
                } else if (input$Statistic == 'threshold-based') {
                  updateTextInput(session,'species_name5',value=as.character(single_model_info()[1]))
                  updateTextInput(session,'model_name5',value=as.character(single_model_info()[2]))
                  updateTextInput(session,'replication_name5',value=as.character(single_model_info()[3]))
                  updateSelectInput(session,'modelID6',selected = input$modelID6)
                  
                  output$thTable <- renderTable({
                    getEvaluation(x,w=as.numeric(input$modelID6),stat=2,wtest=as.character(input$wtest6))
                  })
                  
                } else if (input$Statistic == 'threshold-independent') {
                  updateTextInput(session,'species_name6',value=as.character(single_model_info()[1]))
                  updateTextInput(session,'model_name6',value=as.character(single_model_info()[2]))
                  updateTextInput(session,'replication_name6',value=as.character(single_model_info()[3]))
                  updateSelectInput(session,'modelID7',selected = input$modelID7)
                  
                  s1 <- getEvaluation(x,w=as.numeric(input$modelID7),stat=1,wtest=as.character(input$wtest7))
                  sinfo <- as.character(single_model_info())
                  s2 <- NULL
                  if (length(sinfo) != 0) {
                    ev <- x@models[[sinfo[1]]][[sinfo[2]]][[as.character(input$modelID7)]]@evaluation[[as.character(input$wtest7)]]
                    s2 <- calibration(ev)@statistic
                  }
                  df <- data.frame(matrix(nrow=5,ncol=2))
                  colnames(df) <- c('Statistic','Value')
                  df[,1] <- c(names(s1),'Calibrartion')
                  df[,2] <- c(s1[[1]],s1[[2]],s1[[3]][1],s1[[4]],s2)
                  output$thTable2 <- renderTable({
                    df
                  })
                  
                }
                
              })
              
              observeEvent(input$wtest,{
                if (length(input$wtest) > 2) {
                  ad <- input$wtest[!input$wtest %in% wtSel]
                  wtSel <<- wtSel[-1]
                  wtSel <<- c(wtSel,ad)
                  updateCheckboxGroupInput(session,'wtest',selected=wtSel)
                  
                } else {
                  if (length(input$wtest) == 1) wtSel <<- c(wtSel,as.character(input$wtest))
                  else wtSel <<- c(wtSel,input$wtest[!input$wtest %in% wtSel])
                }
                
              })
              
              output$run_info <- renderTable({
                x@run.info
              })
            }
            
            shinyApp(ui=ui,server=server)
            
          }
)
