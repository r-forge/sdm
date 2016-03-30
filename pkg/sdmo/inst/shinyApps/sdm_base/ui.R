shinyUI(
  fluidPage(theme=shinytheme("Flatly"),
    
  headerPanel(img(src="r_terminal_sdm.png", height = 300, width = 900)),
  #br(),
  
#    HTML ('<div style="background-color:gray; color:white; margin:10px; padding:10px;">
#             <h2>SDM....</h2>
#             <p>
#              A framework to model and simulate species distributions
#             </p>
#           </div>'
#            ),
#     

  
  sidebarPanel(
    bsCollapsePanel("Data Settings",
      radioButtons('format','Input Train Data Format',c('CSV','SHP')),
      selectizeInput("train","Select Train Dataset:","NULL"),
      hr(),
      selectizeInput("predictors","Select Raster Predictors file","NULL"),
      br(),
      br()
      
      ),
    shinyFilesButton('files', label='File select', title='Please select a file', multiple=FALSE),
    bsCollapsePanel("Models Settings",
      checkboxGroupInput("methods","Methods",c("GLM","GAM","BRT","RF","SVM","MARS","NNet","Bioclim","Domin","Mahalanobis","Maxent","Maxlike","Ensemble"),inline=T, selected="GLM"),
      hr(),
      checkboxGroupInput("repMethod","Replication Methods",c("Subsampling","Bootsraping","Cross-Validation"),selected="Subsampling"),
      numericInput("replicates","Replicates",1,min=1),
      numericInput("cv.folds","CV.Folds",5,min=1),
      sliderInput('test.percent','Test persentage',0,100,30,1)
      ),
    bsToggleButton("run","Run!")
    
    
    ),
  mainPanel(
    h3(textOutput("caption")),
    tabsetPanel(position='above',
                tabPanel("Plot",plotOutput('mpgPlot')),
                tabPanel("Summary", verbatimTextOutput("summary")),
                tabPanel("Map", leafletOutput("mymap"),
                         p(),
                         actionButton("recalc", "New points"))
                )
    )
  )
  )

# shinyUI(
#   fluidPage(
#     titlePanel('SDM....!'),
#     hr(),
#     fluidRow(
#       column(3,offset=1,
#              wellPanel(
#                 sliderInput('interaction.depth','Interaction.depth:',min=1,max=10,value=1),
#                 br(),
#                 checkboxInput('boot', 'Bootstrap'),
#                 checkboxInput('cv', 'Cross-validation')
#                 ),
#              wellPanel(
#                selectInput("methods","methods",list('GLM'='glm','GAM'='gam','randomForest'='rf','BRT'='brt','SVM'='svm'))
#                )
#         ),
#       column(8,
#              tabsetPanel(position='below',
#                tabPanel("Plot",plotOutput('mpgPlot')),
#                tabPanel("Summary")
#                )
#           
#         )
#       ),
#     fluidRow(
#         column(12,
#             sliderInput('test','TEST',min=0,max=100,value=10)
#           )
#       ),
#     fluidRow(
#         column(12,
#                navbarPage(
#                  "SDM package",
#                  #tabPanel("Plot",plotOutput('mpgPlot')),
#                  tabPanel("Summary"),
#                  tabPanel("SS"),
#                  tabPanel("WW"),
#                  navbarMenu("Menue1",
#                             tabPanel("p1"),
#                             tabPanel("P2")
#                             )
#                )
#                )
#       )
#     )
#   )
# 
