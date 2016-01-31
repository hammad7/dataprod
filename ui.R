shinyUI(
  pageWithSidebar(
    headerPanel("Singular Value Decomposition"),
    sidebarPanel(
      #submitButton('Submit')
      fileInput('file','Upload an Image',accept = 'image/jpeg'),
      selectInput("imagename", "Select an image:", list.files(path="./Data",pattern="\\.jpg")),
      uiOutput('ui'),#####
      h4('Variance retained'),
      verbatimTextOutput("var"),
      #hr(),
      h4('Image Size in memory'),
      verbatimTextOutput('sizem'),
      h4('Image Dimensions'),
      verbatimTextOutput("dim"),
      h4('Image Size'),
      verbatimTextOutput('size')
      #sliderInput('num','vary',value = 200,min = 1 ,max = 200)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Image',
                 h4('Image after reconstruction'),
                 fluidRow(
                   column(2,
                        imageOutput("fin")
                        ),
                   column(2,offset = 6,
                          downloadButton('downloadData', 'Save As')
                        )                 
                 ),
                 h4('Original Image'),
                 imageOutput("imgi")
                 ),
        tabPanel('Variance',
                 h4('Variance explained'),
                 imageOutput("imgr"),
                 br(),
                 br(),
                 br(),
                 br(),
                 imageOutput("imgg"),
                 br(),
                 br(),
                 br(),
                 br(),
                 imageOutput("imgb")
                 ),
        tabPanel('Documentation',
                 h3('What does this App do'),
                 textOutput('help'),
                 h3('Source Code at Github : hammad7/dataprod'),
                 h4('server.R and ui.R'))
      )
#       #imageOutput("img"),
#       h4('Image after reconstruction'),
#       imageOutput("fin"),
#       h4('Original Image'),
#       imageOutput("imgi"),
#       h3('Variance explained'),
#       imageOutput("imgb")
    ) 
  )
)
