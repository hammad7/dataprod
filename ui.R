shinyUI(
  pageWithSidebar(
    headerPanel("Singular Value Decomposition"),
    sidebarPanel(
      fileInput('file','Upload a JPEG Image',accept = 'image/jpeg'),
      h5('OR'),
      selectInput("imagename", "Select an image:", list.files(path="./Data",pattern="\\.jpg"),selected='Flower.jpg'),
      #'JPG$'
      uiOutput('dynamicslider'),#####
      h4('Variance retained'),
       verbatimTextOutput("var"),
      #hr(),
      h4('Image Size in memory'),
      verbatimTextOutput('sizem'),
      h4('Image Dimensions'),
      verbatimTextOutput("dim"),
      h4('Image Size'),
      verbatimTextOutput('size')
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Image',
                 h4('Image after reconstruction'),
                 fluidRow(
                   column(2,
                        uiOutput("finimg")
                        ),
                   column(2,offset = 6,
                          downloadButton('downloadData', 'Save As')
                        )                 
                 ),
                 h4('Original Image'),
                 imageOutput("origimg")
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
                 h3("Source Code (server.R and ui.R):",br(), a(" Github", href="https://github.com/hammad7/dataprod")))
      )
    ) 
  )
)
