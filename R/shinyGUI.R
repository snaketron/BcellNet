files <- NULL
# setwd("R") is done by shiny since the server file is in here
loadSource <- function(sourceName) {
  pattern <- paste("^", sourceName, "$", sep = "")
  print(pattern)
  files <<- list.files(pattern=pattern, recursive = TRUE)
  for (file in files) {
    source(file)
  }
}
loadSource("BuildIGraph.R")



usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dependencies = TRUE, repos="http://cran.us.r-project.org")
  require(p, character.only = TRUE)
}

usePackage("shiny")
usePackage("shinyjs")
#usePackage("shinyBS")


data <- NULL
selectFirstPatient <- NULL
selectSecondPatient <- NULL
graphFirst <- NULL
graphSecond <- NULL

#UI
ui <- fluidPage(
  # activate shinyjs which enables easy commands without JS knowledge
  shinyjs::useShinyjs(),
  theme = "bcell.css",
  
  # p(style = "font-family:Times New Roman","See other apps in the"),
  # a("Shiny Showcase",href = "http://www.rstudio.com/products/shiny/shiny-user-showcase/"),
  #html code with tags, you can also write h1("test")
  fluidRow(
    column(1, 
      tags$img(height=70, width=70, src="logo.png", style="margin-top: 20px; ")
    ),
    column(11, 
      tags$h1("Find differences in clonal selection", style = "color:#469CF1; font-family:Forte;"),
      tags$em("between healthy and HCV-infected individuals", style = "color:#428BCA;")
    )
  ),
  
  # tags$br(), line break 
  tags$hr(),
  
  # here we write *Input() & *Output() functions
  
  #Create *input functions
  
  #comboBoxes
  sidebarLayout(
    sidebarPanel (
      #select File
      fileInput('csvFile', 'Choose CSV File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      #ende fileInput
      
      #checkBOx
      #checkboxInput(inputId = "checkbox1", label = " select input File", value = FALSE),
      
      # selectInput(inputId = "combo1",label = "",
      #             list(`A` = c("Select B-cell subset", "a", "b"),`AB` = c("WA", "OR", "CA")),
      #             selected = NULL, multiple = FALSE, selectize = TRUE),
      
      #first patient
      disabled(selectInput(inputId = "comboFirstPatient",label = "Select 1st patient",
                  choices = NULL, selected = NULL, multiple = FALSE, selectize = TRUE)),
      
      #second patient
      disabled(selectInput(inputId = "comboSecondPatient", label = "Select 2nd patient",
                  choices = NULL, selected = NULL, multiple = FALSE, selectize = TRUE)),
      
      disabled(selectInput(inputId = "vjSegment",label = "VJ-Segment",
                  choices = "whole data",selected = "whole data", multiple = FALSE, selectize = TRUE)),
      
      selectInput(inputId = "partOfSequence",label = "Part of Sequence",
                  choices = c("whole sequence", "CDR3", "V sequence"),
                  selected = "whole sequence", multiple = FALSE, selectize = TRUE),
      
      tags$hr(),
      
      #numericInput
      div(style="display:inline-block;vertical-align:top; width: 150px;",
      numericInput( inputId = "num2",label = "Relative",value =0.9,min = 0,max = 1, step = 0.01)),
      div(style="display:inline-block;vertical-align:top; width: 150px;",textInput(inputId = "textBox", label = " Absolute", width = "50%")),

      #Slider
     # sliderInput(inputId = "num", label = "Egde definition", 
     #              value = 0.3, min = 0, max = 1, step= 0.1),

      
      # comboBox
      selectInput(inputId = "select_community",label = "Community Selection",
                  choices = names(all_communtiy_algorithms()),
                  selected = NULL, multiple = FALSE, selectize = TRUE),
      
      selectInput(inputId = "select_layout",label = "Layout Generator",
                  choices = names(all_layout_algorithms()),
                  selected = NULL, multiple = FALSE, selectize = TRUE),
      
      #Buttons
     div(style="display:inline-block;vertical-align:top; ",disabled(actionButton(inputId = "pn", label = "Plot Network", style="margin-top:10px;"))),
     div(style="display:inline-block;vertical-align:top; ",disabled(actionButton(inputId = "pdd", label = " Plot degree distribution", style="margin-top:10px;"))),
     div(style="display:inline-block;vertical-align:top; ", disabled(actionButton(inputId = "pcsd", label = " Plot community size distribution", style="margin-top:10px;")))
     # disabled(actionButton(inputId = "exportButton", label = " Export as...", style="margin-top:10px;")),
      
      
      # RadioButton
    #  radioButtons(inputId = "saveAs", label = "Download as type:", choices = list("PNG","PDF"), inline = TRUE),
    #  textInput(inputId = "downloadPlotFileName", label = h5("Enter file name for download")),
      
      # add Export as Button for Download
    # disabled(downloadButton(outputId = "down", label = "Download the plot")),

      #popUp windows Test
     #disabled(actionButton("go", "PopUpWindows", style="margin-left:10px;"))

    ), # End of sidebarLayout
    
    
    
    
    ###################def output function in mainPanel ###################################
    mainPanel(
      
      
      # You must build the object in the server function
      tabsetPanel(
        tabPanel("Network", 
                 tags$label(textOutput("firstPatientLabel"), 'for'="firstPatient", 'style'="margin-top: 5px;"),
                 visNetworkOutput("firstPatient"),
                 
                 tags$label(textOutput("secondPatientLabel"), 'for'="secondPatient"),
                 
                 visNetworkOutput("secondPatient")
                 #popupWindows
                 # bsModal("modalExample", "Your plot", "go", size = "large",visNetworkOutput("firstPatient"),downloadButton('downloadPlot', 'Download'))
        )
        ,

        tabPanel("Degree Distribution",
                 tags$label(textOutput("firstPatientDegreeDistributionLabel"), 'for'="firstPatientDegreeDistribution", 'style'="margin-top: 5px;"),
                 plotOutput("firstPatientDegreeDistribution"),

                 tags$label(textOutput("secondPatientDegreeDistributionLabel"), 'for'="secondPatientDegreeDistribution"),
                 plotOutput("secondPatientDegreeDistribution")
        )
        
        #   tabPanel("tab3")
        
        # plotOutput("firstPatient"),
        # plotOutput("secondPatient"),
        #fluidRow(column(7,plotOutput("firstPatient"), 
        # plotOutput("secondPatient")),offset=5),
      ),
      
      #show messages
      tags$head(tags$script(src = "message-handler.js"))
      
      # link for BcellNet in GitHub just for test href :)
      #  tags$p(tags$a(href="https://github.com/snaketron/BcellNet","GitHub-BcellNet"))
      
    )#end of mainPanel
    
  ) #end of sidebarLayout
  
)  #end of UI


#####################server side####################################

#' @import shiny
#' @importFrom shinyjs enable
server <- function(input,output, session){
  
  
  observe({
    if(is.null(input$csvFile$datapath)) return(NULL)
    
    
    data <<- csvToSubset(input$csvFile$datapath)
    possiblePatients <- names(data)
    possibleVjSegments <- NULL
    
    # Can use character(0) to remove all choices
    if (is.null(possiblePatients))
      possiblePatients <- character(0)
    
    ##########   update content of patient combobox #############
    # Can also set the label and select items
    updateSelectInput(session, "comboFirstPatient",
                      choices = possiblePatients,
                      selected = head(possiblePatients, 1)
    )
    updateSelectInput(session, "comboSecondPatient",
                      choices = possiblePatients,
                      selected = tail(possiblePatients, 1)
    )
   
    
    
    selectFirstPatient <<- head(possiblePatients, 1)
    selectSecondPatient <<- tail(possiblePatients, 1)
    
    
    
    # update combobox with vj segment entries
    updateVJSegment()
    
    
    # enable buttons if csv file is loaded
    shinyjs::enable("pn")
    shinyjs::enable("pdd")
    shinyjs::enable("pcsd")
   # shinyjs::enable("down")
    shinyjs::enable("comboFirstPatient")
    shinyjs::enable("comboSecondPatient")
    shinyjs::enable("vjSegment")
    #shinyjs::enable("go")
    
  })
  
  
  #save selected patient into global var
  observeEvent(input$comboFirstPatient, {
    selectFirstPatient <<- input$comboFirstPatient
    updateVJSegment()
  })
  observeEvent(input$comboSecondPatient, {
    selectSecondPatient <<- input$comboSecondPatient
    updateVJSegment()
  })

  #####################Update Inputnumeric#######################
   observeEvent(input$num2,{
     #x<-is.null(input$num2)
     if(!is.numeric(input$num2)){
       
       updateNumericInput(session,"num2", min=0, max = 1, step = 0.01)
       
     }else if(input$num2>1){

               updateNumericInput(session,"num2",value = 0.9, min=0, max = 1, step = 0.01)

     }
     
     
     
   })

  
  
  
  #plot networt button action
  observeEvent(input$pn, {
    
    if(is.null(data)) session$sendCustomMessage(type = 'testmessage',
                                                message = 'Select data first')
    
    
    ########## create and plot graph of "negative" patient ###############
    dataFirst <- data[[selectFirstPatient]]
    dataSecond <- data[[selectSecondPatient]]
    
    if(!input$vjSegment == "whole data"){
      dataFirst <- dataFirst[dataFirst$VJ.segment == input$vjSegment,]
      dataSecond <- dataSecond[dataSecond$VJ.segment == input$vjSegment,]
    }
    
    if(input$partOfSequence == "whole sequence"){
      arrayFirst <- dataFirst$sequence
      arraySecond <- dataSecond$sequence
    }else if(input$partOfSequence == "CDR3"){
      arrayFirst <- dataFirst$CDR3
      arraySecond <- dataSecond$CDR3
    }else{
      arrayFirst <- dataFirst$V.sequence
      arraySecond <- dataSecond$V.sequence
    }
    
    #returns null when array is numeric(0)
    matrixFirst <- calculateDistances(arrayFirst,arrayFirst)
    matrixSecond <- calculateDistances(arraySecond,arraySecond)
    
    
    #avoid numeric(0) excpetion
    if(is.null(matrixFirst)){
      matrices <- normalizeMatrix(matrixSecond, matrixSecond)
      matrixSecond <- matrices[[1]]
    }else if(is.null(matrixSecond)){
      matrices <- normalizeMatrix(matrixFirst, matrixFirst)
      matrixFirst <- matrices[[2]]
    }else{
      matrices <- normalizeMatrix(matrixFirst, matrixSecond)
      matrixSecond <- matrices[[2]]
      matrixFirst <- matrices[[1]]
    }
    
    
    if(!is.null(matrixFirst)){
      graphFirst <<- buildIGraph(arrayFirst, matrixFirst, thresholdMax = 1.0, thresholdMin = 0.0)
    }
    else {
      graphFirst <<- NULL
    }
    
    
    if(!is.null(matrixSecond)){
      graphSecond <<- buildIGraph(arraySecond, matrixSecond, thresholdMax = 1.0, thresholdMin = 0.0)
    }
    else {
      graphSecond <<- NULL      
    }
    
    comAlgo <- all_communtiy_algorithms()[[input$select_community]]
    cat("community algorithm selected:", input$select_community, "\n")
    
    layout_algo <- all_layout_algorithms()[[input$select_layout]]
    cat("layout algorithm selected:", input$select_layout, "\n")
    
    
    ################ Plot Graphs #####################
    
    if(!is.null(graphFirst)){
      output$firstPatientLabel <- renderText(paste("Patient 1", selectFirstPatient))
      erste<-paste("Patient 1", selectFirstPatient)
      output$firstPatient <- renderVisNetwork({
        patientOne<- plot_graph(graphFirst, edge_threshold=input$num2, community_algorithm = comAlgo, layout_algorithm = layout_algo)
        visExport(patientOne, type = "pdf", name = erste,label = paste("Export as PDF"), style="background-color = #fff")
      })
      
      output$firstPatientDegreeDistribution <- renderPlot(
        hist(degree(graphFirst))
      )
    }
    else {
      output$firstPatientLabel <- renderText("")
      output$firstPatient <- renderVisNetwork({})
      output$firstPatientDegreeDistributionLabel <- renderText("")
      output$firstPatientDegreeDistribution <- renderPlot({})
    }
    
    if(!is.null(graphSecond)){
      output$secondPatientLabel <- renderText(paste("Patient 2", selectSecondPatient))
      zweite<-paste("Patient 2", selectSecondPatient)
      output$secondPatient <- renderVisNetwork({
        patientTwo<- plot_graph(graphSecond, edge_threshold=input$num2, community_algorithm = comAlgo, layout_algorithm = layout_algo)
        visExport(patientTwo, type = "pdf", name = zweite,label = paste("Export as PDF"), style="background-color = #fff" )
      })
      
      output$secondPatientDegreeDistribution <- renderPlot(
        hist(degree(graphSecond))
      )
    }
    else {
      output$secondPatientLabel <- renderText("")
      output$secondPatient <- renderVisNetwork({})
      output$secondPatientDegreeDistributionLabel <- renderText("")
      output$secondPatientDegreeDistribution <- renderPlot({})
    }
  })

  
  #function to update vj segment combo list
  updateVJSegment <- function(){

    posSegmentsFirstPat <- NULL
    posSegmentsSecPat <- NULL
    posSegmentsBoth <- NULL
    
    
    dataFirst <- data[[selectFirstPatient]]
    dataSec <- data[[selectSecondPatient]]

    #loop over first selected patient and store unique vj segments 
    if(!is.null(dataFirst)){
      for( i in 1:nrow(dataFirst)){
        posSegmentsFirstPat <- c(posSegmentsFirstPat, dataFirst$VJ.segment[[i]])
      }
     posSegmentsFirstPat <- unique(posSegmentsFirstPat)
    }

    #loop over second selected patient and store unique vj segments 
    if(!is.null(dataSec)){
      for( i in 1:nrow(dataSec)){
        newSegment <- dataSec$VJ.segment[[i]]
        if(newSegment %in% posSegmentsFirstPat){
          posSegmentsBoth <- c(posSegmentsBoth, newSegment)
          posSegmentsFirstPat <- posSegmentsFirstPat[posSegmentsFirstPat != newSegment]
        }else if(!newSegment %in% posSegmentsBoth){
          posSegmentsSecPat <- c(posSegmentsSecPat, newSegment)
        }
      }
    }
    posSegmentsBoth <- unique(posSegmentsBoth)
    posSegmentsSecPat <- unique(posSegmentsSecPat)

    
    #update combobox first with elements occurs in both
    updateSelectInput(session, "vjSegment", choices = list('whole data' = c("whole data",""), 'Segments for both' = c(posSegmentsBoth,""), 
                      'Segments for 1st patient' = c(posSegmentsFirstPat,""), 'Segments for 2nd patient' = c(posSegmentsSecPat,"")),
                      selected = "whole data")
  
  }
 
  
  
}


shinyApp(ui = ui, server = server)
