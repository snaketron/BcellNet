files <- NULL
# setwd("R") is done by shiny since the server file is in here
loadSource <- function(sourceName) {
  pattern <- paste("^", sourceName, "$", sep = "")
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
maxAbsolutValue <- 100 
selectFirstPatient <- NULL
selectSecondPatient <- NULL
graphFirst <- NULL
graphSecond <- NULL
vjSegmentSelected <- FALSE
choicesOfSecondPatient <- NULL

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
      
      disabled(selectInput(inputId = "vjSegmentFirst",label = "Select VJ-Segment for 1st patient",
                           choices = "whole data",selected = "whole data", multiple = FALSE, selectize = TRUE)),
      
      disabled(selectInput(inputId = "vjSegmentSecond",label = "Select VJ-Segment for 2nd patient",
                           choices = "whole data",selected = "whole data", multiple = FALSE, selectize = TRUE)),
      
      selectInput(inputId = "partOfSequence",label = "Select Part of Sequence",
                  choices = c("whole sequence", "CDR3", "V sequence"),
                  selected = "whole sequence", multiple = FALSE, selectize = TRUE),
      
      tags$hr(),
      
      #numericInput
      div(style="display:inline-block;vertical-align:top; width: 100px;",numericInput( inputId = "num2",label = "Relative %",value =1,min = 0,max = 100, step = 1)),
      div(style="display:inline-block;vertical-align:top; width: 150px;",numericInput(inputId = "absolute", label = "Absolute (100):", 0)),
      
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
        ),
        
        tabPanel("Degree Distribution",
                 plotOutput("firstPatientDegreeDistribution"),
                 plotOutput("secondPatientDegreeDistribution")
        ),
        
        tabPanel("Community Size Distribution",
                 plotOutput("firstPatientCommunitySizeDistribution"),
                 plotOutput("secondPatientCommunitySizeDistribution")
        )
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

  #set maximum upload file to 1 gb
  options(shiny.maxRequestSize=1024*1024^2)
  
  observe({
    if(is.null(input$csvFile$datapath)) return(NULL)
    
    #reset some vars
    vjSegmentSelected <<- FALSE
    choicesOfSecondPatient <<- NULL
    
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
    shinyjs::enable("vjSegmentFirst")
    shinyjs::enable("vjSegmentSecond")
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

  
  # when selecting an element in first patient list, this element will be selected in combolist for
  # second patient too, if no element for second patient would selected before.
  observeEvent(input$vjSegmentFirst,{
    selectedItem <- input$vjSegmentFirst
    if(!vjSegmentSelected && (selectedItem %in% choicesOfSecondPatient)){
      updateSelectInput(session, "vjSegmentSecond", selected = selectedItem)
    }
  })

  observeEvent(input$vjSegmentSecond, {
    vjSegmentSelected <<- TRUE
  })

  #plot networt button action
  observeEvent(input$pn, {
    prepareGraphs()

    community_algorithm <- extract_community_algorithm()
    layout_algorithm <- extract_layout_algorithm()

    ######## match max of absolute after uploaded a graph ######
    maxLabel<-paste("Absolute(",maxAbsolutValue,"):")
    updateNumericInput(session,"absolute",label=maxLabel)
    procentValue <-(input$num2/100)*maxAbsolutValue
    absoluteValue<-as.integer(procentValue+0.5)
    updateNumericInput(session,"absolute",label=maxLabel,value =absoluteValue)
    
    ################ Plot Graphs #####################
    if(!is.null(graphFirst)){
      output$firstPatientLabel <- renderText(paste("Patient 1", selectFirstPatient))
      erste<-paste("Patient 1", selectFirstPatient)
      output$firstPatient <- renderVisNetwork({
        patientOne<- plot_graph(graphFirst, edge_threshold=input$num2, community_algorithm = community_algorithm, layout_algorithm = layout_algorithm)
        visExport(patientOne, type = "pdf", name = erste,label = paste("Export as PDF"), style="background-color = #fff")
      })
    }
    else {
      output$firstPatientLabel <- renderText("")
      output$firstPatient <- renderVisNetwork({})
    }
    
    if(!is.null(graphSecond)){
      output$secondPatientLabel <- renderText(paste("Patient 2", selectSecondPatient))
      zweite<-paste("Patient 2", selectSecondPatient)
      output$secondPatient <- renderVisNetwork({
        patientTwo<- plot_graph(graphSecond, edge_threshold=input$num2, community_algorithm = community_algorithm, layout_algorithm = layout_algorithm)
        visExport(patientTwo, type = "pdf", name = zweite,label = paste("Export as PDF"), style="background-color = #fff" )
      })
    }
    else {
      output$secondPatientLabel <- renderText("")
      output$secondPatient <- renderVisNetwork({})
    }
  })

  
  # for plotting the degree distribution
  observeEvent(input$pdd, {
    prepareGraphs()
    maxLabel<-paste("Absolute(",maxAbsolutValue,"):")
    updateNumericInput(session,"absolute",label=maxLabel)
    procentValue <-(input$num2/100)*maxAbsolutValue
    absoluteValue<-as.integer(procentValue+0.5)
    updateNumericInput(session,"absolute",label=maxLabel,value =absoluteValue)

    if(!is.null(graphFirst)){
      output$firstPatientDegreeDistribution <- renderPlot(
        hist(degree(graphFirst))
      )
    }
    else {
      output$firstPatientDegreeDistribution <- renderPlot({})
    }
    
    if(!is.null(graphSecond)){
      output$secondPatientDegreeDistribution <- renderPlot(
        hist(degree(graphSecond))
      )
    }
    else {
      output$secondPatientDegreeDistribution <- renderPlot({})
    }
  })
  
  observeEvent(input$pcsd, {
    prepareGraphs()
    maxLabel<-paste("Absolute(",maxAbsolutValue,"):")
    updateNumericInput(session,"absolute",label=maxLabel)
    procentValue <-(input$num2/100)*maxAbsolutValue
    absoluteValue<-as.integer(procentValue+0.5)
    updateNumericInput(session,"absolute",label=maxLabel,value =absoluteValue)

    community_algorithm <- extract_community_algorithm()

    if(!is.null(graphFirst)){
      output$firstPatientCommunitySizeDistribution <- renderPlot({
        hist(sizes(community_algorithm(graphFirst)))
      })
    }
    else {
      output$firstPatientCommunitySizeDistribution <- renderPlot({})
    }
    
    if(!is.null(graphSecond)){
      output$secondPatientCommunitySizeDistribution <- renderPlot(
        hist(sizes(community_algorithm(graphSecond)))
      )
    }
    else {
      output$secondPatientCommunitySizeDistribution <- renderPlot({})
    }
  })
  
  prepareGraphs <- function() {
    if(is.null(data)) session$sendCustomMessage(type = 'testmessage',
                                                message = 'Select data first')
    

    ########## create and plot graph of patients ###############
    
    dataFirst <- data[[selectFirstPatient]]
    dataSecond <- data[[selectSecondPatient]]

    if(!input$vjSegmentSecond == "whole data"){
      dataSecond <- dataSecond[dataSecond$VJ.segment == input$vjSegmentSecond,]
    }
    
    if(input$partOfSequence == "whole sequence"){
      arraySecond <- dataSecond$sequence
    }else if(input$partOfSequence == "CDR3"){
      arraySecond <- dataSecond$CDR3
    }else{
      arraySecond <- dataSecond$V.sequence
    }
    
    arraySecond <- unique(arraySecond)
    
    #returns null when array is numeric(0)
    matrixFirst <- extract_first_matrix()
    matrixSecond <- calculateDistances(arraySecond)
    maxAbsolutValue <<- max(matrixFirst, matrixSecond)
    #print(maxAbsolutValue)
    
    #avoid numeric(0) exception
    if(is.null(matrixFirst)){
      matrices <- normalizeMatrix(matrixSecond, matrixSecond,groundZero = FALSE)
      matrixSecond <- matrices[[1]]
    }else if(is.null(matrixSecond)){
      matrices <- normalizeMatrix(matrixFirst, matrixFirst, groundZero = FALSE)
      matrixFirst <- matrices[[1]]
    }else{
      matrices <- normalizeMatrix(matrixFirst, matrixSecond, groundZero = FALSE)
      matrixSecond <- matrices[[2]]
      matrixFirst <- matrices[[1]]
    }
    
    if(!is.null(matrixFirst)){
      #map of bcr and its number of occurrence
      arrayFirst <- extract_first_array()
      mulityCounterFirst <- getMapOfBcrs(arrayFirst)
      graphFirst <<- buildIGraph(arrayFirst, matrixFirst, mulityCounterFirst, thresholdMax = 1.0, thresholdMin = 0)
    }
    else {
      graphFirst <<- NULL
    }

    if(!is.null(matrixSecond)){
      #map of bcr and its number of occurrence
      mulityCounterSecond <- getMapOfBcrs(arraySecond)
      graphSecond <<- buildIGraph(arraySecond, matrixSecond, mulityCounterSecond, thresholdMax = 1.0, thresholdMin = 0)
    }
    else {
      graphSecond <<- NULL      
    }
  }
  
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
      for(i in 1:nrow(dataSec)){
        posSegmentsSecPat <- c(posSegmentsSecPat, dataSec$VJ.segment[[i]])
      }
      posSegmentsSecPat <- unique(posSegmentsSecPat)
    }

    choicesOfSecondPatient <<- posSegmentsSecPat
    
    updateSelectInput(session, "vjSegmentFirst", choices = c('whole data', posSegmentsFirstPat), selected = "whole data")
    updateSelectInput(session, "vjSegmentSecond", choices = c('whole data', posSegmentsSecPat), selected = "whole data")
    
  }
  
  #####################Update Inputnumeric#######################


  ############ change absolute value, which it changes relative value ##########

  observeEvent(input$absolute,{
        neuAbsoluteValue<-input$absolute
       # print(neuAbsoluteValue)
    if(!is.null(neuAbsoluteValue)){
      neuProcentValue<-(neuAbsoluteValue*100)/maxAbsolutValue
      procentInInteger<-as.integer(neuProcentValue+0.5)
      updateNumericInput(session,"num2",value = procentInInteger, min=0, max = 100, step = 1)


    }
  })
  ############ change relative value %, which it changes absolute value ##########
  observeEvent(input$num2,{
    maxLabel<-paste("Absolute(",maxAbsolutValue,"):")
    
    if(!is.numeric(input$num2)){
      
      updateNumericInput(session,"num2", min=0, max = 100, step = 1)
      
    }else if(input$num2>0 && input$num2<=100){
      
      userInput<-(input$num2)
      updateNumericInput(session,"num2",value = userInput, min=0, max = 100, step = 1)
      procentValue<-(userInput/100)*maxAbsolutValue
      absoluteValue<-as.integer(procentValue+0.5)

      updateNumericInput(session,"absolute",label=maxLabel,value =absoluteValue)
      
    }else if(input$num2>100){
      updateNumericInput(session,"num2",value = 100, min=0, max = 100, step = 1)
      
    }
  })

  # this wraps the community algorithm into a wrapper where its content is only
  # updated if the reactive event was triggered else the returned value will be
  # the same this is useful for heavy calculation where the plots are based on
  # the same caluclation thus there is no need to recalculate it
  extract_community_algorithm <- eventReactive(input$select_community, {
    cat("community algorithm selected:", input$select_community, "\n")
    selected_community_algorithm <- all_communtiy_algorithms()[[input$select_community]]

    return (selected_community_algorithm)
  })

  # this wraps the layout algorithm into a wrapper where its content is only
  # updated if the reactive event was triggered else the returned value will be
  # the same this is useful for heavy calculation where the plots are based on
  # the same caluclation thus there is no need to recalculate it
  extract_layout_algorithm <- eventReactive(input$select_layout, {
    cat("layout algorithm selected:", input$select_layout, "\n")
    selected_layout_algorithm <- all_layout_algorithms()[[input$select_layout]]

    return (selected_layout_algorithm)
  })
  
  extract_first_array <- eventReactive({
    input$comboFirstPatient
    input$vjSegmentFirst
    input$partOfSequencet
    input$csvFile
  }, {
    dataFirst <- data[[selectFirstPatient]]
    
    print("recalculating first matrix")
    if(!input$vjSegmentFirst == "whole data"){
      dataFirst <- dataFirst[dataFirst$VJ.segment == input$vjSegmentFirst,]
    }
    
    if(input$partOfSequence == "whole sequence"){
      arrayFirst <- dataFirst$sequence
    }else if(input$partOfSequence == "CDR3"){
      arrayFirst <- dataFirst$CDR3
    }else{
      arrayFirst <- dataFirst$V.sequence
    }
    
    arrayFirst <- unique(arrayFirst)
    
    return (arrayFirst)
  })
  
  extract_first_matrix <- eventReactive({
    input$comboFirstPatient
    input$vjSegmentFirst
    input$partOfSequencet
    input$csvFile
  },{
    first_array <- extract_first_array()
    
    matrixFirst <- calculateDistances(first_array)
    
    return (matrixFirst)
  })
  
  extract_first_graph <- eventReactive({
    input$comboFirstPatient
    input$vjSegmentFirst
    input$partOfSequence
    input$absolute
  },
  {
    if(!is.null(matrixFirst)){
      arrayFirst <- extract_first_array()
      
      return (buildIGraph(arrayFirst, matrixFirst, mulityCounterFirst, thresholdMax = 1.0, thresholdMin = 0))
    }
    else {
      return (NULL)
    }
  })

}


shinyApp(ui = ui, server = server)
