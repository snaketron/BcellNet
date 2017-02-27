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
vjSegmentLinked <- TRUE
choicesOfSecondPatient <- NULL
choicesOfFirstPatient <- NULL
currentMetric <- "Damerau-Levenshtein"
metricPara <- -1
oldMetricPara <- -1

# change this var if you know what you are doing
# -1 means, the number of threads are setting by system
nthread <- -1

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
      
      checkboxInput("linkVJSegments", "Link VJ-Segments", TRUE),
      
      selectInput(inputId = "partOfSequence",label = "Select Part of Sequence",
                  choices = c("whole sequence", "CDR3", "V sequence"),
                  selected = "whole sequence", multiple = FALSE, selectize = TRUE),
      
      selectInput(inputId = "metric",label = "Select metric",
                  choices = c("Levensthein", "Optimal string aligment", "Damerau-Levenshtein",
                              "Longest common substring","Q-gram","Cosine of q-gram","Jaccard of q-gram","Jaro-Winker"),
                  selected = "Damerau-Levenshtein", multiple = FALSE, selectize = TRUE),
      
      disabled(numericInput( inputId = "metricParameter",label = "Parameter",value = 1,min = 0)),
      
      
      tags$hr(),
      
      #numericInput
      #numericInput
      div(style="display:inline-block;vertical-align:top; width: 150px;",numericInput( inputId = "relative_edge_weight_filter",label = "Relative distance in %",value =5,min = 0,max = 100, step = 1.00)),
      div(style="display:inline-block;vertical-align:top; width: 150px;",numericInput(inputId = "absolute_edge_weight_filter", label = "Absolute distance (100):", 0)),
      
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
    choicesOfSecondPatient <<- NULL
    choicesOfFirstPatient <<- NULL
    
    data <<- csvToSubset(input$csvFile$datapath)
    print("data ready!")
    possiblePatients <- names(data)
    print(possiblePatients)
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

  observeEvent(input$linkVJSegments,{
    vjSegmentLinked <<- input$linkVJSegments
  })
  
  observeEvent(input$metricPara,{
    metricPara <<- input$metricPara
    oldMetricPar <<- metricPara
  })
  
  observeEvent(input$metric,{
    
    distanceMetric <- input$metric
    
    # Map distances to shortform
    if(distanceMetric == "Levensthein"){
      distanceMetric <- "lv"
      shinyjs::disable("metricParameter")
      metricPara <<- -1
    }else if(distanceMetric == "Optimal string aligment"){
      distanceMetric <- "osa"
      shinyjs::disable("metricParameter")
      metricPara <<- -1
    }else if(distanceMetric == "Damerau-Levenshtein"){
      distanceMetric <- "dl"
      shinyjs::disable("metricParameter")
      metricPara <<- -1
    }else if(distanceMetric == "Longest common substring"){
      distanceMetric <- "kcs"
      shinyjs::disable("metricParameter")
      metricPara <<- -1
    }else if(distanceMetric == "Q-gram"){
      distanceMetric <- "qgram"
      shinyjs::enable("metricParameter")
      metricPara <<- oldMetricPara
    }else if(distanceMetric == "Cosine of q-gram"){
      distanceMetric <- "cosine"
      shinyjs::enable("metricParameter")
      metricPara <<- oldMetricPara
    }else if(distanceMetric == "Jaccard of q-gram"){
      distanceMetric <- "jaccard"
      shinyjs::enable("metricParameter")
      metricPara <<- oldMetricPara
    }else if(distanceMetric == "Jaro-Winker"){
      distanceMetric <- "jw"
      shinyjs::enable("metricParameter")
      metricPara <<- oldMetricPara
    }else{
      print("ERROR")
    }
    
    currentMetric <<- distanceMetric
  })
  
  # when selecting an element in first patient list, this element will be selected in combolist for
  # second patient too. 
  observeEvent(input$vjSegmentFirst,{
    selectedItem <- input$vjSegmentFirst
    if(vjSegmentLinked && (selectedItem %in% choicesOfSecondPatient)){
      updateSelectInput(session, "vjSegmentSecond", selected = selectedItem)
    }
  })

  observeEvent(input$vjSegmentSecond,{
    selectedItem <- input$vjSegmentSecond
    if(vjSegmentLinked && (selectedItem %in% choicesOfFirstPatient)){
      updateSelectInput(session, "vjSegmentFirst", selected = selectedItem)
    }
  })

  #plot networt button action
  observeEvent(input$pn, {
    community_algorithm <- extract_community_algorithm()
    layout_algorithm <- extract_layout_algorithm()

    ######## match max of absolute after uploaded a graph ######
    maxAbsolutValue <<- extract_max_edge_weight()
    maxLabel<-paste("Absolute(",maxAbsolutValue,"):")
    updateNumericInput(session,"absolute_edge_weight_filter",label=maxLabel)
    procentValue <-(input$relative_edge_weight_filter/100)*maxAbsolutValue
    absoluteValue<-as.integer(procentValue+0.5)
    updateNumericInput(session,"absolute_edge_weight_filter",label=maxLabel,value =absoluteValue)
    
    ################ Plot Graphs #####################
    graphFirst <- extract_first_graph()
    
    if(!is.null(graphFirst)){
      output$firstPatientLabel <- renderText(paste("Patient 1", selectFirstPatient))
      erste<-paste("Patient 1", selectFirstPatient)
      output$firstPatient <- renderVisNetwork({
        edge_threshold <- 1 - (input$relative_edge_weight_filter / 100.0)
        patientOne<- plot_graph(graphFirst, edge_threshold=edge_threshold, community_algorithm = community_algorithm, layout_algorithm = layout_algorithm)
        visExport(patientOne, type = "pdf", name = erste,label = paste("Export as PDF"), style="background-color = #fff")
      })
    }
    else {
      output$firstPatientLabel <- renderText("")
      output$firstPatient <- renderVisNetwork({})
    }
    
    graphSecond <- extract_second_graph()
    if(!is.null(graphSecond)){
      output$secondPatientLabel <- renderText(paste("Patient 2", selectSecondPatient))
      zweite<-paste("Patient 2", selectSecondPatient)
      output$secondPatient <- renderVisNetwork({
        edge_threshold <- 1 - (input$relative_edge_weight_filter / 100.0)
        patientTwo<- plot_graph(graphSecond, edge_threshold=edge_threshold, community_algorithm = community_algorithm, layout_algorithm = layout_algorithm)
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
    maxAbsolutValue <<- extract_max_edge_weight()
    maxLabel<-paste("Absolute(",maxAbsolutValue,"):")
    updateNumericInput(session,"absolute_edge_weight_filter",label=maxLabel)
    procentValue <-(input$relative_edge_weight_filter/100)*maxAbsolutValue
    absoluteValue<-as.integer(procentValue+0.5)
    updateNumericInput(session,"absolute_edge_weight_filter",label=maxLabel,value =absoluteValue)

    graphFirst <- extract_first_graph()
    if(!is.null(graphFirst)){
      output$firstPatientDegreeDistribution <- renderPlot(
        hist(degree(graphFirst))
      )
    }
    else {
      output$firstPatientDegreeDistribution <- renderPlot({})
    }
    
    graphSecond <- extract_second_graph()
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
    maxAbsolutValue <<- extract_max_edge_weight()
    maxLabel<-paste("Absolute(",maxAbsolutValue,"):")
    updateNumericInput(session,"absolute_edge_weight_filter",label=maxLabel)
    procentValue <-(input$relative_edge_weight_filter/100)*maxAbsolutValue
    absoluteValue<-as.integer(procentValue+0.5)
    updateNumericInput(session,"absolute_edge_weight_filter",label=maxLabel,value =absoluteValue)

    community_algorithm <- extract_community_algorithm()
    graphFirst <- extract_first_graph()
    if(!is.null(graphFirst)){
      output$firstPatientCommunitySizeDistribution <- renderPlot({
        hist(sizes(community_algorithm(graphFirst)))
      })
    }
    else {
      output$firstPatientCommunitySizeDistribution <- renderPlot({})
    }
    
    graphSecond <- extract_second_graph()
    if(!is.null(graphSecond)){
      output$secondPatientCommunitySizeDistribution <- renderPlot(
        hist(sizes(community_algorithm(graphSecond)))
      )
    }
    else {
      output$secondPatientCommunitySizeDistribution <- renderPlot({})
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
      for(i in 1:nrow(dataSec)){
        posSegmentsSecPat <- c(posSegmentsSecPat, dataSec$VJ.segment[[i]])
      }
      posSegmentsSecPat <- unique(posSegmentsSecPat)
    }

    choicesOfSecondPatient <<- c('whole data', posSegmentsSecPat)
    choicesOfFirstPatient <<- c('whole data', posSegmentsFirstPat)
    
    updateSelectInput(session, "vjSegmentFirst", choices = c('whole data', posSegmentsFirstPat), selected = "whole data")
    updateSelectInput(session, "vjSegmentSecond", choices = c('whole data', posSegmentsSecPat), selected = "whole data")
    
  }
  
  #####################Update Inputnumeric#######################


  ############ change absolute value, which it changes relative value ##########

  observeEvent(input$absolute_edge_weight_filter,{
        neuAbsoluteValue<-input$absolute_edge_weight_filter
       # print(neuAbsoluteValue)
    if(!is.null(neuAbsoluteValue)){
      maxAbsolutValue <<- extract_max_edge_weight()
      calProcentValue<-(neuAbsoluteValue*100)/maxAbsolutValue
      neuProcentValue<-format.default(calProcentValue,digits = 5)
      updateNumericInput(session,"relative_edge_weight_filter",value = neuProcentValue, min=0, max = 100)
    }
  })
  
  
  ############ change relative value %, which it changes absolute value ##########
  observeEvent(input$relative_edge_weight_filter,{
    maxAbsolutValue <<- extract_max_edge_weight()
    maxLabel<-paste("Absolute(",maxAbsolutValue,"):")
    
    if(!is.numeric(input$relative_edge_weight_filter)){
      
      updateNumericInput(session,"relative_edge_weight_filter", min=0, max = 100)
      
    }else if(input$relative_edge_weight_filter>0 && input$relative_edge_weight_filter<=100){
      
      userInput<-(input$relative_edge_weight_filter)
      updateNumericInput(session,"relative_edge_weight_filter",value = userInput, min=0, max = 100)
      procentValue<-(userInput/100)*maxAbsolutValue
      absoluteValue<-as.integer(procentValue+0.5)

      updateNumericInput(session,"absolute_edge_weight_filter",label=maxLabel,value =absoluteValue)
      
    }else if(input$relative_edge_weight_filter>100){
      updateNumericInput(session,"relative_edge_weight_filter",value = 100, min=0, max = 100)
      
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
    input$partOfSequence
    input$csvFile
  }, {
    print("recalculating first array")
    
    withProgress(message = paste0("Patient ", input$comboSecondPatient, ": filtering sequences"), value = 0, {
      dataFirst <- data[[selectFirstPatient]]
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
      
      incProgress(1)
    })
    
    return (arrayFirst)
  })
  
  extract_second_array <- eventReactive({
    input$comboSecondPatient
    input$vjSegmentSecond
    input$partOfSequence
    input$csvFile
  }, {
    print("recalculating second array")
    
    withProgress(message = paste0("Patient ", input$comboSecondPatient, ": filtering sequences"), value = 0, {
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
      
      incProgress(1)
    })
    
    return (arraySecond)
  })
  
  extract_first_matrix <- eventReactive({
    input$comboFirstPatient
    input$vjSegmentFirst
    input$partOfSequence
    input$csvFile
  },{
    print("recalculating first matrix")
    first_array <- extract_first_array()
    

    withProgress(message = paste0("Patient ", input$comboFirstPatient, ": calculating matrix"), value = 0, {
      matrixFirst <- calculateDistances(first_array, currentMetric, metricPara, nthread = nthread)
      
      incProgress(1)
    })

    return (matrixFirst)
  })
  
  
  extract_second_matrix <- eventReactive({
    input$comboSecondPatient
    input$vjSegmentSecond
    input$partOfSequence
    input$csvFile
  }, {
    print("recalculating second matrix")
    second_array <- extract_second_array()
    

    withProgress(message = paste0("Patient ", input$comboSecondPatient, ": calculating matrix"), value = 0, {
      second_matrix <- calculateDistances(second_array, currentMetric, metricPara)
      
      incProgress(1)
    })

    
    return (second_matrix)
  })
  
  
  extract_normalized_first_matrix <- eventReactive({
    input$comboFirstPatient
    input$vjSegmentFirst
    input$partOfSequence
    input$csvFile
  }, {
    first_matrix <- extract_first_matrix()
    second_matrix <- extract_second_matrix()
    
    withProgress(message = paste0("Patient ", input$comboFirstPatient, ": normalizing matrix"), value = 0, {
      #avoid numeric(0) exception
      if(is.null(first_matrix)){
        matrices <- normalizeMatrix(second_matrix, second_matrix,groundZero = FALSE)
        second_matrix <- matrices[[1]]
      }else if(is.null(second_matrix)){
        matrices <- normalizeMatrix(first_matrix, first_matrix, groundZero = FALSE)
        first_matrix <- matrices[[1]]
      }else{
        matrices <- normalizeMatrix(first_matrix, second_matrix, groundZero = FALSE)
        second_matrix <- matrices[[2]]
        first_matrix <- matrices[[1]]
      }
      
      incProgress(1)
    })
    
    return (first_matrix)
  })
  
  
  extract_normalized_second_matrix <- eventReactive({
    input$comboSecondPatient
    input$vjSegmentSecond
    input$partOfSequence
    input$csvFile
  }, {
    first_matrix <- extract_first_matrix()
    second_matrix <- extract_second_matrix()
    
    withProgress(message = paste0("Patient ", input$comboSecondPatient, ": normalizing matrix"), value = 0, {
      #avoid numeric(0) exception
      if(is.null(first_matrix)){
        matrices <- normalizeMatrix(second_matrix, second_matrix,groundZero = FALSE)
        second_matrix <- matrices[[1]]
      }else if(is.null(second_matrix)){
        matrices <- normalizeMatrix(first_matrix, first_matrix, groundZero = FALSE)
        first_matrix <- matrices[[1]]
      }else{
        matrices <- normalizeMatrix(first_matrix, second_matrix, groundZero = FALSE)
        second_matrix <- matrices[[2]]
        first_matrix <- matrices[[1]]
      }
      
      incProgress(1)
    })
    
    return (second_matrix)
  }) 
  
  extract_max_edge_weight <- eventReactive({
    input$comboFirstPatient
    input$comboSecondPatient
    input$vjSegmentFirst
    input$vjSegmentSecond
    input$partOfSequence
    input$csvFile
  }, {
    print("recalculating max edge weight")
    first_matrix <- extract_first_matrix()
    second_matrix <- extract_second_matrix()
    max_edge_weight <- max(first_matrix, second_matrix)
    
    return (max_edge_weight)
  })
  
  extract_first_multiply_counter <- eventReactive({
    input$comboFirstPatient
    input$vjSegmentFirst
    input$partOfSequence
    input$csvFile
  }, {
    print("recalculating first multiplier counter")
    
    first_array <- extract_first_array()
    first_mult_counter <- getMapOfBcrs(first_array)
    
    return (first_mult_counter)
  })
  
  extract_second_multiply_counter <- eventReactive({
    input$comboSecondPatient
    input$vjSegmentSecond
    input$partOfSequence
    input$csvFile
  }, {
    print("recalculating second multiplier counter")
    
    second_array <- extract_second_array()
    second_mult_counter <- getMapOfBcrs(second_array)
    
    return (second_mult_counter)
  })
  
  extract_first_graph <- eventReactive({
    input$comboFirstPatient
    input$vjSegmentFirst
    input$partOfSequence
    input$absolute_edge_weight_filter
  },
  {
    print("recalculating first graph")
    
    first_norm_matrix <- extract_normalized_first_matrix()
    if(!is.null(first_norm_matrix)){
      first_array <- extract_first_array()
      first_mult_counter <- extract_first_multiply_counter()
      
      # Create a Progress object
      progress <- shiny::Progress$new()
      progress$set(message = "Computing data", value = 0)
      # Close the progress when this reactive exits (even if there's an error)
      on.exit(progress$close())
      
      # Create a callback function to update progress.
      # Each time this is called:
      # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
      #   distance. If non-NULL, it will set the progress to that value.
      # - It also accepts optional detail text.
      update_progress <- function(value = NULL, detail = NULL) {
        progress$set(value = value, detail = detail)
      }
      
      return (buildIGraph(first_array, first_norm_matrix, first_mult_counter, thresholdMax = 1.0, thresholdMin = 0, update_progress))
    }
    else {
      return (NULL)
    }
    
  })
  
  extract_second_graph <- eventReactive({
    input$comboSecondPatient
    input$vjSegmentSecond
    input$partOfSequence
    input$absolute_edge_weight_filter
  },
  {
    print("recalculating second graph")
    
    second_matrix <- extract_normalized_second_matrix()
    if(!is.null(second_matrix)){
      second_array <- extract_second_array()
      second_mult_counter <- extract_second_multiply_counter()
      
      # Create a Progress object
      progress <- shiny::Progress$new()
      progress$set(message = "Computing data", value = 0)
      # Close the progress when this reactive exits (even if there's an error)
      on.exit(progress$close())
      
      # Create a callback function to update progress.
      # Each time this is called:
      # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
      #   distance. If non-NULL, it will set the progress to that value.
      # - It also accepts optional detail text.
      update_progress <- function(value = NULL, detail = NULL) {
        progress$set(value = value, detail = detail)
      }
      
      return (buildIGraph(second_array, second_matrix, second_mult_counter, thresholdMax = 1.0, thresholdMin = 0, update_progress = update_progress))
    }
    else {
      return (NULL)
    }
  })

}


shinyApp(ui = ui, server = server)
