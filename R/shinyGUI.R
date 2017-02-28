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
usePackage("shinyBS")
usePackage("markdown")



data <- NULL
maxAbsolutValue <- 100
selectFirstPatient <- NULL
selectSecondPatient <- NULL
vjSegmentLinked <- TRUE
choicesOfSecondPatient <- NULL
choicesOfFirstPatient <- NULL

absoluteDistance <- 5
relativeDistance <- 95
loopDistance <- TRUE

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
  
  
  # tags$br(), line break 

  # here we write *Input() & *Output() functions
  
  #Create *input functions
  
  #comboBoxes
  navbarPage("BCellNet",
    tabPanel("Plot",
             fluidRow(
               column(1, 
                      tags$img(height=70, width=70, src="logo.png", style="margin-top: 20px; ")
               ),
               column(11, 
                      tags$h1("Find differences in clonal selection", style = "color:#469CF1; font-family:Forte;"),
                      tags$em("between healthy and HCV-infected individuals", style = "color:#428BCA;")
               )
             ),
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
      
      selectInput(inputId = "distance_metric_name",label = "Select metric",
                  choices = row.names(all_distance_metrics()[1]),
                  multiple = FALSE),
      
      disabled(numericInput( inputId = "distance_metric_parameter",label = "Parameter",value = 1,min = 0, step = 0.1)),
      
      
      tags$hr(),
      


    
#numericInput
div(style="display:inline-block;vertical-align:top; width: 200px;",numericInput( inputId = "relative_edge_weight_filter",label = "Similarity in %",value =95,min = 0,max = 100, step = 0.01)),
div(style="display:inline-block;vertical-align:top; width: 200px;",numericInput(inputId = "absolute_edge_weight_filter", label = "Absolute distance (100):", 5), min = 0, max = 100),

      tags$br(),
      
      # HELP POPUP community
     
      tags$span("Community Selection",'style'="font-family: Helvetica Neue,Helvetica,Arial,sans-serif;font-size: 14px;margin-bottom:5px; font-weight: 700;",
                popify(bsButton("q1", label = "", icon = icon("question"), 
                                style="info", size = "extra-small"),'Help for Cluster ', 
                       content = paste0("More details for community structures:<br> ", a("Fast Greedy", href = "http://igraph.org/r/doc/cluster_fast_greedy.html", target="_blank"),", ",
                                                                                       a("Label Prop",  href = "http://igraph.org/r/doc/cluster_label_prop.html",  target="_blank"),", ",
                                                                                       a("Leading Eigen", href = "http://igraph.org/r/doc/cluster_leading_eigen.html", target="_blank"),", ",
                                                                                       a("Louvain", href = "http://igraph.org/r/doc/cluster_louvain.html",target="_blank"),", ",
                                                                                       a("Optimal", href = "http://igraph.org/r/doc/cluster_optimal.html",target="_blank"),", ",
                                                                                       a("Walktrap",href = "http://igraph.org/r/doc/cluster_walktrap.html",target="_blank")), trigger = "focus" ) ),

      
      
      # comboBox
      
      selectInput(inputId = "select_community",label = NULL,
                  choices = names(all_communtiy_algorithms()),
                  selected = NULL, multiple = FALSE, selectize = TRUE),
      

      
      
      # HELP POPUP Layer

      tags$span("Layout Generator",'style'="font-family: Helvetica Neue,Helvetica,Arial,sans-serif;font-size: 14px;margin-bottom:5px; font-weight: 700;",
                popify(bsButton("q2", label = "", icon = icon("question"), 
                                style="info", size = "extra-small"),'Help for Layout ', 
                       content = paste0("More details for community structures:<br> ", a("Star", href = "http://igraph.org/r/doc/layout_as_star.html", target="_blank"),", ",
                                        a("Circle",  href = "http://igraph.org/r/doc/layout_in_circle.html",  target="_blank"),", ",
                                        a("Grid", href = "http://igraph.org/r/doc/layout_on_grid.html", target="_blank"),", ",
                                        a("Sphere", href = " http://igraph.org/r/doc/layout_on_sphere.html", target="_blank"),", ",
                                        a("Randomly", href = "http://igraph.org/r/doc/layout_randomly.html", target="_blank"),", ",
                                        a("Davidson-Harel", href = "http://igraph.org/r/doc/layout_with_dh.html", target="_blank"),", ",
                                        a("DRL", href = "http://igraph.org/r/doc/layout_with_drl.html", target="_blank"),", ",
                                        a("GEM", href = "http://igraph.org/r/doc/layout_with_gem.html", target="_blank"),", ",
                                        a("Fruchterman-Reingold", href = "http://igraph.org/r/doc/layout_with_fr.html",target="_blank"),", ",
                                        a("GraphOpt", href = " http://igraph.org/r/doc/layout_with_graphopt.html",target="_blank"),", ",
                                        a("Large Graph", href = "http://igraph.org/r/doc/layout_with_lgl.html",target="_blank"),", ",
                                        a("multidimensional scaling", href = "http://igraph.org/r/doc/layout_with_mds.html",target="_blank"),", ",
                                        a("Kamada-Kawai",href = "http://igraph.org/r/doc/layout_with_kk.html",target="_blank")), trigger = "focus" ) ),
      
     
    
      selectInput(inputId = "select_layout",label = NULL,
                  choices = names(all_layout_algorithms()),
                  selected = NULL, multiple = FALSE, selectize = TRUE),
      
      #Buttons
   
     # # div(style="display:inline-block;vertical-align:top; ",
     #      tags$span(
     #        popify(
     #         disabled(actionButton(inputId = "pn", label = "Plot Network", style="margin-top:10px;display:inline-block;vertical-align:top; ")),'Plot Network',"1. show plot Network<br> 2. you can see the Graphs."))
     # ,
      div(style="display:inline-block;vertical-align:top; ", disabled(actionButton(inputId = "pn", label = "Plot Network", style="margin-top:10px;"))),
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
  
),# end of Plot tab
navbarMenu("Advanced settings",
           tabPanel("Settings",
           fluidRow(
             column(1, 
                    tags$img(height=70, width=70, src="logo.png", style="margin-top: 20px; ")
             ),
             column(11, 
                    tags$h1("Find differences in clonal selection", style = "color:#469CF1; font-family:Forte;"),
                    tags$em("between healthy and HCV-infected individuals", style = "color:#428BCA;")
             )
           ),
           sidebarLayout(
             sidebarPanel (
               
               tags$span("Threads",'style'="font-family: Helvetica Neue,Helvetica,Arial,sans-serif;font-size: 14px;margin-bottom:5px; font-weight: 700;",
                         popify(bsButton("help_threads", label = "", icon = icon("question"), 
                                         style="info", size = "extra-small"),'Threads ', 
                                content = paste0("The default value is coming from \"sd_num_thrad\".<br> Change this parameter when you want to use another number of threads. <br>This parameter effects time performing by calculation, but can crash your program. <br>Use this parameter if you know what you are doing. "),trigger = "focus")),
                                
               textInput("threads",label = NULL,value=getOption("sd_num_thread") ,width = "90%"),
               
               tags$span("Max weight per parameter",'style'="font-family: Helvetica Neue,Helvetica,Arial,sans-serif;font-size: 14px;margin-bottom:5px; font-weight: 700;",
                         popify(bsButton("help_Max", label = "", icon = icon("question"), 
                                         style="info", size = "extra-small"),'Maximum similarity pre parameter ', 
                                content = paste0("This parameter has effect to weight calculation.<br> It can increase calculation performance, but you will loose information. <br>Set this parameter, when you want to cut your maximum similarity at the beginnig. <br>For example, you only want b-cells, which have a maximum similarity of 95%, set it to 95. <br>"  ),trigger = "focus")),
               
               textInput("max_weight",label = NULL,value=100,width = "90%"),
               
               tags$span("Min weight per parameter",'style'="font-family: Helvetica Neue,Helvetica,Arial,sans-serif;font-size: 14px;margin-bottom:5px; font-weight: 700;",
                         popify(bsButton("help_Min", label = "", icon = icon("question"), 
                                         style="info", size = "extra-small"),'Minimum similarity pre parameter ', 
                                content = paste0("This parameter has effect to weight calculation. <br>It can increase calculation performance, but you will loose information.<br> Set this parameter, when you want to cut your minimum similarity at the beginnig.<br> For example, you only want b-cells, which have a mimum similarity of 50%, set it to 50.<br> "  ),trigger = "focus")),
               
               
               textInput("min_weight",label=NULL,value=0,width = "90%"),
               textInput("upload","Max Upload Size",value="1GB",width = "90%")
               
             ),
             mainPanel(
               
             )
             )
           ),
           
           #tabPanel("Setting"),
           tabPanel("Help",  fluidRow(
             column(1, 
                    tags$img(height=70, width=70, src="logo.png", style="margin-top: 20px; ")
             ),
             column(11, 
                    tags$h1("Find differences in clonal selection", style = "color:#469CF1; font-family:Forte;"),
                    tags$em("between healthy and HCV-infected individuals", style = "color:#428BCA;"),
                    tags$br(),
                    tags$br(),
                    
                    tags$h3("Description:",style = "color:#469CF1;"),
                    tags$p ("A bioinformatic tool for biologists to visualize B-Cell correlation. License: MIT + file LICENSE"),
                    tags$h3 ("For more Information:",style = "color:#469CF1;"),
                    tags$p("Simo Kitanovski <simo.kitanovski@uni-due.de>"),
                    tags$p ("Github Link: ",a(href="https://github.com/snaketron/BcellNet","BcellNet")   )
             )
           )
                    
                   
                      )# End of Help Tab
                    )# End of Advance Setting
           )

)



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
  observeEvent(input$comboFirstPatient, ignoreInit = TRUE, {
    selectFirstPatient <<- input$comboFirstPatient
    updateVJSegment()
  })
  
  
  observeEvent(input$comboSecondPatient, ignoreInit = TRUE, {
    selectSecondPatient <<- input$comboSecondPatient
    updateVJSegment()
  })

  observeEvent(input$linkVJSegments, ignoreInit = TRUE, {
    vjSegmentLinked <<- input$linkVJSegments
  })
  
  
  # when selecting an element in first patient list, this element will be selected in combolist for
  # second patient too. 
  observeEvent(input$vjSegmentFirst, ignoreInit = TRUE, {
    print("recalculate first vj segment")
    
    selectedItem <- input$vjSegmentFirst
    if(vjSegmentLinked && (selectedItem %in% choicesOfSecondPatient)){
      updateSelectInput(session, "vjSegmentSecond", selected = selectedItem)
    }
  })

  observeEvent(input$vjSegmentSecond, ignoreInit = TRUE, {
    print("recalculate second vj segment")
    
    selectedItem <- input$vjSegmentSecond
    if(vjSegmentLinked && (selectedItem %in% choicesOfFirstPatient)){
      updateSelectInput(session, "vjSegmentFirst", selected = selectedItem)
    }
  })
  
  recalculate_edge_weight_filter <- function() {
    print("recalculating absolute edge weight filter")
    maxAbsolutValue <<- extract_max_edge_weight()
    maxLabel<-paste("Absolute distance (",maxAbsolutValue,"):")
    procentValue <- (((100-input$relative_edge_weight_filter)/100)*maxAbsolutValue)
    absoluteValue<-as.integer(procentValue+0.5)
    # if(absoluteValue != absoluteDistance){
    if(loopDistance){
      absoluteDistance <<- absoluteValue
      loopDistance <<- FALSE
      updateNumericInput(session,"absolute_edge_weight_filter",label=maxLabel,value =absoluteValue, min = 0, max = maxAbsolutValue)
    }else{
      loopDistance <<- TRUE
    }

  }
  
  #plot networt button action
  observeEvent(input$pn, {
    community_algorithm <- extract_community_algorithm()
    layout_algorithm <- extract_layout_algorithm()
    recalculate_edge_weight_filter()
    
    ################ Plot Graphs #####################
    first_trimmed_graph <- extract_trimmed_first_graph()
    if(!is.null(first_trimmed_graph)){
      output$firstPatientLabel <- renderText(paste("Patient 1", selectFirstPatient))
      erste<-paste("Patient 1", selectFirstPatient)
      output$firstPatient <- renderVisNetwork({
        edge_threshold <- input$relative_edge_weight_filter / 100.0
        patientOne<- plot_graph(first_trimmed_graph, edge_threshold=edge_threshold, community_algorithm = community_algorithm, layout_algorithm = layout_algorithm)
        visExport(patientOne, type = "pdf", name = erste,label = paste("Export as PDF"), style="background-color = #fff")
      })
    }
    else {
      output$firstPatientLabel <- renderText("")
      output$firstPatient <- renderVisNetwork({})
    }
    
    second_trimmed_graph <- extract_trimmed_second_graph()
    if(!is.null(second_trimmed_graph)){
      output$secondPatientLabel <- renderText(paste("Patient 2", selectSecondPatient))
      zweite<-paste("Patient 2", selectSecondPatient)
      output$secondPatient <- renderVisNetwork({
        edge_threshold <- input$relative_edge_weight_filter / 100.0
        patientTwo<- plot_graph(second_trimmed_graph, edge_threshold=edge_threshold, community_algorithm = community_algorithm, layout_algorithm = layout_algorithm)
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
    recalculate_edge_weight_filter()
    first_trimmed_graph <- extract_trimmed_first_graph()
    if(!is.null(first_trimmed_graph)){
      output$firstPatientDegreeDistribution <- renderPlot(
        hist(degree(first_trimmed_graph))
      )
    }
    else {
      output$firstPatientDegreeDistribution <- renderPlot({})
    }
    
    second_trimmed_graph <- extract_trimmed_second_graph()
    if(!is.null(second_trimmed_graph)){
      output$second_graph <- renderPlot(
        hist(degree(second_trimmed_graph))
      )
    }
    else {
      output$secondPatientDegreeDistribution <- renderPlot({})
    }
  })
  
  observeEvent(input$pcsd, {
    recalculate_edge_weight_filter()
    community_algorithm <- isolate(extract_community_algorithm())
    first_trimmed_graph <- extract_trimmed_first_graph()
    if(!is.null(first_trimmed_graph)){
      output$firstPatientCommunitySizeDistribution <- renderPlot({
        hist(sizes(community_algorithm(first_trimmed_graph)))
      })
    }
    else {
      output$firstPatientCommunitySizeDistribution <- renderPlot({})
    }
    
    second_trimmed_graph <- extract_trimmed_second_graph()
    if(!is.null(second_trimmed_graph)){
      output$secondPatientCommunitySizeDistribution <- renderPlot(
        hist(sizes(community_algorithm(second_trimmed_graph)))
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

  observeEvent(input$absolute_edge_weight_filter, ignoreInit = TRUE, {
    print("changed absolute edge weight filter")
        newAbsoluteValue<-input$absolute_edge_weight_filter
       # print(neuAbsoluteValue)
    if(!is.null(newAbsoluteValue)){
      maxAbsolutValue <<- extract_max_edge_weight()
      calProcentValue<-100 - ((newAbsoluteValue*100)/maxAbsolutValue)
      newProcentValue<-format.default(calProcentValue,digits = 5)
      # if(newProcentValue != relativeDistance){
      if(loopDistance){
        relativeDistance <<- newProcentValue
        loopDistance <<- FALSE
        updateNumericInput(session,"relative_edge_weight_filter",value = newProcentValue, min=0, max = 100)
      }else{
        loopDistance <<- TRUE
      }
    }
  })
  
  
  ############ change relative value %, which it changes absolute value ##########
  observeEvent(input$relative_edge_weight_filter, ignoreInit = TRUE, {
    print("changed relative edge weight filter")

    maxAbsolutValue <<- extract_max_edge_weight()
    maxLabel<-paste("Absolute distance (",maxAbsolutValue,"):")
    
    if(!is.numeric(input$relative_edge_weight_filter)){
      
      #updateNumericInput(session,"relative_edge_weight_filter", min=0, max = 100)
      
    }else if(input$relative_edge_weight_filter>=0 && input$relative_edge_weight_filter<=100){
      
      userInput<-(input$relative_edge_weight_filter)
      #updateNumericInput(session,"relative_edge_weight_filter",value = userInput, min=0, max = 100)
      procentValue<-((100-userInput)/100)*maxAbsolutValue
      absoluteValue<-as.integer(procentValue+0.5)

      if(loopDistance){
        absoluteDistance <-- absoluteValue
        loopDistance <<- FALSE
        updateNumericInput(session,"absolute_edge_weight_filter",label=maxLabel,value =absoluteValue, min = 0, max = maxAbsolutValue)
      }else{
        loopDistance <<- TRUE
      }
      
    }else if(input$relative_edge_weight_filter>100){
      relativeDistance <<- 100
      updateNumericInput(session,"relative_edge_weight_filter",value = 100, min=0, max = 100)
      
    }else{
      loopDistance <<- TRUE
    }
  })

  # this wraps the community algorithm into a wrapper where its content is only
  # updated if the reactive event was triggered else the returned value will be
  # the same this is useful for heavy calculation where the plots are based on
  # the same caluclation thus there is no need to recalculate it
  extract_community_algorithm <- reactive({
    print(paste("community algorithm selected:", input$select_community))
    selected_community_algorithm <- all_communtiy_algorithms()[[input$select_community]]

    return (selected_community_algorithm)
  })

  # this wraps the layout algorithm into a wrapper where its content is only
  # updated if the reactive event was triggered else the returned value will be
  # the same this is useful for heavy calculation where the plots are based on
  # the same caluclation thus there is no need to recalculate it
  extract_layout_algorithm <- reactive({
    print(paste("layout algorithm selected:", input$select_layout))
    selected_layout_algorithm <- all_layout_algorithms()[[input$select_layout]]

    return (selected_layout_algorithm)
  })
  
  
  # this is a special handler to rerender the GUI interactivly
  observeEvent(input$distance_metric_name, {
    distance_metric_opts <- all_distance_metrics()[input$distance_metric_name, ]
    distance_metric_parameter_enabled <- distance_metric_opts$parameter_enabled
    distance_metric_parameter_name <- distance_metric_opts$parameter_name
    if (distance_metric_parameter_enabled) {
      shinyjs::enable("distance_metric_parameter")
      updateNumericInput(session, "distance_metric_parameter", label = paste("Parameter (",distance_metric_parameter_name, ")"))
    }
    else {
      shinyjs::disable("distance_metric_parameter")
      updateNumericInput(session, "distance_metric_parameter", label = paste("Parameter (",distance_metric_parameter_name, ")"))
    }
  })
  
  
  observeEvent(input$distance_metric_parameter, ignoreInit = TRUE,{
    
    if(input$distance_metric_parameter < 0){
      updateNumericInput(session, "distance_metric_parameter", value = 0)
    }
    
  })
  
  
  extract_distance_metric <- eventReactive({
    input$distance_metric_name
  }, {
    print("recalculating distance metric")
    
    # input$distance_metric_name is a name
    # but the algorithms use the short name which is saved in the DistanceMetric.R
    distance_metric_opts <- all_distance_metrics()[input$distance_metric_name, ]
    distance_metric <- distance_metric_opts$algorithm
    distance_metric_parameter_enabled <- distance_metric_opts$parameter_enabled
    distance_metric_parameter_name <- distance_metric_opts$parameter_name
    if (distance_metric_parameter_enabled) {
      shinyjs::enable("distance_metric_parameter")
      updateNumericInput(session, "distance_metric_parameter", label = paste("Parameter (",distance_metric_parameter_name, ")"))
    }
    else {
      shinyjs::disable("distance_metric_parameter")
      updateNumericInput(session, "distance_metric_parameter", label = paste("Parameter (",distance_metric_parameter_name, ")"))
    }
    
    return (distance_metric)
  })
  
  extract_distance_metric_parameter <- eventReactive({
    input$distance_metric_name
    input$distance_metric_parameter
  }, {
    print("recalculating distance metric parameter")
    
    distance_metric_opts <- all_distance_metrics()[input$distance_metric_name, ]
    distance_metric_parameter_enabled <- distance_metric_opts$parameter_enabled
    if (distance_metric_parameter_enabled) {
      return (input$distance_metric_parameter)
    }
    else {
      return (-1)
    }
  })
  
  extract_first_array <- eventReactive({
    input$comboFirstPatient
    input$vjSegmentFirst
    input$partOfSequence
    input$csvFile
    input$linkVJSegments
  }, {
    print("recalculating first array")
    
    withProgress(message = paste0("Patient ", input$comboFirstPatient, ": filtering sequences"), value = 0, {
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
    input$linkVJSegments
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
    input$distance_metric_name
    input$distance_metric_parameter
    input$linkVJSegments
  },{
    print("recalculating first matrix")
    
    first_array <- extract_first_array()
    distance_metric <- extract_distance_metric()
    distance_metric_parameter <- extract_distance_metric_parameter()

    withProgress(message = paste0("Patient ", input$comboFirstPatient, ": calculating matrix"), value = 0, {
      matrixFirst <- calculateDistances(first_array, distance_metric, distance_metric_parameter, nthread = nthread)
      
      incProgress(1)
    })

    return (matrixFirst)
  })
  
  
  extract_second_matrix <- eventReactive({
    input$comboSecondPatient
    input$vjSegmentSecond
    input$partOfSequence
    input$csvFile
    input$distance_metric_name
    input$distance_metric_parameter
    input$linkVJSegments
  }, {
    print("recalculating second matrix")
    second_array <- extract_second_array()
    distance_metric <- extract_distance_metric()
    distance_metric_parameter <- extract_distance_metric_parameter()

    withProgress(message = paste0("Patient ", input$comboSecondPatient, ": calculating matrix"), value = 0, {
      second_matrix <- calculateDistances(second_array, distance_metric, distance_metric_parameter, nthread = nthread)
      
      incProgress(1)
    })

    
    return (second_matrix)
  })
  
  
  extract_normalized_first_matrix <- eventReactive({
    input$comboSecondPatient
    input$comboFirstPatient
    input$vjSegmentSecond
    input$vjSegmentFirst
    input$partOfSequence
    input$distance_metric_name
    input$distance_metric_parameter
    input$csvFile
    input$linkVJSegments
  }, {
    print("Normalizing first matrix")
    
    first_matrix <- extract_first_matrix()
    second_matrix <- extract_second_matrix()
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    progress$set(message = paste0("Patient ", input$comboFirstPatient, ": "), value = 0)
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
    
    #avoid numeric(0) exception
    if(is.null(first_matrix)){
      matrices <- normalizeMatrix(second_matrix, second_matrix,groundZero = FALSE, update_progress = update_progress)
      second_matrix <- matrices[[1]]
    }else if(is.null(second_matrix)){
      matrices <- normalizeMatrix(first_matrix, first_matrix, groundZero = FALSE, update_progress = update_progress)
      first_matrix <- matrices[[1]]
    }else{
      matrices <- normalizeMatrix(first_matrix, second_matrix, groundZero = FALSE, update_progress = update_progress)
      second_matrix <- matrices[[2]]
      first_matrix <- matrices[[1]]
    }
    
    return (first_matrix)
  })
  
  
  extract_normalized_second_matrix <- eventReactive({
    input$comboSecondPatient
    input$comboFirstPatient
    input$vjSegmentSecond
    input$vjSegmentFirst
    input$partOfSequence
    input$distance_metric_name
    input$distance_metric_parameter
    input$csvFile
    input$linkVJSegments
  }, {
    print("Normalizing second matrix")
    
    first_matrix <- extract_first_matrix()
    second_matrix <- extract_second_matrix()
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    progress$set(message = paste0("Patient ", input$comboSecondPatient, ": "), value = 0)
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
    
    #avoid numeric(0) exception
    if(is.null(first_matrix)){
      matrices <- normalizeMatrix(second_matrix, second_matrix,groundZero = FALSE, update_progress = update_progress)
      second_matrix <- matrices[[1]]
    }else if(is.null(second_matrix)){
      matrices <- normalizeMatrix(first_matrix, first_matrix, groundZero = FALSE, update_progress = update_progress)
      first_matrix <- matrices[[1]]
    }else{
      matrices <- normalizeMatrix(first_matrix, second_matrix, groundZero = FALSE, update_progress = update_progress)
      second_matrix <- matrices[[2]]
      first_matrix <- matrices[[1]]
    }
      
    return (second_matrix)
  }) 
  
  extract_max_edge_weight <- eventReactive({
    # input$comboSecondPatient
    # input$comboFirstPatient
    # input$vjSegmentSecond
    # input$vjSegmentFirst
    # input$partOfSequence
    # input$distance_metric_name
    # input$distance_metric_parameter
    # input$csvFile
    # # input$linkVJSegments
    extract_first_matrix
    extract_second_matrix
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
    input$linkVJSegments
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
    input$linkVJSegments
  }, {
    print("recalculating second multiplier counter")
    
    second_array <- extract_second_array()
    second_mult_counter <- getMapOfBcrs(second_array)
    
    return (second_mult_counter)
  })
  
  extract_first_graph <- eventReactive({
    input$comboSecondPatient
    input$comboFirstPatient
    input$vjSegmentSecond
    input$vjSegmentFirst
    input$partOfSequence
    input$distance_metric_name
    input$distance_metric_parameter
    input$csvFile
    input$linkVJSegments
    input$min_weight
    input$max_weight
    extract_relative_min_weight
    extract_relative_max_weight
  },
  {
    print("recalculating first graph")
    
    first_norm_matrix <- extract_normalized_first_matrix()
    if(!is.null(first_norm_matrix)){
      first_array <- extract_first_array()
      first_mult_counter <- extract_first_multiply_counter()
      
      # Create a Progress object
      progress <- shiny::Progress$new()
      progress$set(message = paste0("Patient ", input$comboFirstPatient, ": "), value = 0)
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
      
      return (buildIGraph(first_array, first_norm_matrix, first_mult_counter, thresholdMax = extract_relative_max_weight(), thresholdMin = extract_relative_min_weight(), update_progress))
    }
    else {
      return (NULL)
    }
    
  })
  
  extract_second_graph <- eventReactive({
    input$comboSecondPatient
    input$comboFirstPatient
    input$vjSegmentSecond
    input$vjSegmentFirst
    input$partOfSequence
    input$distance_metric_name
    input$distance_metric_parameter
    input$csvFile
    input$linkVJSegments
    input$min_weight
    input$max_weight
    extract_relative_min_weight
    extract_relative_max_weight
  },
  {
    print("recalculating second graph")
    
    second_matrix <- extract_normalized_second_matrix()
    if(!is.null(second_matrix)){
      second_array <- extract_second_array()
      second_mult_counter <- extract_second_multiply_counter()
      
      # Create a Progress object
      progress <- shiny::Progress$new()
      progress$set(message = paste0("Patient ", input$comboSecondPatient, ": "), value = 0)
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
      
      return (buildIGraph(second_array, second_matrix, second_mult_counter, thresholdMax = extract_relative_max_weight(), thresholdMin = extract_relative_min_weight(), update_progress = update_progress))
    }
    else {
      return (NULL)
    }
  })
  
  
  extract_relative_min_weight <- reactive({
    relative_min_weight <- as.numeric(input$min_weight)/100
    print(paste("recalculating relative min weight: ", relative_min_weight))
    
    return (relative_min_weight)
  })
  
  
  extract_relative_max_weight <- reactive({
    relative_max_weight <- as.numeric(input$max_weight)/100
    print(paste("recalculating relative max weight: ", relative_max_weight))
    
    return (relative_max_weight)
  })

  extract_trimmed_first_graph <- eventReactive({
    input$comboSecondPatient
    input$comboFirstPatient
    input$vjSegmentSecond
    input$vjSegmentFirst
    input$partOfSequence
    input$distance_metric_name
    input$distance_metric_parameter
    input$csvFile
    input$linkVJSegments
    input$relative_edge_weight_filter
    input$min_weight
    input$max_weight
    extract_relative_min_weight
    extract_relative_max_weight
  }, {
    first_graph <- extract_first_graph()
    trimmed_first_graph <- trim_igraph_by_similarity(first_graph, input$relative_edge_weight_filter, 1)
    
    return (trimmed_first_graph)
  })

  extract_trimmed_second_graph <- eventReactive({
    input$comboSecondPatient
    input$comboFirstPatient
    input$vjSegmentSecond
    input$vjSegmentFirst
    input$partOfSequence
    input$distance_metric_name
    input$distance_metric_parameter
    input$csvFile
    input$linkVJSegments
    input$relative_edge_weight_filter
    input$min_weight
    input$max_weight
    extract_relative_min_weight
    extract_relative_max_weight
  }, {
    second_graph <- extract_second_graph()
    trimmed_second_graph <- trim_igraph_by_similarity(second_graph, input$relative_edge_weight_filter, 1)

    return (trimmed_second_graph)
  })

}


shinyApp(ui = ui, server = server)
