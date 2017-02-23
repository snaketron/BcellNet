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
  tags$img(height=80, width=80, src="logo.png"),
  tags$h1("Find differences in clonal selection", style = "color:#469CF1; font-family:Forte;"),
  tags$em("between healthy and HCV-infected individuals", style = "color:#428BCA;"),
  
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

        tabPanel("tab1", 
                 tags$label(textOutput("firstPatientLabel"), 'for'="firstPatient", 'style'="margin-top: 5px;"),
                 visNetworkOutput("firstPatient"),

                 tags$label(textOutput("secondPatientLabel"), 'for'="secondPatient"),

                 visNetworkOutput("secondPatient")
                 #popupWindows
                # bsModal("modalExample", "Your plot", "go", size = "large",visNetworkOutput("firstPatient"),downloadButton('downloadPlot', 'Download'))
                 )
        
      #  tabPanel("tab2"),
        
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
  
  
##########   update content of patient combobox #############
  observe({
    if(is.null(input$csvFile$datapath)) return(NULL)
    
    
    data <<- csvToSubset(input$csvFile$datapath)
    possiblePatients <- names(data)
    possibleVjSegments <- NULL
    
    # Can use character(0) to remove all choices
    if (is.null(possiblePatients))
      possiblePatients <- character(0)
    
    # Can also set the label and select items
    updateSelectInput(session, "comboFirstPatient",
                      choices = possiblePatients,
                      selected = head(possiblePatients, 1)
    )
    updateSelectInput(session, "comboSecondPatient",
                      choices = possiblePatients,
                      selected = head(possiblePatients, 2)
    )
   
    
    
    
    #loop over patients and update combobox with vj segment entries
    for(i in 1:length(data)){
      possibleVjSegments <- c(possibleVjSegments,data[[i]]$VJ.segment)
    }
    possibleVjSegments <- unique(possibleVjSegments)
    
    
    updateSelectInput(session, "vjSegment",
                      choices = c("whole data",possibleVjSegments),
                      selected = "whole data"
    )
    
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
  })
  observeEvent(input$comboSecondPatient, {
    selectSecondPatient <<- input$comboSecondPatient
  })

  #####################Update Inputnumeric#######################

   observeEvent(input$num2,{
     if(input$num2>1){
                
               updateNumericInput(session,"num2",value = 0.01, min=0, max = 1, step = 0.01)
     }
   })      
  
  
  
  #plot networt button action
  observeEvent(input$pn, {
    
    if(is.null(data)) session$sendCustomMessage(type = 'testmessage',
                                                message = 'Select data first')
    
    
    ########## create and plot graph of "negative" patient ###############
    dataFirst <- data[[selectFirstPatient]]
    dataSecond <- data[[selectSecondPatient]]
    
    dataFirst <- dataFirst[dataFirst$VJ.segment == input$vjSegment,]
    dataSecond <- dataSecond[dataSecond$VJ.segment == input$vjSegment,]
    
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
    print(arrayFirst)
    print(arraySecond)
    
    matrixFirst <- calculateDistances(arrayFirst,arrayFirst)
    matrixSecond <- calculateDistances(arraySecond,arraySecond)

    matrices <- normalizeMatrix(matrixFirst, matrixSecond)
    
    matrixSecond <- matrices[[1]]
    matrixFirst <- matrices[[2]]
    
    graphFirst <<- buildIGraph(arrayFirst, matrixFirst, thresholdMax = 1.0, thresholdMin = 0.0)
    graphSecond <- buildIGraph(arraySecond, matrixSecond, thresholdMax = 1.0, thresholdMin = 0.0)

    
    comAlgo <- all_communtiy_algorithms()[[input$select_community]]
    cat("community algorithm selected:", input$select_community, "\n")
    
    layout_algo <- all_layout_algorithms()[[input$select_layout]]
    cat("layout algorithm selected:", input$select_layout, "\n")
        
    
    ################ Plot Graphs #####################

    output$firstPatientLabel <- renderText(paste("Patient 1", selectFirstPatient))
    erste<-paste("Patient 1", selectFirstPatient)
    output$firstPatient <- renderVisNetwork({
    patientOne<- plot_graph(graphFirst, edge_threshold=input$num2, community_algorithm = comAlgo, layout_algorithm = layout_algo)
    visExport(patientOne, type = "pdf", name = erste,label = paste("Export as PDF"), style="background-color = #fff" )
       
    
      
    })
   
    output$secondPatientLabel <- renderText(paste("Patient 2", selectSecondPatient))
    zweite<-paste("Patient 2", selectSecondPatient)
    output$secondPatient <- renderVisNetwork({
    patientTwo<- plot_graph(graphSecond, edge_threshold=input$num2, community_algorithm = comAlgo, layout_algorithm = layout_algo)
    visExport(patientTwo, type = "pdf", name = zweite,label = paste("Export as PDF"), style="background-color = #fff" )
 
      

    })
    

    ############ Download as...#####################
    # #  Get the download file name.
    # downloadPlotFileName <- reactive({
    #   input$downloadPlotFileName
    # })
    # 
    # output$down<- downloadHandler(
    #   #Specify the file name
    #   filename = function(){
    #     paste(downloadPlotFileName(), input$saveAs, sep = ".")
    #     # paste("Plot", input$saveAs, sep = ".")
    #     
    #   },
    #   
    #   
    #   # open the device (png() or pdf())
    #   content = function(file){
    #     if(input$saveAs == "PNG")
    #       png(file)
    #     else
    #       pdf(file)
    #     
    #     # 2:create the plot 
    # 
    #     # here you can call (by Print or Func.) your Plot, which you want to print 
    #     #print(hist(rnorm(100), main = " Patient 2"))
    #   
    #     # for GGPLOT
    #     # print(ggplot(iris, aes(x=x(), y=y())) + geom_point(shape=1)) 
    #    
    #     
    #     # 3:close the device
    #     dev.off()   
    #   }
    # )
    #################### End of Download as..###############
    
  
    
    
    
    
    
    
  })
  
  
  
 
  
  
}


shinyApp(ui = ui, server = server)
