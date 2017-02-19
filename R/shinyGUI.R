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
                  choices = "each?",selected = "each?", multiple = FALSE, selectize = TRUE)),
      
      selectInput(inputId = "partOfSequence",label = "Part of Sequence",
                  choices = c("whole sequence", "CDR3", "V sequence"),
                  selected = "whole sequence", multiple = FALSE, selectize = TRUE),
      
      tags$hr(),
      
      #numericInput
      numericInput( inputId = "num2",label = " Egde definition",value =0.01,min = 0,max = 1, step = 0.01, width = "50%"),
  
      #Slider
     sliderInput(inputId = "num", label = "Egde definition", 
                  value = 0.3, min = 0, max = 1, step= 0.1),
      
      # comboBox
      selectInput(inputId = "select_community",label = "Community Selection",
                  choices = names(all_communtiy_algorithms()),
                  selected = NULL, multiple = FALSE, selectize = TRUE),
      
      selectInput(inputId = "select_layout",label = "Layout Generator",
                  choices = names(all_layout_algorithms()),
                  selected = NULL, multiple = FALSE, selectize = TRUE),
      
      #Buttons
      disabled(actionButton(inputId = "pn", label = "Plot Network", style="margin-top:10px;")),
      disabled(actionButton(inputId = "pdd", label = " Plot degree distribution", style="margin-top:10px;")),
      disabled(actionButton(inputId = "pcsd", label = " Plot community size distribution", style="margin-top:10px; margin-bottom:15px;")),
     # disabled(actionButton(inputId = "exportButton", label = " Export as...", style="margin-top:10px;")),
      
      
      # RadioButton
      radioButtons(inputId = "saveAs", label = "Download as type:", choices = list("PNG","PDF"), inline = TRUE),
      textInput(inputId = "downloadPlotFileName", label = h5("Enter file name for download")),
      
      # add Export as Button for Download
     disabled(downloadButton(outputId = "down", label = "Download the plot")),

      #popUp windows Test
     disabled(actionButton("go", "PopUpWindows", style="margin-left:10px;"))
     

     #####################
      
    ), # End of sidebarLayout
    
    
    
    
    ###################def output function in mainPanel ###################################
    mainPanel(
      
      # You must build the object in the server function
      tabsetPanel(
        tabPanel("tab1", visNetworkOutput("firstPatient"),
                 
                 visNetworkOutput("secondPatient")
                 #popupWindows
               # bsModal("modalExample", "Your plot", "go", size = "large",visNetworkOutput("firstPatient"),downloadButton('downloadPlot', 'Download'))
                
                 ),
        
        tabPanel("tab2"),
        
        tabPanel("tab3")
        
        # plotOutput("firstPatient"),
        # plotOutput("secondPatient"),
        #fluidRow(column(7,plotOutput("firstPatient"), 
        # plotOutput("secondPatient")),offset=5),
      ),
      
      #show messages
      tags$head(tags$script(src = "message-handler.js")),
      # link for BcellNet in GitHub just for test href :)
      tags$p(tags$a(href="https://github.com/snaketron/BcellNet","GitHub-BcellNet"))
      
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
                      choices = c("each?",possibleVjSegments),
                      selected = "each?"
    )
    
    # enable buttons if csv file is loaded
    shinyjs::enable("pn")
    shinyjs::enable("pdd")
    shinyjs::enable("pcsd")
    shinyjs::enable("down")
    shinyjs::enable("comboFirstPatient")
    shinyjs::enable("comboSecondPatient")
    shinyjs::enable("vjSegment")
  })
  
  
  #save selected patient into global var
  observeEvent(input$comboFirstPatient, {
    selectFirstPatient <<- input$comboFirstPatient
  })
  observeEvent(input$comboSecondPatient, {
    selectSecondPatient <<- input$comboSecondPatient
  })
  
  
  
  #plot networt button action
  observeEvent(input$pn, {
    
    if(is.null(data)) session$sendCustomMessage(type = 'testmessage',
                                                message = 'Select data first')
    
    
    ########## create and plot graph of "negative" patient ###############
    if(input$partOfSequence == "whole sequence"){
      arrayFirst <- data[[selectFirstPatient]]$sequence
      arraySecond <- data[[selectSecondPatient]]$sequence
    }else if(input$partOfSequence == "CDR3"){
      arrayFirst <- data[[selectFirstPatient]]$CDR3
      arraySecond <- data[[selectSecondPatient]]$CDR3
    }else{
      arrayFirst <- data[[selectFirstPatient]]$V.sequence
      arraySecond <- data[[selectSecondPatient]]$V.sequence
    }
    
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
    
    plota = function(){
   
      title <- paste("Patient ", selectFirstPatient)
      
      plot_graph(graphFirst, edge_threshold=input$num, label=title, community_algorithm = comAlgo, layout_algorithm = layout_algo)
    } 
    output$firstPatient <- renderVisNetwork({
      plota()
    })
    

  
    output$secondPatient <- renderVisNetwork({
      title <- paste("Patient ", selectSecondPatient)
      
      plot_graph(graphSecond, edge_threshold=input$num,label=title, community_algorithm = comAlgo, layout_algorithm = layout_algo)
 
      # you can also use: main =input$titleInTextBox
      # isolate() makes an non-reactive object
      #you can use isolate for main = isolate({input$title}))
      
    })
    
   
    
    ############ Download as...#####################
    #  Get the download file name.
    downloadPlotFileName <- reactive({
      input$downloadPlotFileName
    })
    
    output$down<- downloadHandler(
      #Specify the file name
      filename = function(){
        paste(downloadPlotFileName(), input$saveAs, sep = ".")
        # paste("Plot", input$saveAs, sep = ".")
        
      },
      
      
      # open the device (png() or pdf())
      content = function(file){
        if(input$saveAs == "PNG")
          png(file)
        else
          pdf(file)
        
        # 2:create the plot 
        
        # here you can call (by Print or Func.) your Plot, which you want to print 
        #print(hist(rnorm(100), main = " Patient 2"))
      
        # for GGPLOT
        # print(ggplot(iris, aes(x=x(), y=y())) + geom_point(shape=1)) 
       #plota()
      # plotb() 
       
        # 3:close the device
        dev.off()   
      }
    )
    #################### End of Download as..###############
    
  
    
    
    
    
    
    
  })
  
  
  
 
  
  
}


shinyApp(ui = ui, server = server)
