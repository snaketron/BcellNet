

usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE, repos="http://cran.us.r-project.org")
  require(p, character.only = TRUE)
}

usePackage("shiny")
usePackage("shinyjs")

data <- NULL
selectFirstPatient <- NULL
selectSecondPatient <- NULL
graphFirst <- NULL

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
      checkboxInput(inputId = "checkbox1", label = " select input File", value = FALSE),
      
      selectInput(inputId = "combo1",label = "",
                  list(`A` = c("Select B-cell subset", "a", "b"),`AB` = c("WA", "OR", "CA")),
                  selected = NULL, multiple = FALSE, selectize = TRUE),
      
      #first patient
      selectInput(inputId = "comboFirstPatient",label = "Select 1st patient",
                  choices = NULL, selected = NULL, multiple = FALSE, selectize = TRUE),
      
      #second patient
      selectInput(inputId = "comboSecondPatient", label = "Select 2nd patient",
                  choices = NULL, selected = NULL, multiple = FALSE, selectize = TRUE),
      
      selectInput(inputId = "combo2",label = "",
                  list(`D` = c("Select VH-JH segment", "a", "b"),`DE` = c("WA", "OR", "CA")),
                  selected = NULL, multiple = FALSE, selectize = TRUE),
      
      tags$hr(),
      
      #Slider
      sliderInput(inputId = "num", label = "Egde definition", 
                  value = 30, min = 1, max = 100),
      
      # comboBox
      selectInput(inputId = "select_community",label = "",
                  choices = all_communtiy_algorithms(),
                  selected = NULL, multiple = FALSE, selectize = TRUE),
      #Buttons
      disabled(actionButton(inputId = "pn", label = "Plot Network", style="margin-top:10px;")),
      disabled(actionButton(inputId = "pdd", label = " Plot degree distribution", style="margin-top:10px;")),
      disabled(actionButton(inputId = "pcsd", label = " Plot community size distribution", style="margin-top:10px;")),
      disabled(actionButton(inputId = "exportButton", label = " Export as...", style="margin-top:10px;"))
      
      # textBox
      
      # textInput(inputId = "titleInTextBox",
      # label = "Write a title",
      # value = "test label of Histogram"),
      #column(width = 2),
      
    ), # End of sidebarLayout
    
    ###################def output function in main###################################
    mainPanel(
      
      # You must build the object in the server function
      tabsetPanel(
        tabPanel("tab1", visNetworkOutput("firstPatient"),
                 
                 visNetworkOutput("secondPatient"),
                 tableOutput('contents')),
        
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


server <- function(input,output, session){
  
  # input$csvFile will be NULL initially. After the user selects
  # and uploads a file, it will be a data frame with 'name',
  # 'size', 'type', and 'datapath' columns. The 'datapath'
  # column will contain the local filenames where the data can
  # be found.
  output$contents <- renderDataTable({
    #read selected csv data
    inFile <- input$csvFile
    if (is.null(inFile))
      return(NULL)
    # data <- csvToSubset(inFile$datapath)
    # return(data)
  })
  
  #update content of patient combobox
  observe({
    if(is.null(input$csvFile$datapath)) return(NULL)
    
    
    data <<- csvToSubset(input$csvFile$datapath)
    x <- names(data)
    
    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- character(0)
    
    # Can also set the label and select items
    updateSelectInput(session, "comboFirstPatient",
                      choices = x,
                      selected = head(x, 1)
    )
    updateSelectInput(session, "comboSecondPatient",
                      choices = x,
                      selected = head(x, 2)
    )
  })
  
  #save selected patient into global var
  observeEvent(input$comboFirstPatient, {
    selectFirstPatient <<- input$comboFirstPatient
  })
  observeEvent(input$comboSecondPatient, {
    selectSecondPatient <<- input$comboSecondPatient
  })
  
  # enable buttons if csv file is loaded
  observe({
    if(is.null(input$csvFile$datapath)) return(NULL)
    
    shinyjs::enable("pn")
    shinyjs::enable("pdd")
    shinyjs::enable("pcsd")
    shinyjs::enable("exportButton")
  })
  
  #plot networt button action
  observeEvent(input$pn, {
    
    if(is.null(data)) session$sendCustomMessage(type = 'testmessage',
                                                message = 'Select data first')
    
    #create and plot graph of "negative" patient
    arrayFirst <- data[[selectFirstPatient]]$sequence
    matrixFirst <- calculateDistances(arrayFirst,arrayFirst)
    graphFirst <<- buildIGraph(arrayFirst, matrixFirst, thresholdMax = 10, thresholdMin = 1)
    print("graphfirst create")
    
    arraySecond <- data[[selectSecondPatient]]$sequence
    matrixSecond <- calculateDistances(arraySecond,arraySecond)
    graphSecond <- buildIGraph(arraySecond, matrixSecond, thresholdMax = 10, thresholdMin = 1)
    print("graphsecond created")
    
    # renderPlot is a plot
    output$firstPatient <- renderVisNetwork({
      title <- paste("Patient ", selectFirstPatient)
      plot_graph(graphFirst, edge_threshold=input$num, label=title)
    }) # here the input value changes whenever a user changes the input ( slidinput).
    # you can also use : data <- reactive({ rnorm(input$num) })
    # Then => output$hist <- renderPlot({ hist(data()) })
    
    output$secondPatient <- renderVisNetwork({
      title <- paste("Patient ", selectSecondPatient)
      plot_graph(graphSecond, edge_threshold=input$num, label=title)
      #plot_graph(igraph::graph(edges=c(1,2), n=3, directed=FALSE), edge_threshold=input$num, label=title)
      # you can also use: main =input$titleInTextBox
      # isolate() makes an non-reactive object
      #you can use isolate for main = isolate({input$title}))
      
    })
    
  })
  
}


shinyApp(ui = ui, server = server)
