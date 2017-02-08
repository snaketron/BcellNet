library(shiny)
library(visNetwork)

#UI
ui <- fluidPage(
 
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
    
    #select File (Brows)
    fileInput('file1', 'Choose CSV File',
              accept=c('text/csv', 
                       'text/comma-separated-values,text/plain', 
                     '.csv')),  
    
    #shiny.maxRequestSize for more than 5MB
    #ende fileInput
    
    #checkBOx
     # checkboxInput(inputId = "checkbox1", label = " select input File", value = FALSE),
    
     selectInput(inputId = "combo1",label = "",
              list(`A` = c("Select B-cell subset", "a", "b"),`AB` = c("WA", "OR", "CA")),
              selected = NULL, multiple = FALSE, selectize = TRUE),
  
     selectInput(inputId = "combo1",label = "",
              list(`B` = c("Select 1st patient", "a", "b"),`BC` = c("WA", "OR", "CA")),
              selected = NULL, multiple = FALSE, selectize = TRUE),
  
     selectInput(inputId = "combo1",label = "",
              list(`C` = c("Select 2nd patient", "a", "b"),`CD` = c("WA", "OR", "CA")),
              selected = NULL, multiple = FALSE, selectize = TRUE),
  
     selectInput(inputId = "combo1",label = "",
              list(`D` = c("Select VH-JH segment", "a", "b"),`DE` = c("WA", "OR", "CA")),
              selected = NULL, multiple = FALSE, selectize = TRUE),
    

 tags$hr(),
 
  #Slider
     sliderInput(inputId = "num", label = "Egde definition", 
              value = 30, min = 1, max = 100),
  
  # comboBox
     selectInput(inputId = "combo1",label = "",
              list(`East Coast` = c("Community detection", "NJ", "CT"),`West Coast` = c("WA", "OR", "CA")),
              selected = NULL, multiple = FALSE, selectize = TRUE),
  #Buttons
     actionButton(inputId = "pn", label = "Plot Network", style="margin-top:10px;"),
     actionButton(inputId = "pdd", label = " Plot degree distribution", style="margin-top:10px;"),
     actionButton(inputId = "pcsd", label = " Plot community size distribution", style="margin-top:10px; margin-bottom:15px;"),
    
 
 # RadioButton
   radioButtons(inputId = "saveAs", label = "Download as type:", choices = list("PNG","PDF"), inline = TRUE),
   textInput(inputId = "downloadPlotFileName", label = h5("Enter file name for download")),
 
 # add Export as Button for Download
   downloadButton(outputId = "down", label = "Download the plot")
  

 
), # End of sidebarLayout


###################def output function in main###################################
 mainPanel(
  
   # You must build the object in the server function
   tabsetPanel(
     tabPanel("tab1", visNetworkOutput("firstPatient"),
              
              #visNetworkOutput("secondPatient"),
              plotOutput("secondPatient")
             # tableOutput('contents')
              ),
              
     tabPanel("tab2"),
              
     tabPanel("tab3")
     ),
     
     # plotOutput("firstPatient"),
     # plotOutput("secondPatient"),
     #fluidRow(column(7,plotOutput("firstPatient"), 
     # plotOutput("secondPatient")),offset=5),
   
  
  
  
   
 
   
 
 # link for BcellNet in GitHub just for test href :)
  tags$p(tags$a(href="https://github.com/snaketron/BcellNet","GitHub-BcellNet"))
  
  
  
    )#end of mainPanel

  ) #end of sidebarLayout


)  #end of UI



#####################server side####################################

 server <- function(input,output, session){
   

   
######### outPut First Patient ############
   
    plota = function(){
    title <- " Patient 1"
    plot_graph(igraph::graph(edges=c(1,2), n=4, directed=FALSE), edge_threshold=input$num, label=title)
  }
     # here the input value changes whenever a user changes the input ( slidinput).
     # you can also use : data <- reactive({ rnorm(input$num) })
     # Then => output$hist <- renderPlot({ hist(data()) })
  
    
    output$firstPatient <- renderVisNetwork({
      plota()
    })

#############  End Patient 1 ################
    

######### outPut Second Patient ############
  
    plotb = function(){
    title <- " Patient 2"
    hist(rnorm(100), main = title)
      # you can also use: main =input$titleInTextBox
     # isolate() makes an non-reactive object
     #you can use isolate for main = isolate({input$title}))
    
    }
    
  output$secondPatient <- renderPlot({
    plotb()
    
  })
  
  # output$secondPatient <- renderVisNetwork({
  #   title <- " Patient 2"
  #  plot_graph(igraph::graph(edges=c(1,2), n=3, directed=FALSE), edge_threshold=input$num, label=title)
  # you can also use: main =input$titleInTextBox
  # isolate() makes an non-reactive object
  #you can use isolate for main = isolate({input$title}))
  
  # })
  #############  End Patient 2 ################
  
  
  
 
  ############ Eport as...#####################
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
       plotb()
       
      # 3:close the device
      dev.off()   
    }
  )
#################### End of Export as..###############
  
  # anoder Way to export or Download Plot
  
  # output$downloadReport <- downloadHandler(
  #   filename = function() {
  #     paste('my-report', sep = '.', switch(
  #       input$format, PDF = 'pdf', Word = 'docx'
  #     ))
  #   },
  #   
  #   content = function(file) {
  #     src <- normalizePath('report.Rmd')
  #     
  #     # temporarily switch to the temp dir, in case you do not have write
  #     # permission to the current working directory
  #     owd <- setwd(tempdir())
  #     on.exit(setwd(owd))
  #     file.copy(src, 'report.Rmd', overwrite = TRUE)
  #     
  #     library(rmarkdown)
  #     out <- render('report.Rmd', switch(
  #       input$format,
  #       PDF = pdf_document(), Word = word_document()
  #     ))
  #     file.rename(out, file)
  #   }
  # )
  # 
  # 
  # 
  
  ########### End of Export as ..###################
  
  
  
  
  
  ################## read CSV Example ############################
  
  # output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
  #   inFile <- input$file1
  #   
  #   if (is.null(inFile))
  #     return(NULL)
  #   
  #   read.csv(inFile$datapath)
  # })
  ################### end of read CSV #######################
  
  
}

shinyApp(ui = ui, server = server)
