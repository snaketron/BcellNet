library(shiny)

#UI
ui <- fluidPage(
 # h1("test"),
 # p(style = "font-family:Times New Roman","See other apps in the"),
 # a("Shiny Showcase",href = "http://www.rstudio.com/products/shiny/shiny-user-showcase/"),
  
  #html code with tags
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
    fileInput('file1', 'Choose CSV File',
              accept=c('text/csv', 
                       'text/comma-separated-values,text/plain', 
                     '.csv')),  
    #ende fileInput
    
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
  actionButton(inputId = "pcsd", label = " Plot community size distribution", style="margin-top:10px;"),
  actionButton(inputId = "pdd", label = " Export as...", style="margin-top:10px;")
  
 # textBox
 
 # textInput(inputId = "titleInTextBox",
 # label = "Write a title",
 # value = "test label of Histogram"),
 
 #column(width = 2),
),

 mainPanel(
   tabsetPanel(
     tabPanel("tab1", plotOutput("firstPatient"),
              
              visNetworkOutput("secondPatient")),
              
     tabPanel("tab2"),
              
     tabPanel("tab3")
     
   ),
  #output function
  # You must build the object in the server function
  # plotOutput("firstPatient"),
  # plotOutput("secondPatient"),
   
 
 #fluidRow(column(7,plotOutput("firstPatient"), 
 # plotOutput("secondPatient")),offset=5),
 
 # link for ourGitHub just for test href :)
  tags$p(tags$a(href="https://github.com/snaketron/BcellNet","GitHub-BcellNet"))
  
  
  
 )#end of mainPanel
) #end of sidebarLayout


   )  #end of UI



#server side
 
server <- function(input,output){
  
  # renderPlot is a plot
  output$firstPatient <- renderVisNetwork({
    title <- " Patient 1"
    plot_graph(igraph::graph(edges=c(1,2), n=4, directed=FALSE), edge_threshold=input$num, label=title)
  }) # here the input value changes whenever a user changes the input ( slidinput).
  
  # you can also use : data <- reactive({ rnorm(input$num) })
  # Then => output$hist <- renderPlot({ hist(data()) })
  
  
  # output$secondPatient <- renderPlot({
  #   title <- " Patient 2"
  #   hist(rnorm(100), main = title)
  #   # you can also use: main =input$titleInTextBox
  #   
  #   # isolate() makes an non-reactive object
  #   #you can use isolate for main = isolate({input$title}))
  #   
  # })
  output$secondPatient <- renderVisNetwork({
    title <- " Patient 2"
    plot_graph(igraph::graph(edges=c(1,2), n=3, directed=FALSE), edge_threshold=input$num, label=title)
    # you can also use: main =input$titleInTextBox
    
    # isolate() makes an non-reactive object
    #you can use isolate for main = isolate({input$title}))
    
  })
  

  
  
}

shinyApp(ui = ui, server = server)
