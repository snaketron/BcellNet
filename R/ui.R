
#UI
fluidPage(
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
                          
                          selectInput(inputId = "distance_metric_name",label = "Select distance metric",
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
                          div(style="display:inline-block;vertical-align:top; ", disabled(actionButton(inputId = "pn", label = "Plot Network", style="margin-top:10px;"))),
                          div(style="display:inline-block;vertical-align:top; ",disabled(actionButton(inputId = "pdd", label = " Plot degree distribution", style="margin-top:10px;"))),
                          div(style="display:inline-block;vertical-align:top; ", disabled(actionButton(inputId = "pcsd", label = " Plot community size distribution", style="margin-top:10px;")))
                          
                          
                          
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
                                     
                                     textInput("numberOfMaxThreads",label = NULL,value=getOption("sd_num_thread") ,width = "90%"),
                                     
                                     tags$span("Maximum similarity pre parameter",'style'="font-family: Helvetica Neue,Helvetica,Arial,sans-serif;font-size: 14px;margin-bottom:5px; font-weight: 700;",
                                               popify(bsButton("help_Max", label = "", icon = icon("question"), 
                                                               style="info", size = "extra-small"),'Maximum similarity pre parameter', 
                                                      content = paste0("This parameter has effect to weight calculation.<br> It can increase calculation performance, but you will loose information. <br>Set this parameter, when you want to cut your maximum similarity at the beginnig. <br>For example, you only want b-cells, which have a maximum similarity of 95%, set it to 95. <br>"  ),trigger = "focus")),
                                     
                                     textInput("max_weight",label = NULL,value=100,width = "90%"),
                                     
                                     tags$span("Minimum similarity pre parameter ",'style'="font-family: Helvetica Neue,Helvetica,Arial,sans-serif;font-size: 14px;margin-bottom:5px; font-weight: 700;",
                                               popify(bsButton("help_Min", label = "", icon = icon("question"), 
                                                               style="info", size = "extra-small"),'Minimum similarity pre parameter', 
                                                      content = paste0("This parameter has effect to weight calculation. <br>It can increase calculation performance, but you will loose information.<br> Set this parameter, when you want to cut your minimum similarity at the beginnig.<br> For example, you only want b-cells, which have a mimum similarity of 50%, set it to 50.<br> "  ),trigger = "focus")),
                                     
                                     
                                     textInput("min_weight",label=NULL,value=0,width = "90%"),
                                     
                                     tags$span("Max upload size",'style'="font-family: Helvetica Neue,Helvetica,Arial,sans-serif;font-size: 14px;margin-bottom:5px; font-weight: 700;",
                                               popify(bsButton("help_Min", label = "", icon = icon("question"), 
                                                               style="info", size = "extra-small"),'Max upload size"', 
                                                      content = paste0("Set the maximum upload size of files. Use kb, mb or gb.<br> Hint: It is case sensitive!"),trigger = "focus")),
                                     
                                     
                                     textInput("maximum_upload_size",label=NULL, value="1gb",width = "90%")
                                     
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
                                 tags$p ("Tool for network-based analysis of B-cell clonal expansion. License: MIT + file LICENSE"),
                                 tags$h3 ("For more Information:",style = "color:#469CF1;"),
                                 tags$p("Simo Kitanovski <simo.kitanovski@uni-due.de>"),
                                 tags$p ("Github Link: ",a(href="https://github.com/snaketron/BcellNet","BcellNet")   )
                          )
                        )
                        
                        
                        )# End of Help Tab
             )# End of Advance Setting
  )
  
)

