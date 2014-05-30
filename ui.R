
shinyUI(navbarPage("MyFitnessPal Analyzer",
                   tabPanel("Macro Analysis",
                            includeHTML("ad.js"),
                            sidebarLayout(
                              sidebarPanel(
                                h4(textOutput("text_un")),
                                h6(textOutput("text_start")),
                                textInput("un", "MyFitnessPal User Name:", "MyFitnessPalAnalyzer"),
                                dateRangeInput(inputId = "dates", 
                                               label   ="Date range",
                                               start    = as.character(Sys.Date()-3), end = as.character(Sys.Date())),
                                
                                actionButton("get", "Submit User Name and Dates"),
                                radioButtons("plotType1", "Chart Type",
                                             c("Bar Chart"="b","Pie Chart"="p")
                                )
                                
                                
                              ),
                              mainPanel(
                                
                                plotOutput(outputId = "main_plot", height = "500px", width="600px")
                              )
                            )
                   ),
                   tabPanel("Food Analysis",
                            includeHTML("ad.js"),
                            sidebarLayout(
                              sidebarPanel(
                                h3(textOutput("text1")),
                                htmlOutput("selectUI")
                                
                              ),
                              mainPanel(
                               
                                plotOutput(outputId = "main_plot1", height = "500px", width="500px")
                              )
                            ) 
                              
                             
                   ),
                   
                   tabPanel("Food Data",
                            includeHTML("ad.js"),
                            h3(textOutput("text2")),
                            dataTableOutput("mytable5")
                            
                            
                   ),
                   tabPanel("Food Recommendation",
                            includeHTML("ad.js"),
                            sidebarPanel( 
                          #    h3(textOutput("textRec")),
                            textInput("recIn",   "Please enter a calorie goal.  The system will search for similar days in your history during the previous 10 days and select those macros. The 
  algorithm takes a little bit of time to find the best foods for your macros. Calorie Goal:", "0"),
                            actionButton("getRec", "Get Recommendation")
                            )
                            , mainPanel( 
                           # h3(textOutput("text2")),
                                                            
                            dataTableOutput("rec")
                            ) 
                            
                            
                   ),
                   tabPanel("About",
                            
                            fluidRow(
                              column(6,
                                     textOutput("text3"),
                                     column(6,
                                            tags$small(
                                              "I built this web app because the data analytics 
                                              offered by MyFitnessPal is lacking. Please Send 
                                              Bugs and Enhancement Requests to: ",
                                              
                                              a(href="mailto:galaxy.josh@gmail.com",
                                                "galaxy.josh@gmail.com")
                                            )
                                     ),
                                     
                                     column(6,
                                            img(class="img-polaroid",
                                                src=paste0("https://googledrive.com/host/0BzKoMvuVz4xURU8xbk5YT3A2WUE/me.jpg")),
                                            tags$small(
                                              "Source: Photographed in Santa Monica, CA",
                                              a(href="http://www.everytrail.com/guide/los-liones-trail-to-parker-mesa",
                                                "Los Liones Trail")
                                            )
                                     )
                                    
#                                    
                                     
                              )
                            )
                   )
))


