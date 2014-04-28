# 
# 
# shinyUI(bootstrapPage(
#   
#   
#   sidebarPanel(
# 
#      
#     
#     tags$head(
#       tags$style(type="text/css", "label.radio { display: inline-block; }", ".radio input[type=\"radio\"] { float: none; }"),
#       tags$style(type="text/css", "select { max-width: 200px; }"),
#       tags$style(type="text/css", "textarea { max-width: 145px; }"),
#       tags$style(type="text/css", ".jslider { max-width: 150px; }"),
#       tags$style(type='text/css', ".well { padding: 12px; margin-bottom: 5px; max-width: 280px; }"),
#       tags$style(type='text/css', ".span4 { max-width: 280px; }")
#     ) ,
# 
#     
#     # Date Input
#     dateRangeInput(inputId = "dates", 
#                    label   ="Date range",
#                    start    = as.character(Sys.Date()-1), end = as.character(Sys.Date())),
#     textInput("un", "User Name:", "s2konstantine"),
#     actionButton("get", "Submit Date Range"),
#     htmlOutput("selectUI")
#     
#   ),
#   
# 
#   
#   # Plot it 
#   mainPanel( div(align = "center",
#                  plotOutput(outputId = "main_plot", height = "500px", width="800px") ),
#   tabPanel('mfp data', dataTableOutput("mytable4")),
#   tabPanel('mfp data', dataTableOutput("mytable5"))
#         )
#     
#   
#   
#   
#   
# ))

library(markdown)

shinyUI(navbarPage("MyFitnessPal Analyzer",
                   tabPanel("Macro Analysis",
                            sidebarLayout(
                              sidebarPanel(
                                h4(textOutput("text_un")),
                                h6(textOutput("text_start")),
                                textInput("un", "MyFitnessPal User Name:", "s2konstantine"),
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
                            h3(textOutput("text2")),
                            dataTableOutput("mytable5")
                            
                            
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