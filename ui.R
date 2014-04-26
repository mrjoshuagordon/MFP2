input.colors = c("red", "blue", "green")

shinyUI(bootstrapPage(
  
  
  sidebarPanel(

     
    
    tags$head(
      tags$style(type="text/css", "label.radio { display: inline-block; }", ".radio input[type=\"radio\"] { float: none; }"),
      tags$style(type="text/css", "select { max-width: 200px; }"),
      tags$style(type="text/css", "textarea { max-width: 145px; }"),
      tags$style(type="text/css", ".jslider { max-width: 150px; }"),
      tags$style(type='text/css', ".well { padding: 12px; margin-bottom: 5px; max-width: 280px; }"),
      tags$style(type='text/css', ".span4 { max-width: 280px; }")
    ) ,

    
    # Date Input
    dateRangeInput(inputId = "dates", 
                   label   ="Date range",
                   start    = as.character(Sys.Date()-1), end = as.character(Sys.Date())),
    textInput("un", "User Name:", "s2konstantine"),
    actionButton("get", "Submit Date Range"),
    htmlOutput("selectUI")
    
  ),
  

  
  # Plot it 
  mainPanel( div(align = "center",
                 plotOutput(outputId = "main_plot", height = "500px", width="800px") ),
  tabPanel('mfp data', dataTableOutput("mytable4")),
  tabPanel('mfp data', dataTableOutput("mytable5"))
        )
    
  
  
  
  
))