input.colors = c("red", "blue", "green")

shinyUI(bootstrapPage(
  
  
  sidebarPanel(
    
    
    tags$head(
      tags$style(type="text/css", "label.radio { display: inline-block; }", ".radio input[type=\"radio\"] { float: none; }"),
      tags$style(type="text/css", "select { max-width: 150px; }"),
      tags$style(type="text/css", "textarea { max-width: 145px; }"),
      tags$style(type="text/css", ".jslider { max-width: 150px; }"),
      tags$style(type='text/css', ".well { padding: 12px; margin-bottom: 5px; max-width: 180px; }"),
      tags$style(type='text/css', ".span4 { max-width: 180px; }")
    ) ,
    
    
    # Date Input
    dateRangeInput(inputId = "dates", 
                   label   ="Date range",
                   start    = as.character(Sys.Date()-10), end = as.character(Sys.Date()-4))
    
  ),
  

  
  # Plot it 
  mainPanel( 
  tabPanel('mfp data', dataTableOutput("mytable4")),
  tabPanel('mfp data', dataTableOutput("mytable5"))
        )
    
  
  
  
  
))