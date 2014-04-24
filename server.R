source("libraries.R")

tables = getData()

shinyServer(function(input, output) {

    
getFood = reactive({
    
  options(stringsAsFactors=TRUE)
  
  min.date = input$dates[1]
  max.date = input$dates[2]
  date = seq(as.Date(min.date, "%Y-%m-%d"), as.Date(max.date, "%Y-%m-%d"), by=1)
  
  output = tables[tables$day %in% date,]
  output  
    
})

output$mytable4 = renderDataTable({ 
  getFood()
  
}, options = list(aLengthMenu = c(5, 20, 30), iDisplayLength = 5))


output$mytable5 = renderDataTable({ 
  food = getFood()
  cleanFood(food)
  
}, options = list(aLengthMenu = c(5, 20, 30), iDisplayLength = 5))



})