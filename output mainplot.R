
output$main_plot <- renderPlot({
  # Get Data 
  
  food = getFood()
  tables1 = food 

  par(mfrow=c(1,2))
  
  date = seq(from = input$dates[1], to = input$dates[2], by=1) 
  
  tables = tables1[as.character(tables1$day) %in% as.character(date) ,]
  
  
  
  
  # Macro Pie Chart Aggregate 
  food.out = data.frame()
  
  for(i in 1:length(unique(tables$day))){
    food = tables[as.character(tables$day) == date[i],]
    #food = na.omit(food)
    
    
    calories = sum(as.numeric(as.character(food$Calories)))
    carbs = sum(as.numeric(gsub("[A-z]", "", food$Carbs) ))
    fat = sum(as.numeric(gsub("[A-z]", "", food$Fat) ))
    protein = sum(as.numeric(gsub("[A-z]", "", food$Protein) ))
    fiber = sum(as.numeric(gsub("[A-z]", "", food$Fiber) ))
    
    temp = data.frame(calories, carbs, fat, protein, fiber)
    food.out = rbind(food.out, temp)
    
  }
  
  
  
# Plot Macro Pie Chart
  
  par(mar=c(3.1, 3.1, 3.1, 3.1), xpd=TRUE) 
  macros = c(mean(food.out$carbs)-mean(food.out$fiber), mean(fiber), mean(food.out$fat), mean(food.out$protein))
  lbls = c("Carbs", "Fiber",  "Fat","Protein")
  pie3D(macros,labels=lbls,explode=0.1,  main=paste("Average Macros:", format(min(date), "%b-%d"), "to",  format(max(date), "%b-%d"), sep=" "), col=c("darkgoldenrod2", "chartreuse4", "brown4", "blue3" ))
  legend('bottomleft', pt.bg=c("darkgoldenrod2", "chartreuse4", "brown4", "blue3" ), pch=c(22,22,22,22), 
         legend=paste(lbls, ":",  round(macros,0), "grams", sep=" "), inset=c(.05,.05))
  
   
    
  food.data =   cleanFood(food)   
  
  
  ff = aggregate(x =food.data$quant , by = list( food.data$day, food.data$food.type.Food, food.data$unit  ), FUN=sum) 
  names(ff) = c("date","food", "unit", "quantity")
  
  
  ### List starts empty #########################################
  if(length(input$foodName) == 0) {
    
    food.query = as.character(ff[,2][1])
    
  } else {
    
    food.query = input$foodName 
    
  }
  #####################################################################
  
  iv = ff[which(ff[,2] == food.query ),]
  iv$date = as.Date(iv$date, "%Y-%m-%d")
  dr = seq(min(date), max(date), by=1)
  
  ###### Iniate an Empty Data Frame with Date Range ########################################
  iv1 = data.frame(dr, rep(0,length(dr)))
  names(iv1) = c("date", "quantity")
  
  ##### Fill the Data Frame ################################################################# 
  iv1$quantity[match(iv$date,iv1$date)] = iv$quantity
  
  
  
  par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE) 
  
  bp = barplot(iv1$quantity, names.arg= format(iv1$date, "%b-%d"), main=
                 paste(
                   paste("Consumption of", food.query ,":", sum(iv1$quantity), unique(iv$unit),  sep=" "), 
                   paste(format(min(date), "%b-%d"), "to",  format(max(date), "%b-%d"), sep=" "),
                   sep = "\n"
                 )
               
               
               
               , xlab="Dates", ylab=as.character(unique(iv$unit)))
  
  
  legend('topright', pch=c(22,NA), pt.bg=c("gray", "red"), lty=c(NA,2),
         legend=c(as.character(unique(iv$unit)), "  Trend"), inset=c(-.1,0), col=c("gray", "red") )
  
  lines(lowess(bp, iv1$quantity), col="red", lty=2)
  # abline(reg=lm(iv1$quantity~bp), col="red", lty=2)
  
  

  
  
  
})