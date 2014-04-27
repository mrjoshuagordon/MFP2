source("libraries.R")


shinyServer(function(input, output,session) {
  
  dataInput <- reactive({  
    if(input$get == 0){
      options(stringsAsFactors=TRUE)
    
    
    min.date = Sys.Date()-1
    max.date = Sys.Date()
    date = seq(as.Date(min.date, "%Y-%m-%d"), as.Date(max.date, "%Y-%m-%d"), by=1)
    
    theurl = paste("http://www.myfitnesspal.com/reports/printable_diary/", input$un, "?from=", date[1], "&to=" , date[1], sep="") 
    scrape = readHTMLTable(theurl)
    
    ns = names(scrape[[1]])
    
    theurl = paste("http://www.myfitnesspal.com/reports/printable_diary/", input$un, "?from=", min.date, "&to=" , max.date, sep="") 
    scrape = readHTMLTable(theurl, header=F)
    
    
    ## add date back in *sigh*
    
    ######## Append Date ################################################
    tables = data.frame()
    for(i in 1:length(scrape)){
      
      if(ncol(scrape[[i]])==length(ns)){
        
        day = rep(date[i],nrow(scrape[[i]])) 
        temp =   data.frame(scrape[[i]], day ) 
        tables = rbind(tables, temp)
      } else{
        tables =tables
        
      }
      
    }
    
    tables = na.omit(tables)
    names(tables) = c(ns, "day")
    tables$day = as.Date(tables$day, "%Y-%m-%d")
    tables
    }
    
    isolate({
      
      
      options(stringsAsFactors=TRUE)
      
      
      min.date = input$dates[1]
      max.date = input$dates[2]
      date = seq(as.Date(min.date, "%Y-%m-%d"), as.Date(max.date, "%Y-%m-%d"), by=1)
      
      theurl = paste("http://www.myfitnesspal.com/reports/printable_diary/", input$un, "?from=", date[1], "&to=" , date[1], sep="") 
      scrape = readHTMLTable(theurl)
      
      ns = names(scrape[[1]])
      
      theurl = paste("http://www.myfitnesspal.com/reports/printable_diary/", input$un, "?from=", min.date, "&to=" , max.date, sep="") 
      scrape = readHTMLTable(theurl, header=F)
      

      
      ######## Append Date ################################################
      tables = data.frame()
      for(i in 1:length(scrape)){
        
        if(ncol(scrape[[i]])==length(ns)){
          
          day = rep(date[i],nrow(scrape[[i]])) 
          temp =   data.frame(scrape[[i]], day ) 
          tables = rbind(tables, temp)
        } else{
          tables =tables
          
        }
        
      }
      
      tables = na.omit(tables)
      names(tables) = c(ns, "day")
      tables$day = as.Date(tables$day, "%Y-%m-%d")
      tables

    })
  })
  
  
    
getFood = reactive({
  output = dataInput()
  output  
    
})


getDrop = reactive({
  
  food = getFood()
  food.data =   cleanFood(food)  
  
  ff = aggregate(x =food.data$quant , by = list( food.data$day, food.data$food.type.Food, food.data$unit  ), FUN=sum) 
  names(ff) = c("date","food", "unit", "quantity")
  

  if( length( which(  as.character(ff[,2]) %in%  input$foodName)    == T) > 0)   {
    
    f.out = c(input$foodName, sort(as.character(ff[,2][-which(as.character(ff[,2])==input$foodName)])))
    
  } else{
    
    
    f.out = as.character(ff[,2])
  }
    
  
})





output$main_plot <- renderPlot({
  # Get Data 
  
  food = getFood()
  tables1 = food 
  
#  par(mfrow=c(1,2))
  
  date = seq(from = input$dates[1], to = input$dates[2], by=1) 
  

  tables = tables1[as.character(tables1$day) %in% as.character(date) ,]
  
  foodIn = foodOut(tables)
  
  food.out  = foodIn[as.character(foodIn$day) %in% as.character(date), ]

  
  food.data =   cleanFood(tables)   
  
  
  # Bar Plot
  
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
  
  bp = barplot(iv1$quantity, names.arg= format(iv1$date, "%b-%d"),               
               
               , xlab="Dates", ylab=as.character(unique(iv$unit)))
title(  main=
    paste(
      paste("Consumption of", food.query ,":\n", sum(iv1$quantity), unique(iv$unit)[1],  sep=" "), 
      paste(format(min(date), "%b-%d"), "to",  format(max(date), "%b-%d"), sep=" "),
      sep = "\n"
    ) , cex.main=.85)
  
  legend('topright', pch=c(22,NA), pt.bg=c("gray", "red"), lty=c(NA,2),
         legend=c(as.character(unique(iv$unit)[1]), "  Trend"), inset=c(-.1,0), col=c("gray", "red") )
  
  lines(lowess(bp, iv1$quantity), col="red", lty=2)
  # abline(reg=lm(iv1$quantity~bp), col="red", lty=2)
  
  
  

  
#   
#   # Plot Macro Pie Chart
#   
#   par(mar=c(3.1, 3.1, 3.1, 3.1), xpd=TRUE) 
#   macros = c(mean(food.out$carbs)-mean(food.out$fiber), mean(food.out$fiber), mean(food.out$fat), mean(food.out$protein))
#   lbls = c("Carbs", "Fiber",  "Fat","Protein")
#   pie3D(macros,labels=lbls,explode=0.1,  main=paste("Average Calories:", round(mean(food.out$calories),0), "\n from" ,format(min(date), "%b-%d"), "to",  format(max(date), "%b-%d"), sep=" "), col=c("darkgoldenrod2", "chartreuse4", "brown4", "blue3" ))
#   legend('bottomleft', pt.bg=c("darkgoldenrod2", "chartreuse4", "brown4", "blue3" ), pch=c(22,22,22,22), 
#          legend=paste(lbls, ":",  round(macros,0), "grams", sep=" "), inset=c(.05,.05))
#   
  
  
 
  
  
})





output$main_plot1 <- renderPlot({
  # Get Data 
  
  food = getFood()
  tables1 = food 
  
#  par(mfrow=c(1,2))
  
  date = seq(from = input$dates[1], to = input$dates[2], by=1) 
  tables = tables1[as.character(tables1$day) %in% as.character(date) ,]
  foodIn = foodOut(tables)
  food.out  = foodIn[as.character(foodIn$day) %in% as.character(date), ]
  food.data =   cleanFood(tables)   
  
  
  
  
  
 # Plot Macro Pie Chart

if(input$plotType1 == "p"){
  
  par(mar=c(3.1, 3.1, 3.1, 3.1), xpd=TRUE) 
  macros = c(mean(food.out$carbs)-mean(food.out$fiber), mean(food.out$fiber), mean(food.out$fat), mean(food.out$protein))
  lbls = c("Carbs", "Fiber",  "Fat","Protein")
  pie3D(macros,labels=lbls,explode=0.1,  main=paste("Average Calories:", round(mean(food.out$calories),0), "\n from" ,format(min(date), "%b-%d"), "to",  format(max(date), "%b-%d"), sep=" "), col=c("darkgoldenrod2", "chartreuse4", "brown4", "blue3" ))
  legend('bottomleft', pt.bg=c("darkgoldenrod2", "chartreuse4", "brown4", "blue3" ), pch=c(22,22,22,22), 
         legend=paste(lbls, ":",  round(macros,0), "grams", sep=" "), inset=c(.05,.05))
  
  
} else{
  
  dat = food.out[,c(3,6,5,4)]
  dat$carbs = dat$carbs*4
  dat$fiber = dat$fiber*2
  dat$protein = dat$protein*4
  dat$fat = dat$fat*9
  
  
  par(mar=c(3.1, 3.1, 3.1, 8.1), xpd=TRUE) 
  lbls = c("Carbs", "Fiber", "Protein", "Fat") 
  barplot(t(as.matrix(dat)), col=c("darkgoldenrod2", "chartreuse4", "brown4", "blue3"), cex.axis=.7, cex.names=.7, ylab="Calories", names.arg=format(food.out$day,"%b-%d"))
  title(main=paste(round(mean(food.out$calories),0)," Calories from Macros:", "\n from" ,format(min(food.out$day), "%b-%d"), "to",  format(max(food.out$day), "%b-%d"), sep=" "))
  legend('topright', pt.bg=c("darkgoldenrod2", "chartreuse4", "brown4", "blue3","red" ), col=c("darkgoldenrod2", "chartreuse4", "brown4", "blue3","red" ), lty = c(NA,NA,NA,NA,2) , pch=c(22,22,22,22,NA),
         legend=c(lbls, "Average"), inset=c(-.25,-.05))
  abline(h=mean(food.out$calories), lty=2, col="red")
}
  
  
  
})




output$mytable5 = renderDataTable({ 
 
  food.data =   cleanFood(getFood())  
  
  ro0 =   aggregate(x =food.data$quant , by = list( food.data$food.type.Food, food.data$unit  ), FUN=sum)
  ro1 =   aggregate(x =food.data$quant , by = list( food.data$food.type.Food  ), FUN=length)
  ro = merge(ro0, ro1, by="Group.1")
  
  ro = data.frame(ro[,1], round(ro[,3],1), ro[,2], ro[,4])
  names(ro) = c("Food",  "Quantity","Unit", "Frequency")
  ro[order(ro$Frequency, decreasing=T),]
  
  
}, options = list(aLengthMenu = c(5, 20, 30), iDisplayLength = 5))

output$selectUI <- renderUI({ 
  
  out.food = unique(getDrop())
  
  selectInput("foodName", "Select your choice", out.food) 
  

})

})