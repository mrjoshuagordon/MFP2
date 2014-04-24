

####### Functions#####################################

calc = function(x){
  
  x1 = c()
  for(i in 1:length(x)){
    
    
    t = try(eval(parse(text=x[i])) , silent=T)
    if("try-error" %in% class(t)) {
      
      v  = strsplit(x[i], " ")
      v = v[[1]][which(nchar(v[[1]]) != 0)]
      
      x1[i] =    sum(calc(v))
      
    } else{
      
      x1[i] =   eval(parse(text=x[i])) 
      
    }
    
    
  }
  
  return(x1)
} 



######### Libraries
library(shiny)
library(plotrix)
library(xts)
library(RCurl)
library(XML)

########## Get Data Function  ##############################


getData = function(){
  
  
  options(stringsAsFactors=TRUE)
  
  
  min.date = "2014-03-01"
  max.date = Sys.Date()
  date = seq(as.Date(min.date, "%Y-%m-%d"), as.Date(max.date, "%Y-%m-%d"), by=1)
  
  theurl = paste("http://www.myfitnesspal.com/reports/printable_diary/s2konstantine?from=", date[1], "&to=" , date[1], sep="") 
  scrape = readHTMLTable(theurl)
  
  ns = names(scrape[[1]])
  
  theurl = paste("http://www.myfitnesspal.com/reports/printable_diary/s2konstantine?from=", min.date, "&to=" , max.date, sep="") 
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
  return(tables)
  
}




#Daily Get Macros ##################################################

  
##### Split the data into food and quantity ########################################



cleanFood = function(tables){
  
  
  h = as.character(tables[,1])
  
  food.type = data.frame()
  
  for(i in 1:length(h)){
    temp = data.frame(strsplit(h[i], ",")[[1]][1], strsplit(h[i], ",")[[1]][length(strsplit(h[i], ",")[[1]]) ] )
    food.type = rbind(food.type, temp)  
    
  }
  
  
  
  names(food.type) = c("Food", "Quantity")
  
  ######## Remove characters, fluff ####################################################
  
  food.type$Quantity = gsub("\\(.*\\)", "", food.type$Quantity )  
  food.type$Quantity =  sub("mL.*", "ml", food.type$Quantity)
  food.type$Quantity =  sub("grams.*", "grams", food.type$Quantity)
  unit = gsub("[./0-9]", "", food.type$Quantity) 
  #food.type$Quantity =  sub("[0-9] .*", "", food.type$Quantity)

  temp = gsub("[A-z]", "", food.type$Quantity)
  temp = gsub("(?!/)[[:punct:]]", "", temp, perl=TRUE)
  
  quant =  calc(  temp )
  
  food.data = data.frame(food.type$Food,quant, unit)
  
  food.data = data.frame(food.data, tables[,-1]) 
  
  return(food.data)
  
}

