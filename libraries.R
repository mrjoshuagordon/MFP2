

####### Functions#####################################

calc = function(x){
  
  x1 = c()
  for(i in 1:length(x)){
    # for(i in 240){
    
    
    t=   try(eval(parse(text=x[i])) , silent=T)
    if("try-error" %in% class(t)) {
      
      
      
      v  = strsplit(x[i], " ")
      
      v = v[[1]][which(nchar(v[[1]]) != 0)]
      
      temp = c()
      for(j in 1:length(v)) {
        t1 =   try(eval(parse(text=v[j])) , silent=T)
        
        if("try-error" %in% class(t1)) { 
          v[j] = gsub("/", "0", v[j])
          v[j] = gsub(".", "0", v[j])
          temp[j] = sum(eval(parse(text=v[j])) )
        } else {       
          temp[j] = sum(eval(parse(text=v[j])) )
        }
        
        
      }
      
      x1[i] =  sum(temp)
      
    } else{
      
      x1[i] =   eval(parse(text=x[i])) 
      
    }
   # print(i)
    
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

# 
# getData = function(){
#   
#   
#   options(stringsAsFactors=TRUE)
#   
#   
#   min.date = "2014-04-20"
#   max.date = Sys.Date()
#   date = seq(as.Date(min.date, "%Y-%m-%d"), as.Date(max.date, "%Y-%m-%d"), by=1)
#   
#   theurl = paste("http://www.myfitnesspal.com/reports/printable_diary/s2konstantine?from=", date[1], "&to=" , date[1], sep="") 
#   scrape = readHTMLTable(theurl)
#   
#   ns = names(scrape[[1]])
#   
#   theurl = paste("http://www.myfitnesspal.com/reports/printable_diary/s2konstantine?from=", min.date, "&to=" , max.date, sep="") 
#   scrape = readHTMLTable(theurl, header=F)
#   
#   
#   ## add date back in *sigh*
#   
#   ######## Append Date ################################################
#   tables = data.frame()
#   for(i in 1:length(scrape)){
#     
#     if(ncol(scrape[[i]])==length(ns)){
#       
#       day = rep(date[i],nrow(scrape[[i]])) 
#       temp =   data.frame(scrape[[i]], day ) 
#       tables = rbind(tables, temp)
#     } else{
#       tables =tables
#       
#     }
#     
#   }
#   
#   tables = na.omit(tables)
#   names(tables) = c(ns, "day")
#   tables$day = as.Date(tables$day, "%Y-%m-%d")
#   return(tables)
#   
# }




#Daily Get Macros ##################################################

  
##### Split the data into food and quantity ########################################



cleanFood = function(tables){
  
  
  h = as.character(tables[,1])
  
  h1 = strsplit(h, ",") 
  Food = c()
  Quantity = c()
  for(i in 1:length(h)){
    Food[i] = h1[[i]][1] 
    Quantity[i] = h1[[i]][length(h1[[i]]) ]  
    
    
  }
  
  food.type = data.frame(Food, Quantity) 
  
  
  
  names(food.type) = c("Food", "Quantity")
  
  ######## Remove characters, fluff ####################################################
  
  food.type$Quantity = gsub("\\(.*\\)", "", food.type$Quantity )  
  food.type$Quantity =  sub("mL.*", "ml", food.type$Quantity)
  food.type$Quantity =  sub("grams.*", "grams", food.type$Quantity)
  unit = gsub("[./0-9]", "", food.type$Quantity) 
  #food.type$Quantity =  sub("[0-9] .*", "", food.type$Quantity)

  temp = gsub("[A-z]", "", food.type$Quantity)
  temp = gsub("(?!/)|(?!/.)[[:punct:]]", "", temp, perl=TRUE)
  
  options(scipen=999)
  quant = as.numeric(round(calc(temp),2))
  
  food.data = data.frame(food.type$Food,quant, unit)
  
  food.data = data.frame(food.data, tables[,-1]) 
  
  return(food.data)
  
}



foodOut = function(tables){
  
    food = tables
    
    date = food$day
    calories = as.numeric(gsub(",","",food$Calories))
    carbs = as.numeric(gsub("[A-z]", "", food$Carbs) )
    fat = as.numeric(gsub("[A-z]", "", food$Fat) )
    protein = as.numeric(gsub("[A-z]", "", food$Protein) )
    fiber = as.numeric(gsub("[A-z]", "", food$Fiber) )
    
    temp = data.frame(calories, carbs, fat, protein, fiber) 
    food.out = aggregate(temp, by=list(date), FUN=sum)
      
    names(food.out)[1] = "day"
  return(food.out)
}

