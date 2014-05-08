

####### Functions#####################################


calc = function(x){
  if(length(x)>0) {
    x1 = c()
    for(i in 1:length(x)){
      # for(i in 240){
      
      
      t=   try(eval(parse(text=x[i])) , silent=T)
      if("try-error" %in% class(t) || is.null(t)==T) {
        
        
        
        v  = strsplit(x[i], " ")
        
        v = v[[1]][which(nchar(v[[1]]) != 0)]
        
        if(length(v)>0){
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
          
          x1[i] = 0
          
        }
      } else{
        
        x1[i] =   eval(parse(text=x[i])) 
        
      }
      #print(i)
      
    } # End For
    
  } else{
    
    x1 = 0
  }
 # print(i)
  
  return(x1)
} 


######### Libraries
library(shiny)
library(plotrix)
library(xts)
library(RCurl)
library(XML)
#library(markdown)


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
# 
# 


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






recFood = function(tables, goal){
  
  if( !(max(tables$day) >= Sys.Date()-1) ) { print("Not Enough Data to Use Historical Data")
                                             
                                             
  } else {
    goal= goal
    
    # Monte Carlo Algorithm to Find Food Reccomendations 
   # goal = 2800
    
    foodIn = foodOut(tables)
    
    # compare to similar day in query
    
    today = foodIn[which(foodIn$day==Sys.Date()),]
    
    if(nrow(today)==0){
      today = rep(0, ncol(foodIn))
      names(today) = names(foodIn)
      foodIn = foodIn
    } else{
      
      foodIn = foodIn[-which(foodIn$day==Sys.Date()),]
    }
    
    compare = foodIn[which.min(abs(foodIn$calories - goal)),]
    
    macros.needed = round(compare[-1]/(compare$calories/goal),0) - today[-1]
   #macros.needed = compare - today
    
    if(macros.needed$calories<0) print("Above Expected Calories")
    
    
    foodCompare = tables[which(tables$day==compare$day),]
    
    #cfc = foodCompareOut(foodCompare)
    #macro = macros.needed[,-c(1:2)]  
    
    
    cfc = unique(foodCompareOut(foodCompare))
    rn = cfc[,1]
    cfc = cfc[,-1]
    row.names(cfc) = rn
    
   if(goal>3000) spike = .8 # Tuning Parameters to say how much of the compare days food to eat if not eaten at all 
   if(goal<=3000 & goal>2000)  spike = .6
   if(goal <= 2000 & goal>1600) spike = .5
   if(goal <= 2000 & goal>1600) spike = .5
   if(goal <=1600) spike = .4
   
    if(today[which(names(today)=="calories")] < goal* spike) {
      
      reEat1 = data.frame()
      if(sum(today[-1])==0){
        reEat1 = cfc[1:floor(.5*nrow(cfc)),] 
        today = colSums(reEat1)
        macros.needed = round(compare[-1]/(compare$calories/goal),0) - today
        # macros.needed = compare[-1] - today
        macroCalories = macros.needed[,1]  
        macro = macros.needed[,-c(1)]   
        
      } else {
        reEat1 = foodCompareOut(tables[which(tables$day == Sys.Date()),])
        rn = reEat1[,1]
        reEat1 = reEat1[,-1]
        rnew = paste(rn[which(  rn ==  names(which(table(droplevels(rn))>1)))],  1:length(rn[which(  rn ==  names(which(table(droplevels(rn))>1)))]),sep="")
        rg = as.character(droplevels(rn))
        rg[which(  rn ==  names(which(table(droplevels(rn))>1)))] = as.character(rnew)
        
        row.names(reEat1) = rg
        reEat = rbind(reEat1, cfc[1:floor(.5*nrow(cfc)),]  )
        today = colSums(reEat) 
        
        macros.needed = round(compare[-1]/(compare$calories/goal),0) - today
       # macros.needed = compare[-1] - today
        macroCalories = macros.needed[,1]  
        macro = macros.needed[,-c(1)]   
        
      }
      
      
      
    } else{
      macroCalories = macros.needed[,1]  
      macro = macros.needed[,-c(1)]   ## to far from goal 
      
    }  
    # set the food equal to part of yesterday 
    
    count =1 
    upper = 0 
    repeat {
      
      
      
      #macroCalories = macros.needed[,2]  
      #macro = macros.needed[,-c(1,2)]  
      
      mc= median(cfc$calories)
      
      ms =  max(2,floor(macroCalories/mc))
      
      if(ms>30){  # if lots of food is needed (user has not eaten today...prefer higher calorie foods!!) 
        
        cfc = cfc[sample(1:nrow(cfc),size=nrow(cfc), rep=T,prob=cfc$calories),]
      }
      
      ms = min(17, ms)
      
      
      
      sets = combn(x=sample(1:nrow(cfc), size= ms+3,rep=T),m= ms-1)
      setMacros = data.frame(matrix(0,nrow(sets),4) )
      for(i in 1:ncol(sets)){
        test = colSums(cfc[sets[,i],])[-1]
        setMacros[i,] = test
        
      } # End for
      names(setMacros) = names(test)
      
      cfc2 = data.frame(matrix(0,nrow(setMacros),ncol(setMacros)))
      row.names(cfc2) = rep(1:nrow(setMacros))
      
      for(i in 1:ncol(setMacros)){
        cfc2[,i] = rep(as.numeric(macro[i] ), nrow(setMacros)) - setMacros[,i]  
        
      }  # End for
      names(cfc2) = names(test)
      
      
      
      dat = which.min(rowSums(abs(cfc2)))[1]
      
      if( sum(abs(cfc2[dat,])) < 40 + upper ) break 
      
      count = count + 1
      print(paste(count,"-",upper))
    
      if(ncol(sets)<800) mover = 3  # Tuning Parameters 
      if(ncol(sets)>=800 & ncol(sets)< 2000) mover = 5
      if(ncol(sets>=2000)) mover = 100
      
      if(count%%3==0) {  upper = upper + mover }  # Break if taking to long 
      if(upper>9) break
      if(ncol(sets)>1200 & count>1) break
      
    } # end Repeat 
    
    out = sets[,dat]
    outFood = cfc[out,]
    
    macro - colSums(outFood[,-1])
    
  } # end else
  
  if(nrow(reEat1)>0){
    rec = rbind( reEat1,outFood)
    
  } else{
    
    rec = outFood
  }
  
  rec = rbind( reEat1,outFood)
  recOut = rec
  total = colSums(recOut)
  recOut = rbind(recOut, total)
  recOut = data.frame(c(row.names(recOut)[-length(row.names(recOut))], "Total"), recOut) 
  names(recOut)[1] = "Food"
  
  return(recOut)
  
} # End recFood 

#goal = 5000
#recFood(tables,goal)


foodCompareOut = function(tables){
  # in
  food = tables
  
  Foods = food$Foods
  calories = as.numeric(gsub(",","",food$Calories))
  carbs = as.numeric(gsub("[A-z]", "", food$Carbs) )
  fat = as.numeric(gsub("[A-z]", "", food$Fat) )
  protein = as.numeric(gsub("[A-z]", "", food$Protein) )
  fiber = as.numeric(gsub("[A-z]", "", food$Fiber) )
  
  temp = data.frame(Foods,calories, carbs, fat, protein, fiber) 
  
  
  
  return(temp)
}

