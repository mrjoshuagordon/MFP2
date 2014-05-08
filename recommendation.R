
min.date = Sys.Date()-7
max.date = Sys.Date()
date = seq(as.Date(min.date, "%Y-%m-%d"), as.Date(max.date, "%Y-%m-%d"), by=1)

theurl = paste("http://www.myfitnesspal.com/reports/printable_diary/", "zj95maxx", "?from=", date[1], "&to=" , date[1], sep="") 
scrape = readHTMLTable(theurl, header=F)

ns = c("Foods" ,   "Calories" ,"Carbs" ,   "Fat"     , "Protein"  ,"Cholest" , "Sodium"  , "Sugars" ,  "Fiber"  ) 

theurl = paste("http://www.myfitnesspal.com/reports/printable_diary/", "zj95maxx", "?from=", min.date, "&to=" , max.date, sep="") 
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


foodIn = foodOut(tables)

# compare to similar day in query

today = foodIn[which(foodIn$day==Sys.Date()),]
compare = foodIn[which(foodIn$calories == median(foodIn$calories))[1],]
macros.needed = compare - today


foodCompare = tables[which(tables$day==compare$day),]
cfc = foodCompareOut(foodCompare)
macro = macros.needed[,-c(1:2)] 

suggestion = data.frame()
i=1
while(any(macro>5) == TRUE) {
  
  
  
repeat{ 
  foodChoice = cfc[sample(1:nrow(cfc),1,prob=c(1/1:nrow(cfc))),]
  check = macro - foodChoice[,-c(1:2)]   
  toEat = foodChoice 
  
if(any(check< -5 )==FALSE ) break

    } # End Repeat

suggestion= rbind(suggestion,toEat)
macro = macro - foodChoice[,-c(1:2)]  
cfc = cfc[-which(cfc$Foods == toEat$Foods),]
first = names(macro[which.max(macro)])  
cfc = cfc[order(cfc[,which(names(cfc)==first)], decreasing=T),] 

if(length(which(cfc[,names(cfc)==first]>macro[,names(macro)==first]))>0) {

cfc = cfc[-which(cfc[,names(cfc)==first]>macro[,names(macro)==first]),]
} else{
 cfc = cfc  
}
i=i+1
print(i)
}



  



test = cfc[,-c(1,2)]

which(




  
  
  
  
#   foodCompareOut = function(tables,macros.needed){
#     

















      
stay = FALSE
while(stay==FALSE) {

    
    
    
   
    
    food = foodCompare
    
    
    Foods = food$Foods
    calories = as.numeric(gsub(",","",food$Calories))
    carbs = as.numeric(gsub("[A-z]", "", food$Carbs) )
    fat = as.numeric(gsub("[A-z]", "", food$Fat) )
    protein = as.numeric(gsub("[A-z]", "", food$Protein) )
    fiber = as.numeric(gsub("[A-z]", "", food$Fiber) )
    
  food = data.frame(Foods, carbs, fat, protein, fiber) 
  temp = data.frame(food$Foods, food$carbs, food$fat, food$protein, food$fiber) 
  names(temp) = c("Foods", "carbs", "fat", "protein", "fiber") 
  temp1 = temp[sample(1:nrow(temp), 10, rep=T),]
nt = temp1[,1]
temp1= temp1[,-1]

if( length(which(colSums(temp1) > macro - 5) == T) <=1   & length(which(colSums(temp1) < macro+  5) == T) <=1 ) {
  stay = TRUE
  
  } else{
    
    stay = FALSE
  }

}

output = data.frame(nt, temp1)
names(output) = c("Foods", "carbs", "fat", "protein", "fiber") 



macro
colSums(output[,-1])

































#food = tables
    
for(i in 1:length(food)){

  if(macro$carbs>10) { first = "carbs" } else 
  if(macro$fat>10) { first = "fat" } else 
  if(macro$protein>5) { first = "protein" }
  if(macro$fiber>5) { first = "fiber" } else {
    
  
  
  }
    temp = data.frame(food$Foods, food$carbs, food$fat, food$protein, food$fiber) 
    names(temp) = c("Foods", "carbs", "fat", "protein", "fiber") 
      
    temp = unique(temp[order(temp[,which(names(temp)==first)], decreasing=T),])    
    eat = temp[sample(1:5,1),]
    macro = macro - eat[,-1]
    
   # food = food[-which(food$Foods == eat$Foods),]    
    
    
    suggestion = rbind(suggestion, eat)
      
      stay = ( macro$carbs > -5 && macro$carbs < 5  && macro$fat> - 5 && macro$fat<5 &&  macro$protein > -5 && macro$protein < 5 && macro$fiber > -5 && macro$fiber <5)
    if(stay == TRUE) break 
    print(i)
    }
    
macro$carb>10
#     
#     return(temp)
#   }
#  






cfc = foodCompareOut(foodCompare)

cfc1 = cfc[-c(1,2)]

macro = macros.needed[,-c(1:2)]  





  
cfc2 = as.data.frame(cfc[,1])
for(i in 1:ncol(cfc1)){
  cfc2 = cbind(cfc2, rep(as.numeric(macro[i] ), nrow(cfc1)) - cfc1[,i]  )
  
}
names(cfc2) = names(cfc)[c(1,3,4,5,6)]

# remove food that doesnt fit
cf1 = cfc2[-which(cfc2$carbs <=0 | cfc2$fat <=0 | cfc2$protein<= 0| cfc2$fiber<=0),]




