goal = 2800
min.date = Sys.Date()-10
max.date = Sys.Date()
date = seq(as.Date(min.date, "%Y-%m-%d"), as.Date(max.date, "%Y-%m-%d"), by=1)

theurl = paste("http://www.myfitnesspal.com/reports/printable_diary/", "s2konstantine", "?from=", date[1], "&to=" , date[1], sep="") 
scrape = readHTMLTable(theurl, header=F)

ns = c("Foods" ,   "Calories" ,"Carbs" ,   "Fat"     , "Protein"  ,"Cholest" , "Sodium"  , "Sugars" ,  "Fiber"  ) 

theurl = paste("http://www.myfitnesspal.com/reports/printable_diary/", "s2konstantine", "?from=", min.date, "&to=" , max.date, sep="") 
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

if( !(max(tables$day) >= Sys.Date()-1) ) { print("Not Enough Data to Use Historical Data")

                                    
} else {

# Monte Carlo Algorithm to Find Food Reccomendations 
  goal = 2800

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
macros.needed = compare - today

if(macros.needed$calories<0) print("Above Expected Calories")


foodCompare = tables[which(tables$day==compare$day),]

#cfc = foodCompareOut(foodCompare)
#macro = macros.needed[,-c(1:2)]  


cfc = unique(foodCompareOut(foodCompare))
rn = cfc[,1]
cfc = cfc[,-1]
row.names(cfc) = rn

if(today$calories < 1000) {

 
  if(sum(today[-1])==0){
reEat = cfc[1:floor(.5*nrow(cfc)),] 
today = colSums(reEat)
macros.needed = compare[-1] - today
macroCalories = macros.needed[,1]  
macro = macros.needed[,-c(1)]   

} else {
  reEat = foodCompareOut(tables[which(tables$day == Sys.Date()),])
  rn = reEat[,1]
  reEat = reEat[,-1]
  row.names(reEat) = rn
  reEat = rbind(reEat, cfc[1:floor(.5*nrow(cfc)),]  )
  today = colSums(reEat) 
  
  macros.needed = compare[-1] - today
  macroCalories = macros.needed[,1]  
  macro = macros.needed[,-c(1)]   

}



} else{
  macroCalories = macros.needed[,2]  
  macro = macros.needed[,-c(1,2)]  
  
}  
# set the food equal to part of yesterday 


repeat {
 

  
#macroCalories = macros.needed[,2]  
#macro = macros.needed[,-c(1,2)]  

mc= median(cfc$calories)

ms =  max(2,floor(macroCalories/mc))

ms = min(17, ms)

sets = combn(x=sample(1:nrow(cfc), size= ms+3,rep=T),m= ms-1)
setMacros = data.frame(matrix(0,nrow(sets),4) )
for(i in 1:ncol(sets)){
  test = colSums(cfc[sets[,i],])[-1]
  setMacros[i,] = test
    
}
names(setMacros) = names(test)

cfc2 = data.frame(matrix(0,nrow(setMacros),ncol(setMacros)))
row.names(cfc2) = rep(1:nrow(setMacros))

for(i in 1:ncol(setMacros)){
  cfc2[,i] = rep(as.numeric(macro[i] ), nrow(setMacros)) - setMacros[,i]  
  
}
names(cfc2) = names(test)



dat = which.min(rowSums(abs(cfc2)))[1]

if( sum(abs(cfc2[dat,])) < 40 ) break 

}

out = sets[,dat]
outFood = cfc[out,]

macro - colSums(outFood[,-1])

} # end else

rec = rbind( reEat,outFood)
