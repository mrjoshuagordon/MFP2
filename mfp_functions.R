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
