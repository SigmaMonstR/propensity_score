#Propensity score greedy match
#Syntax
##a + b are dataframes containing a unique ID and probability to be matched (e.g. df1, df2)
##var is the variable containing probabilities (e.g. "prob")
##min and max are the level of precision as integers. max dictates the maximum precision whereas min is the minimum acceptable precision
##id is the ID variable in both a + b. Note that the ID var should be labeled the same in both dataframes.

psm <- function(a,b,var,max,min,id){
  
  matched <- data.frame()
  
  for(k in max:min){
    a[,var]<-substr(a[,var],1,k+2)
    b[,var]<-substr(b[,var],1,k+2)
    
    for(j in 1:nrow(a)){
      
      target <- a[j,]
      target <- merge(target,b,by=var)
    
    if(nrow(target)>0){
     # print("if")
      target <- target[1:1,]
      matched<-rbind(matched,cbind(target,k))
      listx <- matched[,paste(id,".x",sep="")]
      listy <- matched[,paste(id,".y",sep="")]
      
      a <- a[ !a[,id] %in% listx, ]
      b <- b[ !b[,id] %in% listy, ]
      
    }
    
    rm(target)
    }
  }
  return(matched)
}
