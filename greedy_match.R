
psm <- function(a,b,var,max,min,id, nn){
  # Desc: 
  #  Propensity score greedy match with replacement
  # 
  # Args:
  #   a + b are dataframes containing a unique ID and probability to be matched (e.g. df1, df2). df a should contain the treatment.
  #   var is a string of the variable containing probabilities (e.g. "prob")
  #   min and max are the level of precision as integers. max dictates the maximum precision whereas min is the minimum acceptable precision
  #   id is the ID variable in both a + b. Note that the ID var should be labeled the same in both dataframes.
  #   nn is the number of matched per record
  #   
  # Returns:
  #   Dataframe of matches 
  
  matched <- c()
  
  for(k in max:min){
    print(paste0("length = ", k))
    
    if(nrow(a) > 0){
      #Round match value
      a$round <- round(a[,var], k)
      b$round <- round(b[,var], k)
      
      for(rows in 1:nrow(a)){
        inq <- a$round[rows] 
        found <- grep(inq, b$round)
        if(length(found) >= nn){
          m1 <- c(a[rows, id], b[found[1:nn], id] )
          a <- a[!(a[[id]] %in% m1),]
          b <- b[!(b[[id]] %in% m1),]
          matched <- c(matched, m1)
        }
        
      }
    }
  }
  return(matched)
}
