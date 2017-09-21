
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

  matched <- data.frame()
  
  for(k in max:min){
    print(paste0("length = ", k))
    
      if(nrow(a) >0){
        #Round match value
        a$round <- round(a[,var], k)
        b$round <- round(b[,var], k)
        
        #merge by rounded value
        c <- merge(a[,c("round",id)], b[,c("round",id)], by = "round")
        c$rank <- ave(c$id.x, c$round,FUN = function(x){
          r <- rank(x,ties.method = 'random')
          as.numeric(factor(rank(sort(r))))[r]
        })
        
        #Sort through matches
        c <- c[c$rank <= nn, ]
        c$precision <- k
        matched <- rbind(matched, c)
        
        #Drop records that have been matched
        a <- a[!(a$id %in% unique(c$id.x)),]
        #b <- b[!(b$id %in% unique(c$id.y)),]
        rm(c)
      }
  }
  colnames(matched) <- c("pr","a_id","b_id","order_match","precision")
  return(matched)
}
