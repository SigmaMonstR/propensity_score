
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
  
  
  #placeholder
  matched <- data.frame()
  
  #breakage
  unis <- unique(b[[var]])
  rand <- round(4*runif(nrow(b)))
  b0 <- b[b[[var]] %in% unis[rand ==0], ]
  b1 <- b[b[[var]] %in% unis[rand ==1], ]
  b2 <- b[b[[var]] %in% unis[rand ==2], ]
  b3 <- b[b[[var]] %in% unis[rand ==3], ]
  b4 <- b[b[[var]] %in% unis[rand ==4], ]
  
  #loop
  for(k in max:min){
    #print(paste0("length = ", k))
    
    if(nrow(a) >0){
      #Round match value
      a$round <- round(a[,var], k)
      b0$round <- round(b0[,var], k)
      b1$round <- round(b1[,var], k)
      b2$round <- round(b2[,var], k)
      b3$round <- round(b3[,var], k)
      b4$round <- round(b4[,var], k)
      
      #merge by rounded value
      c0 <- merge(a[,c("round",id)], b0[,c("round",id)], by = "round")
      c1 <- merge(a[,c("round",id)], b1[,c("round",id)], by = "round")
      c2 <- merge(a[,c("round",id)], b2[,c("round",id)], by = "round")
      c3 <- merge(a[,c("round",id)], b3[,c("round",id)], by = "round")
      c4 <- merge(a[,c("round",id)], b4[,c("round",id)], by = "round")
      
      c <- rbind(c0,c1,c2,c3,c4)
      
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
