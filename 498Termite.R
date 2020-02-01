distancesq <- function(row1, row2){
  return((row1$x - row2$x)^2 + (row1$y - row2$y)^2)
}

forage2 <- function(x0, y0){
  rolls <- data.frame(x = c(rep(1, 5), rep(2, 5), rep(3, 5), rep(4, 5), rep(5,5)),
                      y = rep(c(1,2,3,4,5), 5), attacked = rep(FALSE, 25))
  
  attacked_index = ((x0 - 1)*5 + y0)
  rolls[attacked_index,]$attacked = TRUE 
  
  
  index <- 1:25
  index <- index[-attacked_index]
  
  results <- data.frame(first = rep(NA, 625), second = rep(NA, 625), T_2 = rep(NA, 625))
  
  for(i in index){
    for(j in index){
      if(i == j) next() #cant forrage same roll twice
      dist1 <- distancesq(rolls[i,], rolls[attacked_index,])
      dist2 <- min(distancesq(rolls[j,], rolls[attacked_index,]), distancesq(rolls[j,], rolls[i,]))
      T_2 <- dist1 + dist2
      index_result <- (i-1)*(25) + j
      results[index_result,] <- c(i, j, T_2)
    }
  }
  results <- na.omit(results) # not the most elegant thing i've ever written
  
  a <- nrow(results) # answer to a
  b <- length(which(results$T_2 == 2)) #answer to b
  c <- mean(results$T_2 <= 2) #answer to c
  return(c(a, b, c))
}

forage2(3,3) # First Q
forage2(1,1) # Second Q
