#Returns a vector with amount of ink for each row.
densityFunc <- function(table){
  
amountOfInk <- vector()
i = 0
for(row in 1:nrow(table))
{
  i = i + 1
  ink = 0
  #misschien 2:ncol of params?
  for(col in 2:785)
  {
    ink = ink + as.numeric(table[row,col])
  }
  
  amountOfInk[i] <- ink
}

return(amountOfInk)
}

#make a multinom of scaled density to label given data.
makeMultinom <- function(data){
  data.multinom <- multinom(label ~ scaledDensity, data)
  return(data.multinom)
}

#make a prediction of data given a multinom function and data.
predict <- function(multinom, data){
  data.pred <- predict(multinom, data[,-c(1)])
  return(data.pred)
}