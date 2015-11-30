#Returns a vector with amount of ink for each row.
densityFunc <- function(table){
  
amountOfInk <- vector()
i = 0
for(row in 1:42000)
{
  i = i + 1
  ink = 0
  for(col in 2:785)
  {
    ink = ink + as.numeric(table[row,col])
  }
  
  amountOfInk[i] <- ink
}

return(amountOfInk)
}