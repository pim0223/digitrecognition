densityFunc <- function(table){
table <- mnist  
  
amountOfInk <- vector()

for(row in 2:1000)
{
  print(table[row,col])
  ink = 0
  for(col in 2:785)
  {
    ink = ink + table[row,col]
  }
  amountOfInk[row] <- ink
}

return(amountOfInk)
}