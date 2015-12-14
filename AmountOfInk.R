#Returns a vector with amount of ink for each row.
#input: table, amount of observations and amount of pixels.
amountOfInk<- function(table, nobs, nvars){
  
amountOfInk <- vector() #the returnvector with amount of ink
i = 0 #index of vector
for(row in 1:nobs)
{
  i = i + 1
  ink = 0
  for(col in 2:nvars)
  {
    ink = ink + as.numeric(table[row,col])
  }
  
  amountOfInk[i] <- ink
}

return(amountOfInk)
}

#return number of horizontal passes
horizontalPasses <- function(table, nobs, nvars)
{
  horizontalPasses <- vector()
  for(row in 1:nobs)
  {
    pass = 0;
    prev = 0;
    dim = sqrt(nvars);
    
    for (col in 2: (nvars + 1))
    {
      if ((col - 2) %% dim == 0)
        prev = 0;
      cur = as.numeric((table[row,col]))
      if(prev < 128 && cur >= 128)
        pass = pass + 1;
      prev = cur;
    }
    horizontalPasses[row] = pass;
  }
  return(horizontalPasses);
}

#calculates amount of vertical passes.
verticalPasses <- function(table, nobs, nvars)
{
  verticalPasses <- vector();
  for(row in 1:nobs)
  {
    pass <- 0;
    dim <- sqrt(nvars);
    
    for (x in 1 : dim)
    {
      prev <- 0;
      for (y in 0 : (dim - 1))
      {
        index <- x  + y * dim + 1
        cur <- as.numeric(table[row, index])
        if(prev < 128 && cur >= 128)
          pass <- pass + 1;
        prev <- cur;
      }
    }
    verticalPasses[row] <- pass
  }
  return(verticalPasses);
}

makeAOImultinom <- function(data)
{
  data.AOImultinom <- multinom(label ~ AOI, data)
  return(data.AOImultinom)
}

#make a multinom of scaled density to label given data.
makeScaledAOImultinom <- function(data)
{
  data.ScaledAOImultinom <- multinom(label ~ scaledAOI, data)
  return(data.ScaledAOImultinom)
}

makeHorPassmultinom <- function(data)
{
  data.HorPassmultinom <- multinom(label ~ horPass, data)
  return(data.HorPassmultinom)
}

#make a prediction of data given a multinom function and data.
predict <- function(multinom, data){
  data.pred <- predict(multinom, data[,-c(1)])
  return(data.pred)
}