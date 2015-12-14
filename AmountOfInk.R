calculateFeatures <- function()
{
  mnist$AOI <- amountOfInk(mnist, 42000, 784)
  mnist$scaledAOI <- scale(mnist$AOI)
  mnist$horPass <- horizontalPasses(mnist, 42000, 784)  
  mnist$verPass <- verticalPasses(mnist, 42000, 784)
}

#Returns a vector with amount of ink for each row.
#input: table, amount of observations and amount of pixels.
amountOfInk<- function(table, nobs, nvars)
{
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


#multinoms and prediction (done over whole dataset)
AOI.multinom <- multinom(label ~ AOI, mnist)
AOI.multinom.pred <- predict(AOI.multinom, mnist[,-c(1)])
AOI.multinom.table <- table(mnist[,1], AOI.multinom.pred)

ScaledAOI.multinom <- multinom(label ~ scaledAOI, mnist)
ScaledAOI.multinom.pred <- predict(ScaledAOI.multinom, mnist[,-c(1)])
ScaledAOI.multinom.table <- table(mnist[,1], ScaledAOI.multinom.pred)

HorPass.multinom <- multinom(label ~ HorPass, mnist)
HorPass.multinom.pred <- predict(HorPass.multinom, mnist[,-c(1)])
HorPass.multinom.table <- table(mnist[,1], HorPass.multinom.pred)

VerPass.multinom <- multinom(label ~ VerPass, mnist)
VerPass.multinom.pred <- predict(VerPass.multinom, mnist[,-c(1)])
VerPass.multinom.table <- table(mnist[,1], VerPass.multinom.pred)

Passes.multinom <- multinom(label ~ HorPass * VerPass, mnist)
Passes.multinom.pred <- predict(Passes.multinom, mnist[,-c(1)])
Passes.multinom.table <- table(mnist[,1], Passes.multinom.pred)

PassAOI.multinom <- multinom(label ~ HorPass * VerPass * scaledAOI, mnist)
PassAOI.multinom.pred <- predict(PassAOI.multinom, mnist[,-c(1)])
PassAOI.multinom.table <- table(mnist[,1], PassAOI.multinom.pred)

