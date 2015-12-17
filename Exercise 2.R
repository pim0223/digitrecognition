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

#calculates vertical passes per column.
horizontalPasses <- function(table, nobs, nvars)
{
  dim <- sqrt(nvars);
  horizontalPasses <- matrix(nrow = nobs, ncol = dim);
  
  for(ob in 1: nobs)
  {
    for (x in 1 : dim)
    {
      pass <- 0;
      prev <- 0;
      for (y in 1 : dim)
      {
        index <- (x - 1) * dim + y + 1
        cur <- as.numeric(table[ob, index])
        if(prev < 128 && cur >= 128)
          pass <- pass + 1;
        prev <- cur;
      }
      horizontalPasses[ob, x] <- pass;
    }
  }
  return(as.data.frame(horizontalPasses));
}


#return number of horizontal passes
totalHorizontalPasses <- function(table, nobs, nvars)
{
  hPass <- horizontalPasses(table, nobs, nvars);
  return(apply(hPass, 1, sum));
}

#calculates vertical passes per column.
verticalPasses <- function(table, nobs, nvars)
{
  dim <- sqrt(nvars);
  verticalPasses <- matrix(nrow = nobs, ncol = dim);
  
  for(ob in 1: nobs)
  {
    for (x in 1 : dim)
    {
      pass <- 0;
      prev <- 0;
      for (y in 0 : (dim - 1))
      {
        index <- x  + y * dim + 1
        cur <- as.numeric(table[ob, index])
        if(prev < 128 && cur >= 128)
          pass <- pass + 1;
        prev <- cur;
      }
      verticalPasses[ob, x] <- pass;
    }
  }
  return(as.data.frame(verticalPasses));
}

#calculates amount of vertical passes.
totalVerticalPasses <- function(table, nobs, nvars)
{
  vPass <- verticalPasses(table, nobs, nvars);
  return(apply(vPass, 1, sum));
}


#multinoms and prediction (done over whole dataset)
predictions <- function()
{
  AOI.multinom <- multinom(label ~ AOI, mnist)
  AOI.multinom.pred <- predict(AOI.multinom, mnist[,-c(1)])
  AOI.multinom.table <- table(mnist[,1], AOI.multinom.pred)
  
  scaledAOI.multinom <- multinom(label ~ scaledAOI, mnist)
  scaledAOI.multinom.pred <- predict(scaledAOI.multinom, mnist[,-c(1)])
  scaledAOI.multinom.table <- table(mnist[,1], scaledAOI.multinom.pred)
  
  horPass.multinom <- multinom(label ~ horPass, mnist)
  horPass.multinom.pred <- predict(horPass.multinom, mnist[,-c(1)])
  horPass.multinom.table <- table(mnist[,1], horPass.multinom.pred)
  
  verPass.multinom <- multinom(label ~ verPass, mnist)
  verPass.multinom.pred <- predict(verPass.multinom, mnist[,-c(1)])
  verPass.multinom.table <- table(mnist[,1], verPass.multinom.pred)
  
  Passes.multinom <- multinom(label ~ horPass * verPass, mnist)
  Passes.multinom.pred <- predict(Passes.multinom, mnist[,-c(1)])
  Passes.multinom.table <- table(mnist[,1], Passes.multinom.pred)
  
  PassAOI.multinom <- multinom(label ~ horPass * verPass * scaledAOI, mnist, maxit=1000)
  PassAOI.multinom.pred <- predict(PassAOI.multinom, mnist[,-c(1)])
  PassAOI.multinom.table <- table(mnist[,1], PassAOI.multinom.pred)
  
  passPerRow.multinom <- multinom(label ~ ., mnist[,c(1,790:845)], maxit = 1000)
  passPerRow.multinom.pred <- predict(passPerRow.multinom, mnist[,-c(1)])
  passPerRow.multinom.table <- table(mnist[,1], passPerRow.multinom.pred)
}

