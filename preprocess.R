setwd("C:/Users/DELL/uni/pr/digitrecognition")

# Load in data
if(!exists("mnist")){
  mnist   = read.csv("mnist.csv")
}

labels  = mnist[,1]
mnist28 = mnist[,-1]

num_obs = dim(mnist28)[1]

reducedf = function(df, g){
  # Inputs are data-frame and desired granularity
  num_pix = dim(mnist28)[2]
  
  # Flatten (horizontally)
  for(i in 1:(num_pix/g)){
    new_col = matrix(rep(0, num_obs), nrow = num_obs)
    for(j in 1:g){
      new_col = new_col + df[,(i-1)*g + j]
    }
    df[,i] = new_col
  }
  
  # Flatten (vertically)
  for(i in 1:(num_pix/g^2)){
    new_col = matrix(rep(0, num_obs), nrow = num_obs)
    for(j in 1:g){
      new_col = new_col + df[,i + (i%/%(sqrt(num_pix)))*28 + (j-1)*sqrt(num_pix)]
    }
    df[,i] = new_col 
  }
  
  df = df[,1:(num_pix/g^2)]
  df[,i] = df[,i]/g^2

  return(df)
}

# Plot digits
plot_digits = function(df, num){
  num_pix = dim(df)[2]
  for(i in 1:num){
    png(filename = paste(toString(i),"_", toString(num_pix), ".png", sep = ""))
    image(matrix(as.numeric(df[i,]),
                 nrow = sqrt(num_pix), ncol = sqrt(num_pix)),
                 xaxt = 'n', yaxt = 'n',
                 ylim = (1:0))
    dev.off()
  }
}

mnist14 = reducedf(mnist,2)
mnist7 = reducedf(mnist,4)
mnist4 = reducedf(mnist,7)
mnist2 = reducedf(mnist,14)

plot_digits(mnist28,10)
plot_digits(mnist14,10)
plot_digits(mnist7,10)
plot_digits(mnist4,10)
plot_digits(mnist2,10)