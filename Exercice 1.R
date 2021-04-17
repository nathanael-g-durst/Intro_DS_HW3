install.packages("plotrix")
library(plotrix)


find_pi = function(B = 50000, seed = 10, make_plot = TRUE){
  # Control seed
  set.seed(seed)
  
  # Simulate B points
  point = matrix(runif(2*B, -1, 1), B, 2)
  
  # generate the x1 and x2 co-ordinates from uniform
  # distribution taking values from -1 to 1
  x<-runif(n, min=-1, max=1)
  y<-runif(n, min=-1, max=1)
  
  # Distance of the points from the center (0,0)
  z<-sqrt(x^2+y^2)

  # radius=1
  hat_pi = 4*sum((z<=1))/length(z)
  
  InOut<-as.factor(ifelse(z<=1, "In", "Out"))
  
  plot(x,y,pch = 19, col=InOut, main="Estimate PI with Monte Carlo")
  
  plot(x,y,
       main="Estimate PI with Monte Carlo",
       xlim = c(-1,1),
       ylim = c(-1,1),
       col = InOut,
       cex = 1,
       pch = 19,
       asp = 1,
       grid())
  rect( -1, -1, 1, 1) 
  draw.circle( 0, 0, 1 )
  return(hat_pi)
}

find_pi(make_plot = TRUE)

