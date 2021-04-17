install.packages("plotrix")
install.packages("scales")
library(plotrix)
library(scales)

find_pi = function(B = 5000, seed = 10, make_plot = TRUE){
  # Control seed
  set.seed(seed)
  
  # generate the x and y co-ordinates from uniform
  # distribution taking values from -1 to 1
  x<-runif(B, min=-1, max=1)
  y<-runif(B, min=-1, max=1)
  
  # Distance of the points from the center (0,0)
  z<-sqrt(x^2+y^2)

  # radius=1
  hat_pi = 4*sum((z<=1))/length(z)
  
  if (make_plot == "TRUE"){
  plot(x,y,
       main="Estimate PI with Monte Carlo",
       xlim = c(-1,1),
       ylim = c(-1,1),
       col = ifelse(z<=1, alpha("blue", .4), alpha("red", .4)),
       cex = 1,
       pch = 16,
       asp = 1,
       grid())
  rect( -1, -1, 1, 1) 
  draw.circle( 0, 0, 1 )
  }else{
  }
  return(hat_pi)
}


find_pi(make_plot = TRUE)

