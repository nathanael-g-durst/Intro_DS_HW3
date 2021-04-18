# Requirements

## Check if installed, install it if not and load library
installedPackages <- installed.packages()

### Add here the packages needed ##############
packagesNeeded <- c("plotrix", "scales")
###############################################

for (packageName in packagesNeeded) {
  packageExists <- is.element(packageName, installedPackages)
  if(packageExists != TRUE){
    install.packages(packageName)
    library(packageName, character.only = TRUE)
    print(paste(packageName, "has been installed and the library is loaded !"))
  } else {
    library(packageName, character.only = TRUE)
    print(paste(packageName, "is installed and the library is loaded !"))
  }
}

## Clean environment
rm(installedPackages, packageName, packagesNeeded, packageExists)

# Function
find_pi = function(B = 5000, seed = 10, make_plot = TRUE){
  # Control seed
  set.seed(seed)
  
  # Generate the x and y co-ordinates from uniform
  # Distribution taking values from -1 to 1
  x<-runif(B, min=-1, max=1)
  y<-runif(B, min=-1, max=1)
  
  # Distance of the points from the center (0,0)
  z<-sqrt(x^2+y^2)

  # Radius=1
  hat_pi = 4*sum((z<=1))/length(z)
  
  # Plot
  if (make_plot == "TRUE"){
  par(pty="s")
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
  }

  return(hat_pi)

}

# Execute function
find_pi(make_plot = TRUE)
