# Requirements

## Check if installed, install it if not and load library
installedPackages <- installed.packages()

### Add here the packages needed ###############
packagesNeeded <- c("plotrix", "pryr", "scales")
################################################

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

#Problem 2: Satellite Navigation System

## Functions

### Position
get_position <- function (dist) {
  
  matr = matrix(NA, nrow = length(dist)/3, ncol = 2)
  
  if (is.matrix(dist) == TRUE) {
    
    for (i in 1:(length(dist)/3)) {
      
      obj_func = function (theta) {
        x = theta[1]
        y = theta[2]
        eps = theta[3]
        d1_hat = dist[i,1]
        d2_hat = dist[i,2]
        d3_hat = dist[i,3]
        return(((-300-x)^2+(300-y)^2-(d1_hat-eps)^2)^2+
                 ((300-x)^2+(300-y)^2-(d2_hat-eps)^2)^2+
                 ((-x)^2+(-300-y)^2-(d3_hat-eps)^2)^2)
      }
      
      est = optim(par=c(0,0,0), fn=obj_func)$par
      matr[i,1]= est[1]
      matr[i,2]= est[2]
      
    }
    
    class(matr) <- "matrix"
    return(matr)
    
  } else if (is.numeric(dist) == TRUE) {
    
    obj_func <- function (theta) {
      x = theta[1]
      y = theta[2]
      eps = theta[3]
      d1_hat = dist[1]
      d2_hat = dist[2]
      d3_hat = dist[3]
      return(((-300-x)^2+(300-y)^2-(d1_hat-eps)^2)^2+
               ((300-x)^2+(300-y)^2-(d2_hat-eps)^2)^2+
               ((-x)^2+(-300-y)^2-(d3_hat-eps)^2)^2)
    }
    
    est = optim(par=c(0,0,0), fn=obj_func)$par
    pos = c(est[1],est[2])
    
    class(pos) <- "vector"
    return(pos)
    
  } else {
    print("Error: the data is neither a vector nor a matrix.")
  }
}

### Plots

#### Vector
plot.vector <- function (pos) {
  
  par(pty="s")
  plot(x = pos[1], y = pos[2],
       xlim = c(-300,300),
       ylim = c(-300,300),
       xlab = "x",
       ylab = "y",
       col = alpha("orange", 1),
       cex = 1,
       pch = 4,
       asp = 1,
       grid(),
  )
  draw.circle( 0, -300, 13 , col ="blue")
  draw.circle( -300, 300, 13, col = "red" )
  draw.circle( 300, 300, 13 , col = "green")
  draw.circle( 0, 0, 200 , col = alpha("grey", .3))
} 

#### Matrix
plot.matrix <- function (pos) {
  
  for(i in 1:(length(pos)/2)){
    
    par(pty="s")
    plot(x = pos[,1], y = pos[,2],
         xlim = c(-300,300),
         ylim = c(-300,300),
         xlab = "x",
         ylab = "y",
         col = alpha("orange", 1),
         cex = 1,
         pch = 4,
         asp = 1,
         grid(),
    )
    draw.circle( 0, -300, 13 , col ="blue")
    draw.circle( -300, 300, 13, col = "red" )
    draw.circle( 300, 300, 13 , col = "green")
    draw.circle( 0, 0, 200 , col = alpha("grey", .3))
    points(pos[i,1],pos[i,2],pch=4,col = alpha("orange", 1))
  }
}

### Summary

#### Vector
summary.vector <- function (position) {
  
  cat("The estimated position is:")
  cat("\n")
  cat("X =", position[1])
  cat("\n")
  cat("Y =", position[2])
  
}

#### Matrix
summary.matrix <- function (position) {
  
  for (i in 1:(length(position)/2)) {
    cat(paste("The estimated position of the object number ", i, " is :", sep=""))
    cat("\n")
    cat("X =", position[i,1])
    cat("\n")
    cat("Y =", position[i,2])
    cat("\n")
  }
  
}

### Test
execute <- function() {
  user_input <- readline("Would you like to test or code with the vector or the matrix given in HW3 ? \nType v for vector or m for matrix. (v/m)")
  if(user_input == 'v') {
    posVector <- get_position(dist_vect)
    plot(posVector)
    summary(posVector)
  } else if (user_input == 'm') {
    posMatrix <- get_position(dist_mat)
    plot(posMatrix)
    summary(posMatrix)
  } else {
    stop("you did not press v or m.")
  }
}

## Data

### Distance matrix
dist_mat <- matrix(data = c(458.9474, 337.1013, 363.1112, 
                            337.0894, 458.9355, 363.0993, 
                            442.5835, 442.5835, 283.9493, 
                            520.1845, 520.1845, 184.0449, 
                            534.1411, 499.0299, 191.3455, 
                            499.1322, 534.2434, 191.4479, 
                            542.0904, 470.4216, 212.7515, 
                            470.4070, 542.0758, 212.7369, 
                            541.6032, 429.4569, 250.9978,
                            429.4120, 541.5583, 250.9528),
                   ncol=3, nrow=10, byrow = T)

### Distance vector
dist_vect <- c(453.2136, 288.8427, 418.3106)

## Tests
execute()
