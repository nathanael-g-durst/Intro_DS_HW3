#Problem 2: Satellite Navigation System
##the position 

install.packages("pryr")
install.packages("plotrix")
library(pryr)  # only for otype
library(plotrix)


data = c(458.9474, 337.1013, 363.1112, 
         337.0894, 458.9355, 363.0993, 
         442.5835, 442.5835, 283.9493, 
         520.1845, 520.1845, 184.0449, 
         534.1411, 499.0299, 191.3455, 
         499.1322, 534.2434, 191.4479, 
         542.0904, 470.4216, 212.7515, 
         470.4070, 542.0758, 212.7369, 
         541.6032, 429.4569, 250.9978,
         429.4120, 541.5583, 250.9528)
dist_mat = matrix(data, ncol=3,nrow=10, byrow = T)


get_position=function(dist){
  
  matr = matrix(NA, nrow = length(dist)/3, ncol = 2)
  
  if(class(dist)=="matrix"){
    for(i in 1:(length(dist)/3)){
      obj_func = function(theta){
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
    return(matr)
  }else{
    obj_func = function(theta){
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
    return(pos)
  }
}

position = get_position(c(453.2136, 288.8427, 418.3106))
position

# matrix as input
#position = get_position(dist_mat)
#position
#class(position)

if(class(position)=="matrix"){
  attr(position, "class") <- "matrix"
  plot.matrix <- function(position){
    for(i in 1:(length(position)/2)){
      par(pty="s")
      plot(position[,1],position[,2],
           xlim = c(-300,300),
           ylim = c(-300,300),
           xlab = "x",
           ylab = "y",
           col = alpha("orange", 1),
           cex = 1,
           pch = 4,
           asp = 1,
           grid())
      points(position[i,1],position[i,2],pch=4,col = alpha("orange", 1))
      draw.circle( 0, -300, 13 , col ="blue")
      draw.circle( -300, 300, 13, col = "red" )
      draw.circle( 300, 300, 13 , col = "green")
      draw.circle( 0, 0, 200 , col = alpha("grey", .3))
    }
  }
}else if(class(position)=="numeric"){
  attr(position, "class") <- "numeric"
  plot.numeric <- function(position){
    par(pty="s")
    plot(position[1],position[2],
         xlim = c(-300,300),
         ylim = c(-300,300),
         xlab = "x",
         ylab = "y",
         col = alpha("orange", 1),
         cex = 1,
         pch = 4,
         asp = 1,
         grid())
    draw.circle( 0, -300, 13 , col ="blue")
    draw.circle( -300, 300, 13, col = "red" )
    draw.circle( 300, 300, 13 , col = "green")
    draw.circle( 0, 0, 200 , col = alpha("grey", .3))
  }
}

plot(position)


if(class(position)=="matrix"){
  
  attr(position, "class") <- "matrix"
  summary.matrix <- function(position)
    for(i in 1:(length(position)/2)){
      cat("The estimated position is:")
      cat("\n")
      cat("X =", position[i,1])
      cat("\n")
      cat("Y =", position[i,2])
      cat("\n")
    }
}else{
  attr(position, "class") <- "numeric"
  summary.numeric <- function(position){
    cat("The estimated position is:")
    cat("\n")
    cat("X =", position[1])
    cat("\n")
    cat("Y =", position[2])
  }
}

summary(position)