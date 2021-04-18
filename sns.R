#Problem 2: Satellite Navigation System
##the position 

get_position=function(dist){
  
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

  cat("x=",est[1],"y=",est[2])
}

position = get_position(c(453.2136, 288.8427, 418.3106))
summary(position)
