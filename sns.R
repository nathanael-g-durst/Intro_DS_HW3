#Problem 2: Satellite Navigation System
##the position 

get_position=function(d_1_hate, d_2_hate,d_3_hate){
  
  
  
  
  obj_func = function(theta){
    x = theta[1]
    y = theta[2]
    eps=theta[3]
    return(((-300+x)^2+(300-y)^2-(d_2_hate-eps)^2)^2+
             ((300-x)^2+(300-y)^2-(d_2_hate-eps)^2)^2+
             ((-x)^2+(-300-y)^2-(d_3_hate-eps)^2)^2)
  }
  
  est = optim(par=c(0,0,0), fn=obj_func)$par
  cat("x=",est[1],"y=",est[2])
}
  // ca me donne des resultats différents  ,,,,,, get_position(453.2136,288.8427,418.3106) ca me donne 
x= -16.68395 y= 171.0875  !!!!

