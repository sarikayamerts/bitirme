shin_prob_calculator <- function(list){
    a = list[1]
    b = list[2]
    c = list[3]
    
    if(is.na(a) || is.na(b) || is.na(c)){
      as.double(NA)
    }
    else{
      beta <- a + b + c
      z_current = 0
      z_new = sqrt(z_current^2+4*(1-z_current)*a*a/beta)+sqrt(z_current^2+4*(1-z_current)*b*b/beta)+sqrt(z_current^2+4*(1-z_current)*c*c/beta)-2
      i = 1
      while (abs(z_new - z_current) > 0.001 && i < 21 ){
        z_current = z_new
        z_new = sqrt(z_current^2+4*(1-z_current)*a*a/beta)+sqrt(z_current^2+4*(1-z_current)*b*b/beta)+sqrt(z_current^2+4*(1-z_current)*c*c/beta)-2
        i = i+1
      }
      z <- round((z_current+z_new)/2,3)
      (sqrt(z^2+4*(1-z)*list*list/beta)-z)/(2-2*z)
      
    }
  
}