# rps = 1/(r-1) * sum{i = 1, over r} (sum{over i} p_j -sum{over i} e_j)^2
# where r is number of outcomes (3), p_j forecasted prob, e_j actual prob
# if 0.3, 0.5, 0.2 are forecasted prob and 1,0,0 actual prob
# then rps = 1/2 * (0.49+0.04) = 0.265
# rps(obs = c(1), pred = matrix(c(0.3, 0.5, 0.2), nrow = 1))

calculate_rps <- function(a,b,c,d){
  if(is.na(a) || is.na(b) || is.na(c)){
    as.double(NA)
  }
  else{
    if (d == "odd1") {d=1}
    if (d == "oddX") {d=2}
    if (d == "odd2") {d=3}
    pred = t(matrix(c(a, b, c)))
    output <- rps(obs = c(d), pred = pred)
    output$rps
  }
}




#for over under
calculate_rps2 <- function(a,b,c){
  if(is.na(a) || is.na(b)){
    as.double(NA)
  }
  else{
    if (c == "oddover") {c=1}
    if (c == "oddunder") {c=2}
    pred = t(matrix(c(a, b)))
    output <- rps(obs = c(c), pred = pred)
    output$rps
  }
}