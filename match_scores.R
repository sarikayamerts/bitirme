winner <- function(a){
  if (is.na(a)) {output = NA}
  else{
    home <- as.integer(unlist(strsplit(a, ":"))[1])
    away <- as.integer(unlist(strsplit(a, ":"))[2])
    if(home > away) {output = "odd1"}
    if(home == away) {output = "oddX"}
    if(home < away) {output = "odd2"}
    output
  } 
}

over_under <- function(a){
  home <- as.integer(unlist(strsplit(a, ":"))[1])
  away <- as.integer(unlist(strsplit(a, ":"))[2])
  if(home + away >= 3) {output = "oddover"}
  if(home + away < 3) {output = "oddunder"}
  output
}

inverse <- function(a){1/as.numeric(a)}
