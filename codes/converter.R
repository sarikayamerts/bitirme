convert <- function(arr){
  if (all(unique(arr) %in% c("1", "2", "3"))){
    arr <- replace(arr, arr=="1", "odd1")
    arr <- replace(arr, arr=="2", "oddX")
    arr <- replace(arr, arr=="3", "odd2")
    arr
  }
  else if (all(unique(arr) %in% c("odd1", "oddX", "odd2"))){
    arr <- replace(arr, arr=="odd1", 1)
    arr <- replace(arr, arr=="oddX", 2)
    arr <- replace(arr, arr=="odd2", 3)
    arr
  }
  else if (all(unique(arr) %in% c(1, 2, 3))){
    arr <- replace(arr, arr==1, "odd1")
    arr <- replace(arr, arr==2, "oddX")
    arr <- replace(arr, arr==3, "odd2")
    arr
  }
}

convert_factor <- function(arr){
  if (all(unique(arr) %in% noquote(c("odd1", "oddX", "odd2")))){
    arr <- revalue(arr, c("odd1" = 1, "oddX" = 2 ,"odd2" = 3)) 
    levels(arr) <- c(1, 2, 3)
    arr
  }
}
