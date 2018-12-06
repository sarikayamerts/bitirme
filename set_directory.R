set_directory <- function(name){
  if (grepl("mert", toString(getwd()))){
    if (name == "data"){
      setwd("/Users/mertsarikaya/Downloads/Bitirme/")
    }
    else if (name == "code"){
      setwd("/Users/mertsarikaya/bitirme/")
    }
  }
  if (grepl("Hp", toString(getwd()))){
    if (name == "data"){
      setwd("C:/Users/Hp/Desktop/Bitirme")
    }
    else if (name == "code"){
      ### mert's macbook github directory
      setwd("C:/Users/Hp/Desktop/Bitirme/bitirme")
    }
  }
}
  