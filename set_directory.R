set_directory <- function(name){
  if (name == "mert_data"){
    ### mert's macbook database directory
    setwd("/Users/mertsarikaya/Downloads/Bitirme/")
    ### emre's database directory
    #setwd("C:/Users/Hp/Desktop/Bitirme")
  }
  else if (name == "emre_data"){
    ### emre's database directory
    setwd("C:/Users/Hp/Desktop/Bitirme")
  }
  else if (name == "mert"){
    ### mert's macbook github directory
    setwd("/Users/mertsarikaya/bitirme/")
  }
  else if (name == "emre"){
    ### emre's github directory
    setwd("C:/Users/Hp/Desktop/Bitirme/bitirme")
  }
}
