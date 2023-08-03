#install and load all necessary packages
install.packages("readr")
library(readr)

#Define the required parameters
x <- [Directory where merged data are stored]
y <- [Directory where the list of traits will be stored]

traits <- function(x, y){
  d <- dir(x, pattern = ".csv")
  
  Traits <- matrix(nrow = 0, ncol = 1)
  Traits <- data.frame(Traits, row.names = NULL)
  
  for (i in 1:length(d)){
    Data <- read.csv(paste0(x, d[i]))
    
    for (j in 1:length(Data)){
      Traits[nrow(Traits)+1,1] <- names(Data)[j]
    }
  }
  
  Traits <- Traits[!duplicated(Traits[,1]),]

  write.csv(Traits, paste0(y, "Traits List.csv"))
}
traits(x, y)