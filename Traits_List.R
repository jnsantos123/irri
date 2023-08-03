#install and load all necessary packages
install.packages("readr")
library(readr)

#Define the required parameters
x <- [Directory where merged data are stored]
y <- [Directory where the list of traits will be stored]

traits <- function(x, y){
  d <- dir(x, pattern = ".csv")
  
  #Create a dummy data frame where the slist will be stored
  Trait <- matrix(nrow = 0, ncol = 2)
  Trait <- data.frame(Trait, row.names = NULL)
  
  for (i in 1:length(d)){
    Data <- read.csv(paste0(x, d[i]))
    
    for (j in 1:length(Data)){
      Trait[nrow(Trait)+1,1] <- names(Data)[j]
    }
  }
  
  #Remove duplicated traits and sort the traits alphabetically
  names(Trait)[1] <- "Traits"
  Trait <- Trait[!duplicated(Trait[,1]),]
  Trait <- Trait[ order(Trait[,1]), ]
  
  write.csv(Trait, paste0(y, "Traits List.csv"), row.names = FALSE)
}
traits(x, y)