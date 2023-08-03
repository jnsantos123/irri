#install and load all necessary packages
install.packages("readr")
library(readr)

#Define the required parameters
x <- [Directory where merged data are stored]
y <- [Directory where trait summary will be stored]
z <- [Directory of the file where the list of traits are]

traitSummary <- function(x, y, z){
  d <- dir(x, pattern = ".csv")
  
  traitsList <- read.csv(z, col_names = FALSE)
  
  #Setup the data frame where the summary will be stored
  #Populate all fields with "FALSE" value
  Occurrence_Code <- FALSE
  Occurrence_Name <- FALSE
  traits <- data.frame(Occurrence_Code, Occurrence_Name)
  
  for (i in 1:nrow(traitsList)){
    traits[,2+i] <- FALSE
    names(traits)[2+i] <- traitsList[i,1]
  }
  
  #Fill each column with the occurrence code, name, and "FALSE" values for other columns
  #Done per merged data file
  for (i in 1:length(d)){
    Data <- read.csv(paste0(x, d[i]))
    traits[i,1] <- Data$occurrence_code[1]
    traits[i,2] <- Data$occurrence_name[1]
    
    for (j in 1:nrow(traitsList)){
      traits[i,2+j] <- FALSE
    }
    
    #checks which traits in the list are in the merged data
    #removes null values then stores the number of rows
    for (k in 1:nrow(traitsList)){
      for (l in 1:ncol(Data)){
        if (traitsList[k,1] == names(Data)[l]){
          nullRemoved <- na.omit(Data[,l])
          traits[i,2+k] <- nullRemoved
        }
      }
    }
  }
  
  #saves the data frame as a csv file
  write.csv(traits, paste0(y, "Traits Summary.csv"), row.names = FALSE)
}