#install and load all necessary packages
install.packages("readr")
install.packages("stringr")
library(readr)
library(stringr)

#Define the required parameters
x <- [Directory where plot level data and metadata are stored]
y <- [Directory where merged data will be stored]

Merge <- function(x, y){
  d <- dir(x, pattern = ".csv")
  
  #Reads the plot level data and metadata for each experiment
  for (i in 1:length(d)){
    if (i %% 2 != 0){
      plotData <- read.csv(paste0(x, d[i]))
      occData <- read.csv(paste0(x, d[i+1]))
      
      #merges the two data into a single data by the predetermined variables
      pData <- merge(plotData, occData, 
                     by.x = c("DESIGN.X", "DESIGN.Y", "ENTRY.CODE", "ENTRY.TYPE", "ENTNO", "GERMPLASM.CODE",
                              "GERMPLASM.NAME", "PA.X", "PA.Y", "PLOT.CODE", "PLOTNO"),
                     by.y = c("design_x", "design_y", "entry_code", "entry_type", "entry_number", 
                              "germplasm_code", "germplasm_name", "pa_x", "pa_y", "plot_code", "plot_number"))
      
      #Retains only alphanumeric characters and converts variable names to uppercase
      for (j in 1:ncol(pData)){
        names(pData)[j] <- str_replace_all(names(pData)[j], "[^[:alnum:]]", "")
        names(pData)[j] <- toupper(names(pData)[j])
      }
      #saves the data into a csv file
      #follows a "occurrence_name"__"occurrence_code" format
      write.csv(pData, paste0(y, pData$OCCURRENCENAME[1], "__", pData$OCCURRENECODE[1], ".csv"))
    }
  }
}