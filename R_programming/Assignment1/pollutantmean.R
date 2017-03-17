pollutantmean <- function(directory, pollutant, id = 1:332){
  #filelist <- list.files(directory)
  #filenames <- paste(directory, filelist, sep = "/")
  filenames <- paste(directory, list.files(directory), sep = "/")
  result <- c()
  for(i in id){
   data <- read.csv(filenames[i])
   rm_data <- data[ which(!is.na(data[,pollutant])), pollutant]
   result <- c(result, rm_data)
  }
  meanval <- mean(result)
  round(meanval, 3)
}