complete <- function(directory, id = 1:332){
  files_full <- list.files(directory, full.names = TRUE)
  #files_full
  len <- length(id)
  nobs <- rep(0, times = len)
  res <- data.frame(id, nobs)
  #res
  for(i in id){
    data <- read.csv(files_full[i])
    sum <- sum((!is.na(data["nitrate"])) & (!is.na(data["sulfate"])))
    res[ res[, "id"] == i, "nobs"] <- sum
  }
  res
}

corr <- function(directory, threshold = 0){
  # FULL PATH FOR EACH FILE
  files_full <- list.files(directory, full.names = TRUE)
  #complete["id", "nobs"] of all files
  complete_objs <- complete(directory)
  res <- c()
  
  for(i in complete_objs$id){
    if(complete_objs[i, "nobs"] > threshold){
      # read the data in each file, and remove the incomplete row
      raw_data <- read.csv(files_full[i])
      clean_data <- raw_data[complete.cases(raw_data),]
      #obtain each column and stored in corresponding variable
      sulfate_data <- clean_data[, "sulfate"]
      nitrate_data <- clean_data[, "nitrate"]
      res <- c(res, cor(sulfate_data, nitrate_data))
    }
  }
  return(round(res, 5))
}


