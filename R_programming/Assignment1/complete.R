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
