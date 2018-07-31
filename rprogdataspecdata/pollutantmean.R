getFileName <- function(directory, id){
  if(id >=1 && id < 10){
    paste(directory, "/00", toString(id) , ".csv", sep = "", collapse = NULL)
  }else if(id >= 10 && id < 100){
    paste(directory, "/0", toString(id) , ".csv",sep = "", collapse = NULL)
  }else{
    paste(directory, "/", toString(id) , ".csv",sep = "", collapse = NULL)
  }
  
}

pollutantmean <- function(directory, pollutant, id = 1:332){
  s <- 0
  l <- 0
  for(aId in id){
    file <- getFileName(directory, aId)
    data <- read.csv(file=file, header=TRUE, sep=",")
    list_polutant <- data[,pollutant]
    s = s + sum(list_polutant, na.rm = TRUE)
    l = l + length(list_polutant[!is.na(list_polutant)])
  }
  s / l
}