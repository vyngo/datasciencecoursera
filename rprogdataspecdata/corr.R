getFileName <- function(directory, id){
  if(id >=1 && id < 10){
    paste(directory, "/00", toString(id) , ".csv", sep = "", collapse = NULL)
  }else if(id >= 10 && id < 100){
    paste(directory, "/0", toString(id) , ".csv",sep = "", collapse = NULL)
  }else{
    paste(directory, "/", toString(id) , ".csv",sep = "", collapse = NULL)
  }
  
}

complete <- function(directory, id = 1:332){
  nobs <- c()
  for(j in 1:length(id)){
    aId <- id[j]
    s <- 0
    file <- getFileName(directory, aId)
    data <- read.csv(file=file, header=TRUE, sep=",")
    for(i in 1:length(data$sulfate)){
      if(!is.na(data$sulfate[i]) && !is.na(data$nitrate[i])){
        s = s + 1
      }
    }
    nobs[j] <- s
  }
  data.frame(id = id, nobs = nobs)
}

corr <- function(directory, threshold = 0){
  data <- complete(directory)
  sub_data <- subset(data, nobs > threshold)
  id <- sub_data$id
  vector <- vector(mode="numeric", length=0)
  for(aId in id){
    file <- getFileName(directory, aId)
    temp <- read.csv(file=file, header=TRUE, sep=",")
    s <- c()
    v <- c()
    for(i in 1:length(temp$sulfate)){
      if(!is.na(temp$sulfate[i]) && !is.na(temp$nitrate[i])){
        s <- c(s, temp$sulfate[i])
        v <- c(v,temp$nitrate[i])
      }
    }
    
    vector <- c(vector, cor(s,v))
    
  }
  vector
}