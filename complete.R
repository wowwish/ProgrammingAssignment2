complete <- function(directory, id = 1:332) {
  dat <- data.frame()
  for (i in id){
    len <- sum(complete.cases(read.csv(paste0(directory, "/", 
                                      sprintf("%03d", i),".csv"))))
    dat <- rbind(dat, data.frame(id = i, nobs = len))
  }
  dat
}