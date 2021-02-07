pollutantmean <- function(directory, pollutant, id = 1:332) {
  total_dat <- data.frame()
  for (i in id) {
   dat = read.csv(paste0(directory,"/",sprintf("%03d", i),".csv"))
   total_dat = rbind(total_dat, dat)
  }
  mean(total_dat[[pollutant]], na.rm = TRUE)
}

