corr <- function(directory, threshold = 0) {
  files <- list.files(directory, full.names = TRUE)
  correlation <- vector("numeric")
  for (i in 1:length(files)) {
    if(sum(complete.cases(read.csv(files[i]))) > threshold) {
      dat <- read.csv(files[i])
      dat <- dat[which(complete.cases(dat)),]
      correlation <- c(correlation, cor(dat[["nitrate"]], dat[["sulfate"]]))
    }
  }
  correlation
}