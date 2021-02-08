# Function to read in data and return a dataframe of Hospitals that are at the
# user defined rank for the given outcome, for each state

rankall <- function(outcome, num = "best") {
  dat <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE)
  
  # sanity check for state name and outcome
  if (num != "best" & num != "worst" & !is.numeric(num)) {
    stop("invalid rank")
  }
  else if (outcome == "heart attack") {col = 11}
  else if (outcome == "heart failure") {col = 17}
  else if (outcome == "pneumonia") {col = 23}
  else if (!any(outcome == c("heart attack", "heart failure", "pneumonia"))) {
    stop("invalid outcome")
  }
  dat[dat[,col] == "Not Available", col] <- NA 
  dat <- dat[!is.na(dat[,col]),]
  dat[, col] <- as.numeric(dat[, col])
  
  # Sort by Ascending order of mortality rate and use lexicographic ordering for
  # tie situations
  dat <- dat[order(dat[, col], dat[, 2]),]
  
  # Extracting the Hospitals of given rank for each state
  hospitals <- tapply(dat[, 2], dat[, 7], function (x) {
    if (num == "best") { return(x[1]) }
    if (num == "worst") { return(x[length(x)]) }
    if (is.numeric(num) & num > length(x)) { return(NA) }
    return(x[num])
  })
  
  # returning in the appropriate format of dataframe
  hospitals <- data.frame(hospital = hospitals)
  hospitals$state <- rownames(hospitals)
  hospitals
}