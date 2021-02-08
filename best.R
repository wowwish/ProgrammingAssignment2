# Function to read data, check that arguments are valid and print the hospital
# name with lowest 30-day death rate

best <- function(state, outcome) {
  dat <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE)

  # sanity check for state name and outcome
  if (!any(state == unique(dat$State))) {stop("invalid state")}
  else if (outcome == "heart attack") {col = 11}
  else if (outcome == "heart failure") {col = 17}
  else if (outcome == "pneumonia") {col = 23}
  else if (!any(outcome == c("heart attack", "heart failure", "pneumonia"))) {
    stop("invalid outcome")
  }
  dat[dat[,col] == "Not Available", col] <- NA 
  dat <- dat[(!is.na(dat[,col]) & (dat$State == state)),]
  dat[, col] <- as.numeric(dat[, col])
  
  # Sort by Ascending order of mortality rate and use lexicographic ordering for
  # tie situations
  dat <- dat[order(dat[, col], dat[, 2]),]
  dat[1, 2]
}