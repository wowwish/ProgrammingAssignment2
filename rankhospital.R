# Function to read data and provide the Hospital with the given ranking for
# the given state

rankhospital <- function(state, outcome, num) {
  dat <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE)
  
  # sanity check for state name and outcome
  if (!any(state == unique(dat$State))) {stop("invalid state")}
  else if (outcome == "heart attack") {col = 11}
  else if (outcome == "heart failure") {col = 17}
  else if (outcome == "pneumonia") {col = 23}
  else if (!any(outcome == c("heart attack", "heart failure", "pneumonia"))) {
    stop("invalid outcome")
  }
  else if (num != "best" & num != "worst" & !is.numeric(num)) {
    stop("invalid rank")
  }
  dat[dat[,col] == "Not Available", col] <- NA 
  dat <- dat[(!is.na(dat[,col]) & (dat$State == state) & !is.na(dat[, 2])),]
  dat[, col] <- as.numeric(dat[, col])
  
  # Sort by Ascending order of mortality rate and use lexicographic ordering for
  # tie situations
  dat <- dat[order(dat[, col], dat[, 2]),]
  
  # Returning appropriate Hospital name based on rank given by user
  if (num == "best") { return(dat[1, 2]) }
  if (num == "worst") { return(dat[nrow(dat), 2]) }
  if (is.numeric(num) & num > nrow(dat)) { return(NA) }
  return(dat[num, 2])
}