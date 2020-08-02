rankhospital <- function(state, outcome, num = "best") {
  # Loading data in R and filtering only required columns
  OutcomeData <- read.csv("outcome-of-care-measures.csv")
  ReqOutcomeData <- OutcomeData[c(2, 7, 11, 17, 23)]
  # Creating a vector of column names
  OutcomeColnames <-
    c("Hospital",
      "State",
      "heart attack",
      "heart failure",
      "pneumonia")
  # Creating a vector of valid outcomes
  Outcomenames <- OutcomeColnames[3:5]
  # Checking if state provided by user is valid
  if (!state %in% ReqOutcomeData$State)
    stop("invalid state")
  # Checking if outcome provided by user is valid
  if (!outcome %in% Outcomenames)
    stop("invalid outcome")
  # Splitting dataframe on basis of different states
  StateSplit <- split(ReqOutcomeData, ReqOutcomeData$State)
  # Subsetting data frame containing rows with user provided state only
  StateSplitDf <- StateSplit[[state]]
  # Changing column names for newly created data frame
  colnames(StateSplitDf) <- OutcomeColnames
  # Coercing required outcome column data to numeric
  StateSplitDf[, outcome] <- as.numeric(StateSplitDf[, outcome])
  # Removing Na's from outcome column
  DfWithoutNA <- complete.cases(StateSplitDf[, outcome])
  StateSplitDf <- StateSplitDf[DfWithoutNA,]
  # Arranging data in ascending order on basis of Hospital name
  StateSplitDf <- StateSplitDf[order(StateSplitDf$Hospital), ]
  # Arranging data in ascending order on basis of required outcome
  StateSplitDf <- StateSplitDf[order(StateSplitDf[outcome]), ]
  # Setting value of num for best & worst respectively
  if (num == "best")
    num <- 1
  if (num == "worst")
    num <- nrow(StateSplitDf)
  if (num > nrow(StateSplitDf)) {
    # Passing Hospital value as NA if num is greater than total rows
    RankHospital <- NA
  }
  else {
    # getting value of Hospital from in row = num
    RankHospital <- StateSplitDf$Hospital[num]
  }
  RankHospital
}