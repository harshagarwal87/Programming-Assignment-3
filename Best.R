# Function to find best hospital in a state on basis of given outcome
# from "outcome-of-care-measures.csv". An error will be generated if
# either invalid state or outcome is provided by the user.


best <- function(state, outcome) {
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
  StateSplitDf <- data.frame(StateSplit[[state]])
  # Changing column names for newly created data frame
  colnames(StateSplitDf) <- OutcomeColnames
  # Coercing required outcome column data to numeric
  StateSplitDf[, outcome] <- as.numeric(StateSplitDf[, outcome])
  # Arranging data in ascending order on basis of Hospital name
  StateSplitDf <- StateSplitDf[order(StateSplitDf$Hospital), ]
  # Arranging data in ascending order on basis of required outcome
  StateSplitDf <- StateSplitDf[order(StateSplitDf[outcome]), ]
  # getting value of Hospital from first row
  BestHospital <- StateSplitDf$Hospital[1]
  BestHospital
}
