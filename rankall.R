rankall <- function(outcome, num = "best") {
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
  # Checking if outcome provided by user is valid
  if (!outcome %in% Outcomenames)
    stop("invalid outcome")
  # Changing column names for the data frame
  colnames(ReqOutcomeData) <- OutcomeColnames
  # Subsetting database with only required columns
  ReqOutcomeData <- ReqOutcomeData[, c("Hospital", "State", outcome)]
  # Coercing required outcome column data to numeric
  ReqOutcomeData[, outcome] <- as.numeric(ReqOutcomeData[, outcome])
  # Removing Na's from outcome column
  DfWithoutNA <- complete.cases(ReqOutcomeData[, outcome])
  ReqOutcomeData <- ReqOutcomeData[DfWithoutNA,]
  # Arranging data in ascending order on basis of Hospital name
  ReqOutcomeData <- ReqOutcomeData[order(ReqOutcomeData$Hospital), ]
  # Arranging data in ascending order on basis of required outcome
  SReqOutcomeData <- ReqOutcomeData[order(ReqOutcomeData[outcome]), ]
  # # Creating a vector of unique states value
  # UniqueStates <- sort(unique( SReqOutcomeData$State))
  # # Initializing RankAllDf - which have the end data frame
  # RankAllDf <- data.frame()
  # 
  # for (i in seq_along(UniqueStates)) {
  #   StatesData <- SReqOutcomeData[SReqOutcomeData$State == UniqueStates[i], ]
  #   # Setting value of num for best & worst respectively
  #   if (num == "best")
  #     num <- 1
  #   if (num == "worst")
  #     num <- nrow(StatesData)
  #   StateSplitDf <- StatesData[num, 1:2]
  #   StateSplitDf[1,2] <- UniqueStates[i]
  #   RankAllDf <- rbind(RankAllDf, StateSplitDf)
  # }
  # RankAllDf
  # Splitting dataframe on basis of different states
  StateSplit <- split(SReqOutcomeData, SReqOutcomeData$State)
  # Creating and initializing RankAllDf
  RankAllDf <- data.frame()
  # Looping through StateSplit list for every state
  for (i in seq_along(StateSplit)) {
    # Setting value of num for best & worst respectively
    if (num == "best")
      num <- 1
    if (num == "worst")
      num <- nrow(StateSplit[[i]])
    # Subsetting data frame from the list for current value of state in the loop
    StateSplitDf <- StateSplit[[i]]
    # Subsetting row = num from the data frame and extracting columns 1 & 2
    StateSplitDf <- StateSplitDf[num, 1:2]
    # Adding value of state to state column to ensure final data frame has
    # state values, incase total rows in StateSplitDf is less than num
    StateSplitDf[1,2] <- names(StateSplit[i])
    # Combining data frame for all state values
    RankAllDf <- rbind(RankAllDf, StateSplitDf)
  }
  RankAllDf
}