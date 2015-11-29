##
## Best finds the hospital with the lowest mortality rate for
## a given state for a specified outcome.
##

rankhospital <- function(state = NULL, outcome = NULL, rank = '1', data = 'ProgAssignment3-data/outcome-of-care-measures.csv') {

  rankhospital <- NULL
  
  
  ##
  ## bestOutcome takes the passed outcome value and
  ## reformats it to match the column heading for the subsetted dataframes.
  ##
  
  bestOutcome <- function(outcome){
    if(outcome == 'pneumonia'){
      offset = 'Pneumonia'
    } else if (outcome == 'heart failure') {
      offset = 'Heart Failure'
    } else {
      offset = 'Heart Attack'
    }
    offset
  }
  
  getRank <- function(theRank, dsSize){
    
    if(toupper(theRank) == 'BEST'){
      getRank <- 1
    } else if(toupper(theRank)=='WORST'){
      getRank <- dsSize
    } else {
      getRank <- as.numeric(theRank)
    }
    getRank
  }

  ##
  ## Use the state data set to build a list of valid USPS
  ## state abbreviations
  ##
  data(state) # load US states dataset
  stateabb <- state.abb
  
  if(!((nchar(state)==2) & (state %in% stateabb))){
    stop('invalid state')
  }
  
  outc <- c('heart attack', 'heart failure', 'pneumonia')
  if(!outcome %in% outc){
    stop('invalid outcome')
  }
  
  ##
  ## Read in the data
  df <- read.csv(data, colClasses = 'character')
  
  ## Find all the rows that match the state specified and
  ## return only the columns specified
  
  dfs <- subset(df, df$State == state, select=c('Hospital.Name', 'State', 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack', 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure', 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'))
  names(dfs) <- c('Hospital', 'State','Heart Attack', 'Heart Failure', 'Pneumonia')
  valid_rows <- subset(dfs, dfs[bestOutcome(outcome)] != 'Not Available', select=c('Hospital', bestOutcome(outcome)))

  svr <- valid_rows[order(as.numeric(valid_rows[,2]),valid_rows[,1]), ]
  
  if(getRank(rank,nrow(svr)) <= nrow(svr)) {
    rankhospital <- svr[getRank(rank,nrow(svr)),1]
  } else {
    rankhospital <- 'NA'
  }
  rankhospital
}