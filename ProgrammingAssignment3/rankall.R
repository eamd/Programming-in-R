rankall <- function(outcome = NULL, rank = '1', data = 'ProgAssignment3-data/outcome-of-care-measures.csv') {
  
  rankall <- NULL

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
  
  outc <- c('heart attack', 'heart failure', 'pneumonia')
  if(!outcome %in% outc){
    stop('invalid outcome')
  }
  
  ##
  ## Read in the data
  df <- read.csv(data, colClasses = 'character')
  
  x <- sapply(stateabb,function(x){print(paste('State: ', x))})
  
  print(class(x))
  rankall
}