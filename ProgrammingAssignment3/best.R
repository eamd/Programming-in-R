best <- function(state = NULL, outcome = NULL, data = NULL) {
  
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
  
  if(is.null(data)){
    stop('invalid data file name')
  } 
  
  df <- read.csv(data, colClasses = 'character')
  dfs <- subset(df, df$State == state, select=c('Hospital.Name', 'State', 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack', 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure', 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'))
  names(dfs) <- c('Hospital', 'State','Heart Attack', 'Heart Failure', 'Pneumonia')

  ds <- subset(dfs, dfs[bestOutcome(outcome)] != 'Not Available', select=c('Hospital', bestOutcome(outcome)))
  
  lowestRate <- min(as.numeric(ds[,2]))
  
  lds <- subset(ds, as.numeric(ds[,2]) == lowestRate)

  if(nrow(lds)==1){
    best <- lds[,1]
  } else {
    tmp <- lds[order(lds$Hospital),]
    best <- tmp[1,1]
  }
  
  best
}