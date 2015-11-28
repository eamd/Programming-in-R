##
## Best finds the hospital with the lowest mortality rate for
## a given state for a specified outcome.
##

best <- function(state = NULL, outcome = NULL, data = 'ProgAssignment3-data/outcome-of-care-measures.csv') {
  
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

  ## Get rid of the rows that do not have data for the column
  ## specified
  ds <- subset(dfs, dfs[bestOutcome(outcome)] != 'Not Available', select=c('Hospital', bestOutcome(outcome)))
  
  ## Find the lowest rate
  lowestRate <- min(as.numeric(ds[,2]))
  
  ## Find the hospital(s) with the lowest rate
  lds <- subset(ds, as.numeric(ds[,2]) == lowestRate)

  ##
  ## It is possible that multiple hospitals will tie for the
  ## 
  if(nrow(lds)==1){ ## if only one hospital was found, return that name
    best <- lds[,1]
  } else {
    
    ## Otherwise, sort the hospitals with the lowest rate,
    ## in ascending order by name, and return the first name
    ## found.
    tmp <- lds[order(lds$Hospital),]
    best <- tmp[1,1]
  }
  
  best
}