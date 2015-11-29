rankall <- function(outcome = 'heart attack', rank = '1', data = 'ProgAssignment3-data/outcome-of-care-measures.csv') {
  
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
  
  getRankedHospital <- function(state, rank, dataset) {
    
    getRankedHospital <- NA

    dfs <- subset(dataset, dataset$State == state, select=c('Hospital.Name', 'State', 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack', 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure', 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'))
    names(dfs) <- c('Hospital', 'State','Heart Attack', 'Heart Failure', 'Pneumonia')
    rowCt <- nrow(dfs)
    
    if(rowCt > 0) {
      valid_rows <- subset(dfs, dfs[bestOutcome(outcome)] != 'Not Available', select=c('Hospital', bestOutcome(outcome)))
      rowCt <- nrow(valid_rows)
      if(rowCt > 0) {
        svr <- valid_rows[order(as.numeric(valid_rows[,2]),valid_rows[,1]), ]
    
        if(getRank(rank,rowCt) <= rowCt) {
          getRankedHospital <- svr[getRank(rank,rowCt),1]
        }
      }
    }    
    
    getRankedHospital
  }
  
  ##
  ## Use the hospital data file to build a list of valid USPS
  ## state abbreviations
  ##
  hosp <- read.csv('ProgAssignment3-data/hospital-data.csv', colClasses = 'character')
  stateabb <- (sort(unique(hosp$State), decreasing = FALSE))
  
  outc <- c('heart attack', 'heart failure', 'pneumonia')
  if(!outcome %in% outc){
    stop('invalid outcome')
  }
  
  ##
  ## Read in the data
  df <- read.csv(data, colClasses = 'character')

  resdf <- sapply(stateabb, getRankedHospital, rank, df)

  resdf <- as.data.frame(cbind(resdf, stateabb))
  names(resdf) <- c('hospital', 'state')

  rankall <-resdf
  
  rankall
}