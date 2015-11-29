rankall <- function(outcome = NULL, rank = 1, datapath = paste(getwd(),'/ProgAssignment3-data/', sep=''), data = 'outcome-of-care-measures.csv') {
  
  rankall <- NULL
  datafile <- paste(datapath, data, sep='')
  hospfile <- paste(datapath, 'hospital-data.csv', sep='' )
  
  ##
  ## Function getRank returns a numeric value for the rank entered.
  ## If the term, 'best' is entered, the rank will be set to 1.
  ##
  ## If the term, 'worst' is entered, the rank will be set to the number of
  ## of rows in the supplied dataset (dsSize).
  ##
  ## If a number is supplied (in character format) convert it to a numeric vector.
  ## This last check is just in case somebody enters a character value for the 
  ## rank.
  ##
  validRank <- function(theRank){
    validRank <- FALSE
    
    if(tolower(theRank) %in% c('best', 'worst')){
      validRank <- TRUE
    } else {
      if(suppressWarnings(is.numeric(as.numeric(theRank)))) {
        validRank <- TRUE
      }
    }
  validRank
  }
  
  calcRank <- function(rank, ds) {
    if(tolower(rank) == 'best') {
      calcRank <- 1
    } else if(tolower(rank) == 'worst') {
      calcRank <- nrow(ds)
    } else calcRank <- as.numeric(rank)
  }
  
  ##
  ## Find the ranked hospital for the given state.
  ##
  
  getRankedHospital <- function(state, rank) {
    
    getRankedHospital <- NA

    ##
    ## Extract the rows for  the specified state.
    ##
    ## Additionally, specify the columns from the dataset that will be used
    ##
    dfs <- subset(df, df$State == state, select=c('Hospital.Name', 'State', 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack', 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure', 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'))

    ##
    ## Relabel the column names to be easier to reference
    ##
    names(dfs) <- c('hospital', 'state','heart attack', 'heart failure', 'pneumonia')
    
    ## Calculate the number of hospitals retrieved for the specified state.
    rowCt <- nrow(dfs)
    
    ##
    ## Remove the rows that have a value 'Not Available' for the specified outcome
    ## and revise the number of hospitals retrieved (row count)
    ##
    if(rowCt > 0) {
      valid_rows <- subset(dfs, dfs[outcome] != 'Not Available', select=c('hospital', outcome))
      rowCt <- nrow(valid_rows)
      if(rowCt > 0) {
        
        ##
        ## Sort rows by outcome, then by hospital within outcome. This is
        ## for when multiple hospitals have the same outcome rate. The hospitals are
        ## sorted by name in ascending alphabetical order.
        ##
        svr <- valid_rows[order(as.numeric(valid_rows[,2]),valid_rows[,1]), ]
    
        ##
        ## Get the ranked hospital for the state specified.
        ##
        theRank <- calcRank(rank, svr)
        if(theRank <= rowCt) { # Note: if theRank is greater than rowCt, the function will return NA (defined earlier)
          getRankedHospital <- svr[theRank,1]
        }
      }
    }    
    
    getRankedHospital
  }
  
  ##
  ## Verify that the outcome submitted, is valid
  ## heart attack, heart failure and pneumonia are the only accepted outcomes
  ##
  
  if(is.null(outcome)) {
    stop('invalid outcome')
  }
  outc <- c('heart attack', 'heart failure', 'pneumonia')
  if(!outcome %in% outc){
    stop('invalid outcome')
  }
  outcome <- tolower(outcome)

  ##
  ## Use the hospital data file to build a list of valid USPS
  ## state abbreviations
  ##
  if(file.exists(hospfile)){
    hosp <- read.csv(hospfile, colClasses = 'character')
    stateabb <- (sort(unique(hosp$State), decreasing = FALSE))
  } else {
    stop(paste(hospfile), 'is an invalid path for the hospital data file.')
  }
  ##
  ## Read in the data
  ##
  
  if(file.exists(datafile)) {
    df <- read.csv(datafile, colClasses = 'character')
  } else {
    stop(paste(datafile,'is an invalid path for the outcome data file.'))
  }
  
  ##
  ## Check for a valid rank entered
  ##
  if(!validRank(rank)) {
    stop('invalid rank')
  }
  
  ##
  ## For each state in the stateabb list, find the hospital for  the rank provided.
  ## The result will be returned as a character vector, one element for each state.
  ##
  resdf <- sapply(stateabb, getRankedHospital, rank)

  ##
  ## The number of elements in resdf should be the same as in the vector, stateabb.
  ## The the two vectors can be column bound into a dataframe.
  ## The resulting data frame is stored in resdf (why create another variable?).
  ##
  resdf <- as.data.frame(cbind(resdf, stateabb))

  ## Since I was changing column names earlier, I had to set them to the names (formatted as well) 
  ## specified in the assignment documentation.
  names(resdf) <- c('hospital', 'state')

  ##
  ## Return the data frame to the function name in case, this data frame
  ## will be used in other code.
  ##
  rankall <-resdf
  
  ##
  ## So we can see the results of the function call,
  ## When called from the command line.
  ##
  rankall
}