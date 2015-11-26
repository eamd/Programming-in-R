corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  ## check that the directory entered is a valid directory
  
  ## Check to make sure the correct directory is entered
  ## If the directory can not be found, exit gracefully
  
  crs <- vector(mode='numeric')

  if(!dir.exists(directory) || (threshold < 0) || !is.numeric(threshold))
  {
    if(!dir.exists(directory)) {
      print('The directory/folder entered is incorrect')
    }
    if((threshold < 0) || !is.numeric(threshold)) {
      print('Threshold must be a value greater than or equal to 0 (threshold >= 0).')
    }

    return()
  } else {
  
    ## Load the names of the data files found in the specified directory
    all_data_files <- list.files(directory, full.names=T)   #creates a list of files names
  
    ##
    ## Create a list of data file names that meet the threshhold requirements
    ##
    df <- data.frame(matrix(nrow=length(all_data_files),ncol=3))
    colnames(df) <- c('id','nobs', 'c')
    
    for (i in 1:length(all_data_files)) {
      import_data <- read.csv(all_data_files[i]) # read the data
      cleaned_data <- import_data[complete.cases(import_data),] # remove the rows that have NA's
      
      ##
      ## count the number of rows left, if it is greater than the threshold,
      ## calculate the correlation value
      ##
      if (nrow(cleaned_data)>threshold){ 
        cr <- cor(cleaned_data[,2],cleaned_data[,3])
        crs <- append(crs,cr) # add the correlation value to the vector of correlation values
      }
    }
  }
  crs
  
}