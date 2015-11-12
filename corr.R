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
  if(!dir.exists(directory))
  {
    return('The directory/folder entered is incorrect')
  }
  
  ## Load the names of the data files found in the specified directory
  all_data_files <- list.files(directory, full.names=T)   #creates a list of files names

  ##
  ## Create a list of data file names that meet the threshhold requirements
  ##
  df <- data.frame(matrix(nrow=length(all_data_files),ncol=3))
  colnames(df) <- c('id','nobs', 'c')
  
  crs <- vector()
  for (i in 1:length(all_data_files)) {
    import_data <- read.csv(all_data_files[i]) # read the data
    cleaned_data <- import_data[complete.cases(import_data),]
    if (nrow(cleaned_data)>threshold){
      cr <- cor(cleaned_data[,2],cleaned_data[,3])
      crs <- append(crs,cr)
    }
    ##
    ## Since the names of the data files, are monitor IDs, and they are of a standard format,
    ## I parsed out the ID# from the the pathname
    ##
    #df[i,1] <- as.integer(substr(all_data_files[i], nchar(all_data_files[i])-6, nchar(all_data_files[i])-4))
    #df[i,2] <- nrow(cleaned)
    
    #x <- (cleaned[,2]) # sulfate data
    #y <- (cleaned[,3]) # nitrate data
    #z <- cor(x,y)
    #df[i,3] <- z
    
  }
  
  crs
  
}