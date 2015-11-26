complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        
        ## check that the directory entered is a valid directory
        
        ## Check to make sure the correct directory is entered
        ## If the directory can not be found, exit gracefully
        if(!dir.exists(directory))
        {
            return('The directory/folder entered is incorrect')
        }
        
        
        ## Load the names of the data files found in the specified directory
        all_data_files <- list.files(directory, full.names=T)   #creates a list of files names
        
        ## Use the ID to subset the the list of files to load
        subset_files <- all_data_files[id]
        
        df <- data.frame(matrix(nrow=length(subset_files),ncol=2))
        colnames(df) <- c('id','nobs')
        
        for (i in 1:length(subset_files)) {
            tmp_df <- read.csv(subset_files[i]) # read the data
            
            ##
            ## Since the names of the data files, are monitor IDs, and they are of a standard format,
            ## I parsed out the ID# from the the pathname
            ##
            df[i,1] <- as.integer(substr(subset_files[i], nchar(subset_files[i])-6,nchar(subset_files[i])-4))
            df[i,2] <- (sum(complete.cases(tmp_df)))
        }
        
        ## create a list to contain the data from the specified data files
##        tmp_list <- vector(mode="list", length(subset_of_data_files))
        
        ## Iterate through the list of data file names and read in the data
##        tmp_list <- lapply(subset_of_data_files, read.csv)
        
        df
}