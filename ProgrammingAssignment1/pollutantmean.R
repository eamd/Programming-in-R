pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        ## NOTE: Do not round the result!


        ## check that the directory entered is a valid directory
        
        ## Check to make sure the correct directory is entered
        ## If the directory can not be found, exit gracefully
        if(!dir.exists(directory))
        {
            return('The directory/folder entered is incorrect')
        }

        ## Check to make sure the pollutant is entered correctly.
        ## If something other than sulfate or nitrate is entered,
        ## exit gracefully.
        if((!pollutant == 'sulfate') & (!pollutant == 'nitrate'))
        {
            return(paste(pollutant,"is not a valid pollutant. Acceptable values are 'nitrate' or 'sulfate'"))    
        }
        
        ## Load the names of the data files found in the specified directory
        all_datafiles <- list.files(directory, full.names=TRUE)   #creates a list of files names
        
        ## Use the ID to subset the the list of files to load
        subset_of_data_files <- all_datafiles[id]
        
        ## create a list to contain the data from the specified data files
        tmp_list <- vector(mode="list", length(subset_of_data_files))
        
        ## Iterate through the list of data file names and read in the data
        tmp_list <- lapply(subset_of_data_files, read.csv)
        
        ## Perform a row bind for all the loaded data frames into one data frame
        df <- do.call(rbind, tmp_list)
        
        ## Get the position ID for the specified pollutant
        pol_index <- match(pollutant, names(df))
        
        ## Create a vector of the column of data specified
        pol_data <- c(df[[pol_index]])
        
        ## Return the calculated mean for the data
        return(mean(pol_data, na.rm=TRUE))
}
