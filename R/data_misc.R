###############################################################################
# Data Organization, Reading/Writing
###############################################################################

#' Read all CSV files found in search string
#'
#' This function uses Sys.glob to search for files matching a given string and then cycles through those files, reading them in as CSVs. This function assumes that all your CSV files are the same format or at least can be 'rbind'ed together.
#' @param search_str string to pass to Sys.glob for the search
#' @param add_file_num Optional: add an additional column (file_num) to indicate the count in the list of files being imported
#' @param debug Optional: print out debugging messages during operation
#' @keywords CSV
#' @export
#' @examples
#' all_CSVs = read.csv.all('directory/*/*.csv')

read.csv.all <- function(search_str, add_file_num = F, debug = F) {
	require(tidyverse);
	csv_files = Sys.glob(search_str);
  
	if (length(csv_files) == 0) {
	  warning(c('Warning: found no files to read, searched ',search_str))
	}
	
	full_set = c();
	file_count = 0;
	for (this_file in csv_files) {
		if (debug) {
			 print(paste0('Working on this file: ',this_file)) 
		}
		file_count = file_count + 1;
		try({
			this_set = read.csv(this_file);
			if(add_file_num) {
				this_set = mutate(this_set, file_num = file_count);
			}	
			full_set = rbind(full_set,this_set);
			
		})
	}
	return(full_set);
}
