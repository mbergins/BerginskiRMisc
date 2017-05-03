###############################################################################
# Data Organization, Reading/Writing
###############################################################################

#' Read all CSV files found in search string
#'
#' This function uses Sys.glob to search for files matching a given string and then cycles through those files, reading them in as CSVs. This function assumes that all your CSV files are the same format or at least can be 'rbind'ed together.
#' @param search_str string to pass to Sys.glob for the search
#' @keywords CSV
#' @export
#' @examples
#' all_CSVs = read.csv.all('directory/*/*.csv')

read.csv.all <- function(search_str) {
	csv_files = Sys.glob(search_str);

	full_set = c();
	for (this_file in csv_files) {
		try({full_set = rbind(full_set,read.csv(this_file))})
	}
	return(full_set);
}
