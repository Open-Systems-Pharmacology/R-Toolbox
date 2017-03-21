
findTableIndex <- function(path_id = numeric(0), tableID = numeric(0), isReference = FALSE, DCI_Info = numeric(0))
{
	## Check that all input arguments are passed
	if (length(path_id) == 0)
	{
		stop("Input 'path_id' is missing.")
	}
	
	if (length(tableID) == 0)
	{
		stop("Input 'tableID' is missing.")
	}
	
	if (length(DCI_Info) == 0)
	{
		stop("Input 'DCI_Info' is missing.")
	}
	
	## Which Table should we operate on?
	if (isReference == TRUE)
	{
		Table <- DCI_Info$ReferenceTab[[tableID]]
	} else {
		Table <- DCI_Info$InputTab[[tableID]]
	}
	
	if (is.numeric(path_id))
	{
		indx <- which(Table$ID %in% path_id)
	} else {
    counts <- length(Table[[names(Table)[1]]])
    if (counts > 0) {
  		indx <- 1:counts
  		if (path_id != '*')
  		{
        searchValue <- path_id
        searchValue <- gsub('|', '\\|', searchValue, fixed = TRUE)
        searchValue <- gsub('&', '\\&', searchValue, fixed = TRUE)
        searchValue <- gsub('*', '.*', searchValue, fixed = TRUE)
        searchValue <- paste("^", searchValue, "$", sep="")
        searchPathes <- Table$Path

        indx <- grep(searchValue, searchPathes, perl = TRUE)                     
  		}
    } else
    {
      indx <- 0
    }
	}
	return(indx)	
}
