
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
  		  # As escaping of certain symbols in path names had to be done manually in previous releases but is performed
  		  # automatically now, a warning message is displayed when manual escaping is detected. This warning message
  		  # should be removed in future.
  		  for (symbol in c("\\(", "\\)", "\\[", "\\]")){
  		    if (grepl(symbol, path_id, fixed = TRUE)){
  		      warning(paste(path_id, ": Escaping of the characters '(', ')', '[', and '[' in path names should not be done manually!"), immediate. = TRUE )
  		      break
  		    }
  		  }
        searchValue <- path_id
        searchValue <- gsub('|', '\\|', searchValue, fixed = TRUE)
        searchValue <- gsub('&', '\\&', searchValue, fixed = TRUE)
        searchValue <- gsub('(', '\\(', searchValue, fixed = TRUE)
        searchValue <- gsub(')', '\\)', searchValue, fixed = TRUE)
        searchValue <- gsub('[', '\\[', searchValue, fixed = TRUE)
        searchValue <- gsub(']', '\\]', searchValue, fixed = TRUE)
        searchValue <- gsub('*', '.*', searchValue, fixed = TRUE)
        searchValue <- paste("^", searchValue, "$", sep="")
        searchPathes <- Table$Path

        indx <- grep(searchValue, searchPathes, perl = TRUE)                     
  		}
    }
    # If no parameter found, return a zero-lenght integer vector to match the behavior of which().
    # This way, a not found parameter should be recognized by checking for (length(indx) == 0)
    else{
      indx <- integer(0);
    }
	}
	return(indx)	
}
