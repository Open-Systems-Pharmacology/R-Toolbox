
existsSpeciesInitialValue <- function(path_id = "*", options = {}, DCI_Info = {})
{
	if (is.character(path_id) && path_id == "")
	{
		stop("Input 'path_id' is missing.")
	}
	
	if (length(DCI_Info) == 0)
	{
		stop("DCI_Info is missing.")
	}

	if (length(options) == 0)
	{
		options <- list(isReference = FALSE, Type = "readOnly")
	} else {
		if (length(grep('isReference', names(options), fixed = TRUE))==0)
		{
			options<-c(options, isReference = FALSE)
		}
		if (length(grep('Type', names(options), fixed = TRUE))==0)
		{
			options<-c(options, Type = "readOnly")
		}
	}
	
	if (!(toupper(options$Type) %in% c("VARIABLE", "READONLY"))	)
	{
	  stop("Invalid type provided. Use one of the following: variable, readonly.")
	}
  
	id <- grep(toupper(options$Type),c("READONLY", "VARIABLE"), fixed = TRUE)+2
	if (options$isReference == FALSE)
	{
		Table <- DCI_Info[[3]][[id]]
	} else {
		Table <- DCI_Info[[4]][[id]]
	}
	indx <- findTableIndex(path_id = path_id, tableID = id, isReference = options$isReference, DCI_Info = DCI_Info)
	if (length(indx) == 0)
	{
		isExisting <- FALSE
	} else {
		isExisting <- TRUE
	}
	return(list(isExisting = isExisting, Index = indx, ID = Table$ID[indx], Path = Table$Path[indx]))
}	


