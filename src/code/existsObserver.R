
existsObserver <- function(path_id = "*",  options = {}, DCI_Info = {})
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
		options<-data.frame(isReference = FALSE)
	} else {
		if (length(grep('isReference', names(options),fixed = TRUE))==0)
		{
			options<-c(options, isReference = FALSE)
		}
	}
  
  id <- 5
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
