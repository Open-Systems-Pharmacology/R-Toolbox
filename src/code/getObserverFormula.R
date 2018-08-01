
getObserverFormula <- function(path_id = "*", options = {}, DCI_Info = {})
{
	if (length(options) == 0)
	{
		options <- list(Property = "Formula", Index = numeric(0), isReference = FALSE)
	}
	if (length(grep("Property",names(options), fixed =TRUE)) == 0)
	{
		options[[length(options)+1]] <- "Formula"
		names(options)[length(options)] <- "Property"
	}
	if (length(grep("Index",names(options), fixed =TRUE)) == 0)
	{
		options[[length(options)+1]] <- numeric(0)
		names(options)[length(options)] <- "Index"
	}
  if (length(grep("isReference",names(options), fixed =TRUE)) == 0)
  {
    options[[length(options)+1]] <- FALSE
    names(options)[length(options)] <- "isReference"
  }
	if (length(DCI_Info) == 0)
	{
		stop("No DCI_Info provided.")
	}
	if (is.character(path_id) && path_id == "")
	{
		stop("Empty path_id provided.")
	}
	if (!(options$Property %in% c("ID", "Unit", "Formula", "Path"))	)
	{
		stop("Invalid property provided. Please use one of the following: ID, Unit, Formula, Path.")
	}
  
	iTab <- which(names(DCI_Info$InputTab) == "AllObservers")
	
	if (length(options$Index) > 0)
	{
	  indx <- options$Index		
	} else 
  {
	  indx <- findTableIndex(path_id = path_id, tableID = iTab, options$isReference, DCI_Info = DCI_Info)
	}
	
	if (length(indx) == 0)
	{
	warning(paste("Observer with path_id", path_id, "does not exist!"))
	  return(list(Value = NULL, Index = NULL));
	}
  
	if (options$isReference == FALSE)
	{
	  Table <- DCI_Info$InputTab[[iTab]]
	} else {
	  Table <- DCI_Info$ReferenceTab[[iTab]]	
	}
  
  iCol <- which(names(Table) == options$Property)

	return(list(Value = Table[[iCol]][indx], Index = indx))
}
