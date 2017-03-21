
getObserverFormula <- function(path_id = "*", options = {}, DCI_Info = {})
{
	if (length(options) == 0)
	{
		options <- list(Type = "current", Property = "Formula", Index = numeric(0))
	}
	if (length(grep("Type",names(options), fixed =TRUE)) == 0)
	{
		options[[length(options)+1]] <- "current"
		names(options)[length(options)] <- "Type"
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
	if (!(toupper(options$Type) %in% c("CURRENT", "READONLY", "REFERENCE"))	)
	{
	  stop("Invalid type provided. Please use one of the following: current, readonly, reference.")
	}
  
	iTab <- which(names(DCI_Info$InputTab) == "AllObservers")
	
	if (length(options$Index) > 0)
	{
	  indx <- options$Index		
	} else 
  {
	  indx <- findTableIndex(path_id = path_id, tableID = iTab, isReference = (toupper(options$Type) == "REFERENCE"), DCI_Info = DCI_Info)
	}
	
	if (length(indx) == 0)
	{
		stop(paste("Observer with path_id", path_id, "does not exist for specified type", options$Type))
	}
  
	if (toupper(options$Type) != "REFERENCE")
	{
	  Table <- DCI_Info$InputTab[[iTab]]
	} else {
	  Table <- DCI_Info$ReferenceTab[[iTab]]	
	}
  
  iCol <- which(names(Table) == options$Property)

	return(list(Value = Table[[iCol]][indx], Index = indx))
}
