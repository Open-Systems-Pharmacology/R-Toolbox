
setRelativeParameter <- function(value = numeric(0), path_id = "*", options = {}, DCI_Info = {})
{
	if (length(options) == 0)
	{
		options <- list(Type = "variable", Property = "Value", Index=numeric(0))
	}
	if (length(grep("Type",names(options), fixed =TRUE)) == 0)
	{
		options[[length(options)+1]] <- "variable"
		names(options)[length(options)] <- "Type"
	}
	if (length(grep("Property",names(options), fixed =TRUE)) == 0)
	{
		options[[length(options)+1]] <- "Value"
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
	if (length(value) == 0)
	{
		stop("No value provided")
	}
	if (is.numeric(path_id) & length(value) != length(path_id))
	{
		stop("Number of values does not match path_id passed.")
	}
	if (!is.numeric(value))
	{
		stop(paste("Variable value was not provided a numeric value but", value, "."))
	}
	if (!(options$Property %in% c("Value"))	)
	{
		stop("Invalid property provided. Use one of the following: Value.")
	}
  if (!(toupper(options$Type) %in% c("VARIABLE", "REFERENCE"))	)
	{
	  stop("Invalid type provided. Use one of the following: variable, reference.")
	}
  
  iTab <- which(names(DCI_Info$InputTab) == "VariableParameters")

	if (toupper(options$Type) == "REFERENCE")
	{
		Table <- DCI_Info$InputTab[[iTab]]
	} else {
		Table <- DCI_Info$ReferenceTab[[iTab]]	
	}
	
	if (length(options$Index) > 0)
	{
	  indx <- options$Index
	} else {
	  indx <- findTableIndex(path_id = path_id, tableID = iTab, isReference = (toupper(options$Type) == "REFERENCE"), DCI_Info = DCI_Info)	
	}
	
	if (length(indx) == 0)
	{
		stop(paste("Parameter with path_id", path_id, "does not exist for specified type", options$Type))
	}
  
	if (length(which(Table$ParameterType[indx] == "Table")) > 0) {
	  warning("Parameters of type table have been ignored!")
	  indx <- which(Table$ParameterType[indx] != "Table")
	}
  
  iCol <- which(names(Table) == options$Property)
	Table[[iCol]][indx] <- Table[[iCol]][indx]*value
	if (toupper(options$Type) != "REFERENCE")
	{
		DCI_Info$InputTab[[iTab]] <- Table
	} else {
		DCI_Info$ReferenceTab[[iTab]] <- Table	
	}		
	return(DCI_Info)
}

