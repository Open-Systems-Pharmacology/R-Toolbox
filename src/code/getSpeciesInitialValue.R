
getSpeciesInitialValue <- function(path_id = "*", options = {}, DCI_Info = {})
{
	if (length(options) == 0)
	{
		options <- list(Type = "current", Property = "InitialValue", Index = numeric(0))
	}
	if (length(grep("Type",names(options), fixed =TRUE)) == 0)
	{
		options[[length(options)+1]] <- "current"
		names(options)[length(options)] <- "Type"
	}
	if (length(grep("Property",names(options), fixed =TRUE)) == 0)
	{
		options[[length(options)+1]] <- "InitialValue"
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
	if (!(options$Property %in% c("InitialValue", "ID", "Unit", "Formula", "IsFormula", "ScaleFactor", "Path"))	)
	{
		stop("Invalid property provided. Use one of the following: InitialValue, ID, Unit, Formula, IsFormula, ScaleFactor, Path.")
	}
	if (!(toupper(options$Type) %in% c("VARIABLE", "READONLY", "CURRENT", "REFERENCE"))	)
	{
	  stop("Invalid type provided. Use one of the following: variable, readonly, current, reference.")
	}
  
	iTab <- switch(toupper(options$Type),
			READONLY =which(names(DCI_Info$InputTab) == "AllSpecies"),
      CURRENT = which(names(DCI_Info$InputTab) == "VariableSpecies"),
      REFERENCE = which(names(DCI_Info$InputTab) == "VariableSpecies"),
      VARIABLE = which(names(DCI_Info$InputTab) == "VariableSpecies"))
	
	if (length(options$Index) > 0)
	{
	  indx <- options$Index
	} else {
	  indx <- findTableIndex(path_id = path_id, tableID = iTab, isReference = (toupper(options$Type) == "REFERENCE"), DCI_Info = DCI_Info)
	}
	
	# for parameter type current search also readonly options if not found in variable options
	if ((length(indx) == 0) & (toupper(options$Type) == "CURRENT"))
	{
	  iTab = which(names(DCI_Info$InputTab) == "AllSpecies");
	  indx <- findTableIndex(path_id = path_id, tableID = iTab, isReference= (toupper(options$Type) == "REFERENCE"), DCI_Info = DCI_Info)
	}
	if (length(indx) == 0)
	{
	  warning(paste("Table parameter with path_id", path_id, "does not exist for specified parameter type", options$Type))
	  return(list(Value = NULL, Index = NULL));
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
