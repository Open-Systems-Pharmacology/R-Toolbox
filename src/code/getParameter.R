
getParameter <- function(path_id = "*", options = {}, DCI_Info = {})
{
	if (length(options) == 0)
	{
		options <- list(Type = "current", Property = "Value", Index = numeric(0))
	}
	if (length(grep("Type",names(options), fixed =TRUE)) == 0)
	{
		options[[length(options)+1]] <- "current"
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
	if (length(grep("TimeProfile",names(options), fixed =TRUE)) == 0)
	{
	  options[[length(options)+1]] <- numeric(0)
	  names(options)[length(options)] <- "TimeProfile"
	}
	if (length(DCI_Info) == 0)
	{
		stop("No DCI_Info provided.")
	}
	if (is.character(path_id) && path_id == "")
	{
		stop("Empty path_id provided.")
	}
	if (!(options$Property %in% c("Value", "ID", "Path", "Unit", "Formula", "ParameterType", "IsFormula", "IsTable", "IsValue"))	)
	{
		stop("Invalid property provided. Use one of the following: Value, ID, Path, Unit, Formula, ParamterType, IsFormula, IsTable, IsValue.")
	}
	if (!(toupper(options$Type) %in% c("VARIABLE", "READONLY", "CURRENT", "REFERENCE"))	)
	{
	  stop("Invalid type provided. Use one of the following: variable, readonly, current, reference.")
	}
	if (!is.numeric(options$TimeProfile) & options$Property == "Value") {
	  stop("Invalid time profile provided. Please specify a numerical vector.")
	}
	
	iTab <- switch(toupper(options$Type),
			READONLY=which(names(DCI_Info$InputTab) == "AllParameters"),
			CURRENT=which(names(DCI_Info$InputTab) == "VariableParameters"),
      REFERENCE=which(names(DCI_Info$InputTab) == "VariableParameters"),
	    VARIABLE=which(names(DCI_Info$InputTab) == "VariableParameters"))
	
	if (length(options$Index) > 0)
	{
	  indx <- options$Index
	} else {
	  indx <- findTableIndex(path_id = path_id, tableID = iTab, isReference = (toupper(options$Type) == "REFERENCE"), DCI_Info = DCI_Info)
	}
	
	# for parameter type current search also readonly options if not found in variable options
	if ((length(indx) == 0) & (toupper(options$Type) == "CURRENT"))
	{
		iTab = which(names(DCI_Info$InputTab) == "AllParameters");
		indx <- findTableIndex(path_id = path_id, tableID = iTab, isReference= (toupper(options$Type) == "REFERENCE"), DCI_Info = DCI_Info)
	}
	
	if (length(indx) == 0)
	{
	  warning(paste("Parameter with path_id", path_id, "does not exist for specified parameter type", options$Type))
	  return(list(Value = NULL, Index = NULL));
	}
  
	if (toupper(options$Type) != "REFERENCE")
	{
	  Table <- DCI_Info$InputTab[[iTab]]
	} else {
	  Table <- DCI_Info$ReferenceTab[[iTab]]	
	}
  
  if (options$Property == "IsFormula" | options$Property == "IsTable" | options$Property == "IsValue") {
    if (options$Property == "IsFormula") {
      return (list(Value = as.numeric(Table$ParameterType[indx] %in% "Formula"), Index = indx))
    }
    if (options$Property == "IsTable") {
      return (list(Value = as.numeric(Table$ParameterType[indx] %in% "Table"), Index = indx))      
    }
    if (options$Property == "IsValue") {
      return (list(Value = as.numeric(Table$ParameterType[indx] %in% "Value"), Index = indx))
    }
  } else {  
    if (options$Property == "Value") {
      iCol <- which(names(Table) == options$Property)
      values <- Table[[iCol]][indx]     
      
      if (length(which(Table$ParameterType[indx] == "Table")) > 0 & length(options$TimeProfile) == 0) {
        warning("At least one parameters is a table parameters but no time profile has been specified.")
        options$TimeProfile <- 0
      }
      
      iParameterTable <- switch(toupper(options$Type),
                     READONLY=which(names(DCI_Info$InputTab) == "AllTableParameters"),
                     CURRENT=which(names(DCI_Info$InputTab) == "VariableTableParameters"),
                     REFERENCE=which(names(DCI_Info$InputTab) == "VariableTableParameters"),
                     VARIABLE=which(names(DCI_Info$InputTab) == "VariableTableParameters"))
      
      if (toupper(options$Type) != "REFERENCE")
      {
        ParameterTable <- DCI_Info$InputTab[[iParameterTable]]
      } else {
        ParameterTable <- DCI_Info$ReferenceTab[[iParameterTable]]	
      }
      
      tableindx <- intersect(indx, which(Table$ParameterType == "Table"))

      for (i in tableindx) {
        id <- Table$ID[i]
        x <- ParameterTable$Time[which(ParameterTable$ID == id)]
        y <- ParameterTable$Value[which(ParameterTable$ID == id)]
        
        # for parameter type current search also readonly options if not found in variable options
        if ((length(x) == 0) & (toupper(options$Type) == "CURRENT"))
        {
          iParameterTable = which(names(DCI_Info$InputTab) == "AllTableParameters");
          if (toupper(options$Type) != "REFERENCE")
          {
            ParameterTable <- DCI_Info$InputTab[[iParameterTable]]
          } else {
            ParameterTable <- DCI_Info$ReferenceTab[[iParameterTable]]	
          }
          x <- ParameterTable$Time[which(ParameterTable$ID == id)]
          y <- ParameterTable$Value[which(ParameterTable$ID == id)]
        }
        
        newy <- approx(x=x, y=y, xout=options$TimeProfile, method="linear", rule=2)$y
        if (length(options$TimeProfile) > 1) 
        {
          values[which(indx == i)] <- list(newy)
        } else
        {
          values[which(indx == i)] <- newy
        }
      }
      return(list(Value = values, Index = indx))      
    } else {
      iCol <- which(names(Table) == options$Property)
      return(list(Value = Table[[iCol]][indx], Index = indx))
    }
  }
  
}
