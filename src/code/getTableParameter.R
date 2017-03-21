
getTableParameter <- function(path_id = "*", options = {}, DCI_Info = {})
{
  if (length(options) == 0)
  {
    options <- list(Type = "current", Index = numeric(0))
  }
  if (length(grep("Type",names(options), fixed =TRUE)) == 0)
  {
    options[[length(options)+1]] <- "current"
    names(options)[length(options)] <- "Type"
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
  if (!(toupper(options$Type) %in% c("VARIABLE", "READONLY", "CURRENT", "REFERENCE"))	)
  {
    stop("Invalid type provided. Use one of the following: variable, readonly, current, reference.")
  }
  
  iTab <- switch(toupper(options$Type),
                 READONLY=which(names(DCI_Info$InputTab) == "AllParameters"),
                 CURRENT=which(names(DCI_Info$InputTab) == "VariableParameters"),
                 REFERENCE=which(names(DCI_Info$InputTab) == "VariableParameters"),
                 VARIABLE=which(names(DCI_Info$InputTab) == "VariableParameters"))

  iParameterTable <- switch(toupper(options$Type),
                            READONLY=which(names(DCI_Info$InputTab) == "AllTableParameters"),
                            CURRENT=which(names(DCI_Info$InputTab) == "VariableTableParameters"),
                            REFERENCE=which(names(DCI_Info$InputTab) == "VariableTableParameters"),
                            VARIABLE=which(names(DCI_Info$InputTab) == "VariableTableParameters"))
  
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
    iParameterTable = which(names(DCI_Info$InputTab) == "AllTableParameters");
  }
  
  if (length(indx) == 0)
  {
    stop(paste("Parameter with path_id", path_id, "does not exist for specified parameter type", options$Type))
  }

  if (toupper(options$Type) != "REFERENCE")
  {
    parameterTable <- DCI_Info$InputTab[[iParameterTable]]
    Table <- DCI_Info$InputTab[[iTab]]
    
  } else {
    parameterTable <- DCI_Info$ReferenceTab[[iParameterTable]]	
    Table <- DCI_Info$ReferenceTab[[iTab]]  
  }

  if (!("Table" %in% unique(Table$ParameterType[indx]))) {
    warning(paste("Table parameter with path_id", path_id, "does not exist for specified parameter type", options$Type))
  }

  idx <- which(parameterTable$ID %in% Table$ID[indx])
  retVal <- {}
  
  retVal$ID <- parameterTable$ID[idx]
  attributes(retVal$ID) <- attributes(parameterTable$ID)

  retVal$Time <- parameterTable$Time[idx]
  attributes(retVal$Time) <- attributes(parameterTable$Time)

  retVal$Value <- parameterTable$Value[idx]
  attributes(retVal$Value) <- attributes(parameterTable$Value)

  retVal$RestartSolver <- parameterTable$RestartSolver[idx]
  attributes(retVal$RestartSolver) <- attributes(parameterTable$RestartSolver)
  
  return(retVal)
}
