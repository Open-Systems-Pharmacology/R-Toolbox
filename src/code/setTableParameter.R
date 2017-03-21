
setTableParameter <- function(table = {}, options = {}, DCI_Info = {})
{
  if (length(options) == 0)
  {
    options <- list(Type = "variable")
  }
  if (length(grep("Type",names(options), fixed =TRUE)) == 0)
  {
    options[[length(options)+1]] <- "variable"
    names(options)[length(options)] <- "Type"
  }
  if (length(DCI_Info) == 0)
  {
    stop("No DCI_Info provided.")
  }
  if (length(names(table)) != 4) {
    stop("Provided table is invalid. Please use getTableParameter to get a well formatted list.")    
  }
  if (names(table)[1] != "ID")
  {
    stop("Provided table is invalid. Column ID is missing. Please use getTableParameter to get a well formatted list.")
  }
  if (names(table)[2] != "Time")
  {
    stop("Provided table is invalid. Column Time is missing. Please use getTableParameter to get a well formatted list.")
  }
  if (names(table)[3] != "Value")
  {
    stop("Provided table is invalid. Column Value is missing. Please use getTableParameter to get a well formatted list.")
  }
  if (names(table)[4] != "RestartSolver")
  {
    stop("Provided table is invalid. Column RestartSolver is missing. Please use getTableParameter to get a well formatted list.")
  }
  
  if (!(toupper(options$Type) %in% c("VARIABLE", "REFERENCE"))	)
  {
    stop("Invalid type provided. Use one of the following: variable, reference.")
  }
  
  iTab <- which(names(DCI_Info$InputTab) == "VariableParameters")
  iParameterTable <- which(names(DCI_Info$InputTab) == "VariableTableParameters")
  
  if (toupper(options$Type) != "REFERENCE")
  {
    Table <- DCI_Info$InputTab[[iTab]]
    parameterTable <- as.data.frame(DCI_Info$InputTab[[iParameterTable]])
  } else {
    Table <- DCI_Info$ReferenceTab[[iTab]]
    parameterTable <- as.data.frame(DCI_Info$ReferenceTab[[iParameterTable]])
  }
  
  if (length(which(unique(table$ID %in% Table$ID[which(Table$ParameterType != "Table")])))) {
    stop("There are time profiles for ids which are no table parameters.")
  }
            
  #delete entries
  newEntries <- which(parameterTable$ID %in% table$ID)
  if (length(newEntries) == 0) {
    stop("No parameters would be effected!")
  }
  
  #delete current entries
  parameterTable <- parameterTable[-newEntries,]
  
  #sort new entries
  ordering <- order(table$ID, table$Time)
  sortedTable <- as.data.frame(table)[ordering,]
  
  #append new entries
  parameterTable <- rbind(parameterTable, sortedTable)

  #take over attributes
  parameterTable <- as.list(parameterTable)
  attributes(parameterTable$ID) <- attributes(table$ID)
  attributes(parameterTable$Time) <- attributes(table$Time)
  attributes(parameterTable$Value) <- attributes(table$Value)
  attributes(parameterTable$RestartSolver) <- attributes(table$RestartSolver)
  
  #write new table to info object
  if (toupper(options$Type) != "REFERENCE")
  {
    DCI_Info$InputTab[[iParameterTable]] <- parameterTable
  } else {
    DCI_Info$ReferenceTab[[iParameterTable]] <- parameterTable
  }		
  return(DCI_Info)
}
