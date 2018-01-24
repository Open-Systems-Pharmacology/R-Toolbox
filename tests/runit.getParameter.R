require(RUnit, quietly=TRUE)
require(MoBiToolboxForR, quietly=TRUE)
simModelXML <- "./models/black american girl.xml"
tableParamXML <- "./models/TableParameters.xml"
standard_dci_info <- initSimulation(XML=simModelXML, whichInitParam="none")
tableParam_dci_info <- initSimulation(XML=tableParamXML, whichInitParam="all", SimulationNumber=2)

test.EmptyPathID <- function() {
  dci_info <- standard_dci_info
  checkException(getParameter(path_id="", DCI_Info = dci_info))
}

test.EmptyDCI_Info <- function() {
  checkException(getParameter(path_id=113, DCI_Info = {}))
}

test.PropertyUnknown <- function() {
  dci_info <- standard_dci_info
  options <- list(Property="Unknown")
  checkException(getParameter(path_id=113, options=options , DCI_Info = dci_info))
}

test.CheckParameterInSimulation <- function() {
  paramList <- {}
  paramList <- initParameter(initStruct = paramList, path_id=113)
  dci_info <- initSimulation(XML = simModelXML, ParamList = paramList)
  
  options <- list(Type="readonly")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkEquals(names(parameter), c("Value","Index"))
  checkEquals(dci_info$InputTab$AllParameters$Value[parameter$Index], parameter$Value)

  options <- list(Type="current")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkEquals(names(parameter), c("Value","Index"))
  checkEquals(dci_info$InputTab$VariableParameters$Value[parameter$Index], parameter$Value)
  
  options <- list(Type="variable")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkEquals(names(parameter), c("Value","Index"))
  checkEquals(dci_info$InputTab$VariableParameters$Value[parameter$Index], parameter$Value)

  options <- list(Type="reference")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkEquals(names(parameter), c("Value","Index"))
  checkEquals(dci_info$ReferenceTab$VariableParameters$Value[parameter$Index], parameter$Value)
  
  options <- list(Type="variable")
  checkException(getParameter(path_id=114, DCI_Info = dci_info, options=options))
  
  options <- list(Type="current")
  parameter <- getParameter(path_id=114, DCI_Info = dci_info, options=options)
  checkEquals(names(parameter), c("Value","Index"))
  checkEquals(dci_info$InputTab$AllParameters$Value[parameter$Index], parameter$Value)
}

test.CheckParameterVectorInSimulation <- function() {
  paramList <- {}
  paramList <- initParameter(initStruct = paramList, path_id= 113)
  paramList <- initParameter(initStruct = paramList, path_id= 114)
  dci_info <- initSimulation(XML = simModelXML, ParamList = paramList)
  
  options <- list(Type="readonly")
  parameter <- getParameter(path_id=c(113, 114), DCI_Info = dci_info, options=options)
  checkEquals(names(parameter), c("Value","Index"))
  checkEquals(dci_info$InputTab$AllParameters$Value[parameter$Index], parameter$Value)
  
  options <- list(Type="current")
  parameter <- getParameter(path_id=c(113, 114), DCI_Info = dci_info, options=options)
  checkEquals(names(parameter), c("Value","Index"))
  checkEquals(dci_info$InputTab$VariableParameters$Value[parameter$Index], parameter$Value)
  
  options <- list(Type="variable")
  parameter <- getParameter(path_id=c(113, 114), DCI_Info = dci_info, options=options)
  checkEquals(names(parameter), c("Value","Index"))
  checkEquals(dci_info$InputTab$VariableParameters$Value[parameter$Index], parameter$Value)
  
  options <- list(Type="reference")
  parameter <- getParameter(path_id=c(113, 114), DCI_Info = dci_info, options=options)
  checkEquals(names(parameter), c("Value","Index"))
  checkEquals(dci_info$ReferenceTab$VariableParameters$Value[parameter$Index], parameter$Value)
}

test.CheckParameterVectorIndexInSimulation <- function() {
  paramList <- {}
  paramList <- initParameter(initStruct = paramList, path_id= 113)
  paramList <- initParameter(initStruct = paramList, path_id= 114)
  dci_info <- initSimulation(XML = simModelXML, ParamList = paramList)
  
  options <- list(Type="readonly")
  index <- existsParameter(path_id=c(113,114), options=options, DCI_Info=dci_info)
  options <- list(Type="readonly", Index=index$Index)
  parameter <- getParameter(DCI_Info = dci_info, options=options)
  checkEquals(names(parameter), c("Value","Index"))
  checkEquals(dci_info$InputTab$AllParameters$Value[parameter$Index], parameter$Value)
  
  options <- list(Type="variable")
  index <- existsParameter(path_id=c(113,114), options=options, DCI_Info=dci_info)
  options <- list(Type="current", Index=index$Index)
  parameter <- getParameter(DCI_Info = dci_info, options=options)
  checkEquals(names(parameter), c("Value","Index"))
  checkEquals(dci_info$InputTab$VariableParameters$Value[parameter$Index], parameter$Value)
  
  options <- list(Type="variable")
  index <- existsParameter(path_id=c(113,114), options=options, DCI_Info=dci_info)
  options <- list(Type="variable", Index=index$Index)
  parameter <- getParameter(DCI_Info = dci_info, options=options)
  checkEquals(names(parameter), c("Value","Index"))
  checkEquals(dci_info$InputTab$VariableParameters$Value[parameter$Index], parameter$Value)
  
  options <- list(Type="variable", isReference=TRUE)
  index <- existsParameter(path_id=c(113,114), options=options, DCI_Info=dci_info)
  options <- list(Type="reference", Index=index$Index)
  parameter <- getParameter(DCI_Info = dci_info, options=options)
  checkEquals(names(parameter), c("Value","Index"))
  checkEquals(dci_info$ReferenceTab$VariableParameters$Value[parameter$Index], parameter$Value)
}

test.CheckPropertiesCurrent <- function() {
  paramList <- {}
  paramList <- initParameter(initStruct = paramList, path_id=113)
  dci_info <- initSimulation(XML = simModelXML, ParamList = paramList)
  
  options <- list(Type="current", Property = "Value")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$VariableParameters$Value[parameter$Index])

  options <- list(Type="current", Property = "ID")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$VariableParameters$ID[parameter$Index])
  
  options <- list(Type="current", Property = "Path")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$VariableParameters$Path[parameter$Index])

  options <- list(Type="current", Property = "Unit")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$VariableParameters$Unit[parameter$Index])
  
  options <- list(Type="current", Property = "Formula")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$VariableParameters$Formula[parameter$Index])

  options <- list(Type="current", Property = "ParameterType")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$VariableParameters$ParameterType[parameter$Index])

  options <- list(Type="current", Property = "IsFormula")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, as.numeric(dci_info$InputTab$VariableParameters$ParameterType[parameter$Index] == "Formula"))

  options <- list(Type="current", Property = "IsTable")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, as.numeric(dci_info$InputTab$VariableParameters$ParameterType[parameter$Index] == "Table"))
  
  options <- list(Type="current", Property = "IsValue")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, as.numeric(dci_info$InputTab$VariableParameters$ParameterType[parameter$Index] == "Value"))

  options <- list(Type="current", Property = "Unknown")
  checkException(getParameter(path_id=113, DCI_Info = dci_info, options=options))
}  

test.CheckPropertiesVariable <- function() {
  paramList <- {}
  paramList <- initParameter(initStruct = paramList, path_id=113)
  dci_info <- initSimulation(XML = simModelXML, ParamList = paramList)
  
  options <- list(Type="variable", Property = "Value")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$VariableParameters$Value[parameter$Index])
  
  options <- list(Type="variable", Property = "ID")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$VariableParameters$ID[parameter$Index])
  
  options <- list(Type="variable", Property = "Path")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$VariableParameters$Path[parameter$Index])
  
  options <- list(Type="variable", Property = "Unit")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$VariableParameters$Unit[parameter$Index])
  
  options <- list(Type="variable", Property = "Formula")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$VariableParameters$Formula[parameter$Index])
  
  options <- list(Type="variable", Property = "ParameterType")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$VariableParameters$ParameterType[parameter$Index])
  
  options <- list(Type="variable", Property = "IsFormula")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, as.numeric(dci_info$InputTab$VariableParameters$ParameterType[parameter$Index] == "Formula"))
  
  options <- list(Type="variable", Property = "IsTable")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, as.numeric(dci_info$InputTab$VariableParameters$ParameterType[parameter$Index] == "Table"))
  
  options <- list(Type="variable", Property = "IsValue")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, as.numeric(dci_info$InputTab$VariableParameters$ParameterType[parameter$Index] == "Value"))

  options <- list(Type="variable", Property = "Unknown")
  checkException(getParameter(path_id=113, DCI_Info = dci_info, options=options))
}  

test.CheckPropertiesReference <- function() {
  paramList <- {}
  paramList <- initParameter(initStruct = paramList, path_id=113)
  dci_info <- initSimulation(XML = simModelXML, ParamList = paramList)
  
  options <- list(Type="reference", Property = "Value")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$ReferenceTab$VariableParameters$Value[parameter$Index])
  
  options <- list(Type="reference", Property = "ID")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$ReferenceTab$VariableParameters$ID[parameter$Index])
  
  options <- list(Type="reference", Property = "Path")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$ReferenceTab$VariableParameters$Path[parameter$Index])
  
  options <- list(Type="reference", Property = "Unit")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$ReferenceTab$VariableParameters$Unit[parameter$Index])
  
  options <- list(Type="reference", Property = "Formula")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$ReferenceTab$VariableParameters$Formula[parameter$Index])
  
  options <- list(Type="reference", Property = "ParameterType")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$ReferenceTab$VariableParameters$ParameterType[parameter$Index])
  
  options <- list(Type="reference", Property = "IsFormula")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, as.numeric(dci_info$ReferenceTab$VariableParameters$ParameterType[parameter$Index] == "Formula"))
  
  options <- list(Type="reference", Property = "IsTable")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, as.numeric(dci_info$ReferenceTab$VariableParameters$ParameterType[parameter$Index] == "Table"))
  
  options <- list(Type="reference", Property = "IsValue")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, as.numeric(dci_info$ReferenceTab$VariableParameters$ParameterType[parameter$Index] == "Value"))
  
  options <- list(Type="reference", Property = "Unknown")
  checkException(getParameter(path_id=113, DCI_Info = dci_info, options=options))
}  

test.CheckPropertiesReadonly <- function() {
  dci_info <- standard_dci_info
  
  options <- list(Type="readonly", Property = "Value")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$AllParameters$Value[parameter$Index])
  
  options <- list(Type="readonly", Property = "ID")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$AllParameters$ID[parameter$Index])
  
  options <- list(Type="readonly", Property = "Path")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$AllParameters$Path[parameter$Index])
  
  options <- list(Type="readonly", Property = "Unit")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$AllParameters$Unit[parameter$Index])
  
  options <- list(Type="readonly", Property = "Formula")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$AllParameters$Formula[parameter$Index])
  
  options <- list(Type="readonly", Property = "ParameterType")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$AllParameters$ParameterType[parameter$Index])
  
  options <- list(Type="readonly", Property = "IsFormula")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, as.numeric(dci_info$InputTab$AllParameters$ParameterType[parameter$Index] == "Formula"))
  
  options <- list(Type="readonly", Property = "IsTable")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, as.numeric(dci_info$InputTab$AllParameters$ParameterType[parameter$Index] == "Table"))
  
  options <- list(Type="readonly", Property = "IsValue")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, as.numeric(dci_info$InputTab$AllParameters$ParameterType[parameter$Index] == "Value"))
  
  options <- list(Type="readonly", Property = "Unknown")
  checkException(getParameter(path_id=113, DCI_Info = dci_info, options=options))
}  

test.CheckParameterInSimulationNoneVariables <- function() {
  dci_info <- standard_dci_info
  options <- list(Type="current")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)

  checkEquals(names(parameter), c("Value","Index"))
  checkEquals(dci_info$InputTab$AllParameters$Value[parameter$Index], parameter$Value)
  
  # Check that an exception is thrown when the parameter does not exist in the simulation
  checkException(getParameter(path_id=0, DCI_Info = dci_info, options=options))

  options <- list(Type="variable")
  checkException(getParameter(path_id=113, DCI_Info = dci_info, options=options))
}

# Check if a parameter can be found by its given path name when no parameters were initialized explicitely.
test.CheckParameterInSimulationNoneVariables_byPath <- function() {
  dci_info <- standard_dci_info
  options <- list(Type="current")
  parameter <- getParameter(path_id = "black american girl|Organism|Muscle|Plasma|my Compound|concentration", DCI_Info = dci_info, options=options)
  
  checkEquals(names(parameter), c("Value","Index"))
  checkEquals(dci_info$InputTab$AllParameters$Value[parameter$Index], parameter$Value)
  
  # Check that an exception is thrown when the parameter does not exist in the simulation
  checkException(getParameter(path_id = "black american girl|Organism|nonExistent", DCI_Info = dci_info, options=options))
  
  options <- list(Type="variable")
  checkException(getParameter(path_id = "black american girl|Organism|Muscle|Plasma|my Compound|concentration", DCI_Info = dci_info, options=options))
}

# Check if a parameter can be found by its given path name when no parameters were initialized explicitely
# and the path name contains round brackets.
test.CheckParameterInSimulationNoneVariables_byPathBrackets <- function() {
  dci_info <- standard_dci_info
  options <- list(Type="current")
  parameter <- getParameter(path_id = "black american girl|Organism|pH (plasma)", DCI_Info = dci_info, options=options)
  
  checkEquals(names(parameter), c("Value","Index"))
  checkEquals(dci_info$InputTab$AllParameters$Value[parameter$Index], parameter$Value)
}

test.CheckTableParameterValueTimeProfile <- function() {
  dci_info <- tableParam_dci_info
  
  options <- list(Type="current", TimeProfile=c(0, 60, 180))
  parameter <- getParameter(path_id=5738, options=options, DCI_Info = dci_info)
  checkEquals(names(parameter), c("Value","Index"))
  idx <- which(dci_info$InputTab$VariableTableParameters$ID %in% dci_info$InputTab$VariableParameters$ID[parameter$Index])
  tableValues <- dci_info$InputTab$VariableTableParameters$Value[idx]
  checkEquals(list(tableValues), parameter$Value)

  options <- list(Type="current", TimeProfile=c(0, 60, 180, 240))
  parameter <- getParameter(path_id=5738, options=options, DCI_Info = dci_info)
  checkEquals(names(parameter), c("Value","Index"))
  idx <- which(dci_info$InputTab$VariableTableParameters$ID %in% dci_info$InputTab$VariableParameters$ID[parameter$Index])
  tableValues <- dci_info$InputTab$VariableTableParameters$Value[idx]
  checkEquals(list(c(tableValues,max(tableValues))), parameter$Value)
  
  options <- list(Type="current", TimeProfile=c(-10, 0, 60, 180, 240))
  parameter <- getParameter(path_id=5738, options=options, DCI_Info = dci_info)
  checkEquals(names(parameter), c("Value","Index"))
  idx <- which(dci_info$InputTab$VariableTableParameters$ID %in% dci_info$InputTab$VariableParameters$ID[parameter$Index])
  tableValues <- dci_info$InputTab$VariableTableParameters$Value[idx]
  checkEquals(list(c(min(tableValues), tableValues,max(tableValues))), parameter$Value)
  
  options <- list(Type="current", TimeProfile=c(0, 120, 240, 360))
  parameter <- getParameter(path_id=5822, options=options, DCI_Info = dci_info)
  checkEquals(names(parameter), c("Value","Index"))
  idx <- which(dci_info$InputTab$VariableTableParameters$ID %in% dci_info$InputTab$VariableParameters$ID[parameter$Index])
  tableValues <- dci_info$InputTab$VariableTableParameters$Value[idx]
  checkEquals(list(tableValues), parameter$Value)
  
  options <- list(Type="current", TimeProfile=c(0, 120, 240, 360, 420))
  parameter <- getParameter(path_id=5822, options=options, DCI_Info = dci_info)
  checkEquals(names(parameter), c("Value","Index"))
  idx <- which(dci_info$InputTab$VariableTableParameters$ID %in% dci_info$InputTab$VariableParameters$ID[parameter$Index])
  tableValues <- dci_info$InputTab$VariableTableParameters$Value[idx]
  checkEquals(list(c(tableValues, max(tableValues))), parameter$Value)
  
  options <- list(Type="current", TimeProfile=c(-60, 0, 120, 240, 360, 420))
  parameter <- getParameter(path_id=5822, options=options, DCI_Info = dci_info)
  checkEquals(names(parameter), c("Value","Index"))
  idx <- which(dci_info$InputTab$VariableTableParameters$ID %in% dci_info$InputTab$VariableParameters$ID[parameter$Index])
  tableValues <- dci_info$InputTab$VariableTableParameters$Value[idx]
  checkEquals(list(c(min(tableValues), tableValues, max(tableValues))), parameter$Value)
}

test.CheckParameterValueTablesAndOthers <- function() {
  dci_info <- tableParam_dci_info
  
  timeProfile <- c(0, 60, 180, 240)
  options <- list(Type="current", TimeProfile=timeProfile)
  parameter <- getParameter(path_id=c(61, 5738, 5822), options=options, DCI_Info = dci_info)
  checkEquals(names(parameter), c("Value","Index"))
  checkEquals(max(as.double(lapply(parameter$Value, length))), length(timeProfile))
  checkEquals(min(as.double(lapply(parameter$Value, length))), 1)
  
  #check values for non-table parameters 
  idx <- intersect(parameter$Index, which(dci_info$InputTab$VariableParameters$ParameterType != "Table"))
  for (i in which(parameter$Index %in% idx)) {
    checkEquals(dci_info$InputTab$VariableParameters$Value[idx], parameter$Value[[i]])
  }
  
  timeProfile <- c(0, 30, 60, 90, 120, 150, 180, 210, 240)
  options <- list(Type="current", TimeProfile=timeProfile)
  parameter <- getParameter(path_id=c(61, 70, 220, 5738, 5822), options=options, DCI_Info = dci_info)
  checkEquals(names(parameter), c("Value","Index"))
  checkEquals(max(as.double(lapply(parameter$Value, length))), length(timeProfile))
  checkEquals(min(as.double(lapply(parameter$Value, length))), 1)
  
  #check values for non-table parameters 
  idx <- intersect(parameter$Index, which(dci_info$InputTab$VariableParameters$ParameterType != "Table"))
  for (i in which(parameter$Index %in% idx)) {
    checkEquals(dci_info$InputTab$VariableParameters$Value[idx[i]], parameter$Value[[i]])
  }
  
}