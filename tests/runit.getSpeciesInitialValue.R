require(RUnit, quietly=TRUE)
require(MoBiToolboxForR, quietly=TRUE)
simModelXML <- "./models/black american girl.xml"
standard_dci_info <- initSimulation(XML=simModelXML, whichInitParam="none")

test.EmptyPathID <- function() {
  dci_info <- standard_dci_info
  checkException(getSpeciesInitialValue(path_id="", DCI_Info = dci_info))
}

test.EmptyDCI_Info <- function() {
  checkException(getSpeciesInitialValue(path_id=134, DCI_Info = {}))
}

test.PropertyUnknown <- function() {
  dci_info <- standard_dci_info
  options <- list(Property="Unknown")
  checkException(getSpeciesInitialValue(path_id=134, options=options , DCI_Info = dci_info))
}

test.CheckParameterInSimulation <- function() {
  paramList <- {}
  paramList <- initSpeciesInitialValue(initStruct = paramList, path_id=134)
  dci_info <- initSimulation(XML = simModelXML, ParamList = paramList)
  
  options <- list(Type="readonly")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(names(parameter), c("Value","Index"))
  checkEquals(dci_info$InputTab$AllSpecies$InitialValue[parameter$Index], parameter$Value)
  
  options <- list(Type="current")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(names(parameter), c("Value","Index"))
  checkEquals(dci_info$InputTab$VariableSpecies$InitialValue[parameter$Index], parameter$Value)

  options <- list(Type="variable")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(names(parameter), c("Value","Index"))
  checkEquals(dci_info$InputTab$VariableSpecies$InitialValue[parameter$Index], parameter$Value)

  options <- list(Type="reference")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(names(parameter), c("Value","Index"))
  checkEquals(dci_info$ReferenceTab$VariableSpecies$InitialValue[parameter$Index], parameter$Value)
  
  options <- list(Type="variable")
  checkException(getSpeciesInitialValue(path_id=141, DCI_Info = dci_info, options=options))
  
  options <- list(Type="current")
  parameter <- getSpeciesInitialValue(path_id=141, DCI_Info = dci_info, options=options)
  checkEquals(names(parameter), c("Value","Index"))
  checkEquals(dci_info$InputTab$AllSpecies$InitialValue[parameter$Index], parameter$Value)
}

test.CheckPropertiesCurrent <- function() {
  paramList <- {}
  paramList <- initSpeciesInitialValue(initStruct = paramList, path_id=134)
  dci_info <- initSimulation(XML = simModelXML, ParamList = paramList)
  
  options <- list(Type="current", Property = "InitialValue")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$VariableSpecies$InitialValue[parameter$Index])
  
  options <- list(Type="current", Property = "ID")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$VariableSpecies$ID[parameter$Index])
  
  options <- list(Type="current", Property = "Path")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$VariableSpecies$Path[parameter$Index])
  
  options <- list(Type="current", Property = "Unit")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$VariableSpecies$Unit[parameter$Index])
  
  options <- list(Type="current", Property = "Formula")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$VariableSpecies$Formula[parameter$Index])
  
  options <- list(Type="current", Property = "IsFormula")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$VariableSpecies$IsFormula[parameter$Index])
  
  options <- list(Type="current", Property = "Unknown")
  checkException(getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options))
}  

test.CheckPropertiesReadonly <- function() {
  dci_info <- standard_dci_info
  
  options <- list(Type="readonly", Property = "InitialValue")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$AllSpecies$InitialValue[parameter$Index])
  
  options <- list(Type="readonly", Property = "ID")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$AllSpecies$ID[parameter$Index])
  
  options <- list(Type="readonly", Property = "Path")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$AllSpecies$Path[parameter$Index])
  
  options <- list(Type="readonly", Property = "Unit")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$AllSpecies$Unit[parameter$Index])
  
  options <- list(Type="readonly", Property = "Formula")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$AllSpecies$Formula[parameter$Index])
  
  options <- list(Type="readonly", Property = "IsFormula")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$AllSpecies$IsFormula[parameter$Index])
  
  options <- list(Type="readonly", Property = "Unknown")
  checkException(getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options))
}  

test.CheckParameterInSimulationNoneVariables <- function() {
  dci_info <- standard_dci_info
  options <- list(Type="current")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  
  checkEquals(names(parameter), c("Value","Index"))
  checkEquals(dci_info$InputTab$AllSpecies$InitialValue[parameter$Index], parameter$Value)

  options <- list(Type="variable")
  checkException(getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options))
}

test.CheckPropertiesReadOnlyWithVariableDefined <- function() {
  paramList <- {}
  paramList <- initSpeciesInitialValue(initStruct = paramList, path_id=134)
  dci_info <- initSimulation(XML = simModelXML, ParamList = paramList)

  options <- list(Type="readonly", Property = "InitialValue")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$AllSpecies$InitialValue[parameter$Index])
  
  options <- list(Type="readonly", Property = "ID")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$AllSpecies$ID[parameter$Index])
  
  options <- list(Type="readonly", Property = "Path")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$AllSpecies$Path[parameter$Index])
  
  options <- list(Type="readonly", Property = "Unit")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$AllSpecies$Unit[parameter$Index])
  
  options <- list(Type="readonly", Property = "Formula")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$AllSpecies$Formula[parameter$Index])
  
  options <- list(Type="readonly", Property = "IsFormula")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$AllSpecies$IsFormula[parameter$Index])
  
  options <- list(Type="readonly", Property = "Unknown")
  checkException(getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options))
}

test.CheckPropertiesCurrentWithVariableDefined <- function() {
  paramList <- {}
  paramList <- initSpeciesInitialValue(initStruct = paramList, path_id=134)
  dci_info <- initSimulation(XML = simModelXML, ParamList = paramList)
  
  options <- list(Type="current", Property = "InitialValue")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$VariableSpecies$InitialValue[parameter$Index])
  
  options <- list(Type="current", Property = "ID")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$VariableSpecies$ID[parameter$Index])
  
  options <- list(Type="current", Property = "Path")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$VariableSpecies$Path[parameter$Index])
  
  options <- list(Type="current", Property = "Unit")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$VariableSpecies$Unit[parameter$Index])
  
  options <- list(Type="current", Property = "Formula")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$VariableSpecies$Formula[parameter$Index])
  
  options <- list(Type="current", Property = "IsFormula")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$VariableSpecies$IsFormula[parameter$Index])
  
  options <- list(Type="current", Property = "Unknown")
  checkException(getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options))
}

test.CheckPropertiesReferenceWithVariableDefined <- function() {
  paramList <- {}
  paramList <- initSpeciesInitialValue(initStruct = paramList, path_id=134)
  dci_info <- initSimulation(XML = simModelXML, ParamList = paramList)
  
  options <- list(Type="reference", Property = "InitialValue")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$ReferenceTab$VariableSpecies$InitialValue[parameter$Index])
  
  options <- list(Type="reference", Property = "ID")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$ReferenceTab$VariableSpecies$ID[parameter$Index])
  
  options <- list(Type="reference", Property = "Path")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$ReferenceTab$VariableSpecies$Path[parameter$Index])
  
  options <- list(Type="reference", Property = "Unit")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$ReferenceTab$VariableSpecies$Unit[parameter$Index])
  
  options <- list(Type="reference", Property = "Formula")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$ReferenceTab$VariableSpecies$Formula[parameter$Index])
  
  options <- list(Type="reference", Property = "IsFormula")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$ReferenceTab$VariableSpecies$IsFormula[parameter$Index])
  
  options <- list(Type="reference", Property = "Unknown")
  checkException(getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options))
}

test.CheckPropertiesCurrentWithNoneVariables <- function() {
  dci_info <- standard_dci_info
  
  options <- list(Type="current", Property = "InitialValue")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$AllSpecies$InitialValue[parameter$Index])
  
  options <- list(Type="current", Property = "ID")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$AllSpecies$ID[parameter$Index])
  
  options <- list(Type="current", Property = "Path")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$AllSpecies$Path[parameter$Index])
  
  options <- list(Type="current", Property = "Unit")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$AllSpecies$Unit[parameter$Index])
  
  options <- list(Type="current", Property = "Formula")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$AllSpecies$Formula[parameter$Index])
  
  options <- list(Type="current", Property = "IsFormula")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$AllSpecies$IsFormula[parameter$Index])
  
  options <- list(Type="current", Property = "Unknown")
  checkException(getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options))
}
