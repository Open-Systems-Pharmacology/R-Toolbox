require(RUnit, quietly=TRUE)
require(MoBiToolboxForR, quietly=TRUE)
simModelXML <- "./models/black american girl.xml"

test.EmptyPathID <- function() {
  paramList <- {}
  checkException(initSpeciesInitialValue(initStruct = paramList, path_id=""))
}

test.InitializeIfFormula_withWarning <- function() {
  paramList <- {}
  paramList <- initSpeciesInitialValue(initStruct = paramList, path_id=134, initializeIfFormula="withWarning")
  checkEquals(length(paramList$InitialValues$Path_ID), 1)
  checkEquals(paramList$InitialValues$Path_ID[1], 134)
}

test.InitializeIfFormula_Always <- function() {
  paramList <- {}
  paramList <- initSpeciesInitialValue(initStruct = paramList, path_id=134, initializeIfFormula="Always")
  checkEquals(length(paramList$InitialValues$Path_ID), 1)
  checkEquals(paramList$InitialValues$Path_ID[1], 134)
}

test.InitializeIfFormula_Never <- function() {
  paramList <- {}
  paramList <- initSpeciesInitialValue(initStruct = paramList, path_id=134, initializeIfFormula="Never")
  checkEquals(length(paramList$InitialValues$Path_ID), 1)
  checkEquals(paramList$InitialValues$Path_ID[1], 134)
}

test.InitializeIfFormula_Unknown <- function() {
  paramList <- {}
  checkException(initSpeciesInitialValue(initStruct = paramList, path_id=134, initializeIfFormula="Unknown")) 
}

test.AddParameter_Twice <- function() {
  paramList <- {}
  paramList <- initSpeciesInitialValue(initStruct = paramList, path_id=134)
  checkEquals(length(paramList$InitialValues$Path_ID), 1)
  checkEquals(paramList$InitialValues$Path_ID[1], 134)
  paramList <- initSpeciesInitialValue(initStruct = paramList, path_id=134)
  checkEquals(length(paramList$InitialValues$Path_ID), 2)
  checkEquals(paramList$InitialValues$Path_ID[2], 134)
}

test.UsingParameterInSimulation <- function() {
  paramList <- {}
  paramList <- initSpeciesInitialValue(initStruct = paramList, path_id=134)
  dci_info <- initSimulation(XML = simModelXML, ParamList = paramList)
  checkEqualsNumeric(length(dci_info$InputTab$VariableSpecies$Path), 1)
  checkEquals(dci_info$InputTab$VariableSpecies$ID[1], 134)
}