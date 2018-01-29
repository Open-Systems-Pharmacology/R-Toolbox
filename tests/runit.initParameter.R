#require(RUnit, quietly=TRUE)
#require(MoBiToolboxForR, quietly=TRUE)
simModelXML <- "./tests/models/black american girl.xml"

test.EmptyPathID <- function() {
  paramList <- {}
  checkException(initParameter(initStruct = paramList, path_id=""))
}

test.InitializeIfFormula_withWarning <- function() {
  paramList <- {}
  paramList <- initParameter(initStruct = paramList, path_id=113, initializeIfFormula="withWarning")
  checkEquals(length(paramList$Parameters$Path_ID), 1)
  checkEquals(paramList$Parameters$Path_ID[1], 113)
}

test.InitializeIfFormula_Always <- function() {
  paramList <- {}
  paramList <- initParameter(initStruct = paramList, path_id=113, initializeIfFormula="Always")
  checkEquals(length(paramList$Parameters$Path_ID), 1)
  checkEquals(paramList$Parameters$Path_ID[1], 113)
}

test.InitializeIfFormula_Never <- function() {
  paramList <- {}
  paramList <- initParameter(initStruct = paramList, path_id=113, initializeIfFormula="Never")
  checkEquals(length(paramList$Parameters$Path_ID), 1)
  checkEquals(paramList$Parameters$Path_ID[1], 113)
}

test.InitializeIfFormula_Unknown <- function() {
  paramList <- {}
  checkException(initParameter(initStruct = paramList, path_id=113, initializeIfFormula="Unknown")) 
}

test.InitByPath <- function() {
  paramList <- {}
  paramList <- initParameter(initStruct = paramList, path_id="black american girl|Organism|Muscle|Plasma|my Compound|concentration", initializeIfFormula="Always")
  checkEquals(length(paramList$Parameters$Path_ID), 1)
  checkEquals(paramList$Parameters$Path_ID[1], "black american girl|Organism|Muscle|Plasma|my Compound|concentration")
  dci_info <- initSimulation(XML=simModelXML, ParamList=paramList)
}

test.AddParameter_Twice <- function() {
  paramList <- {}
  paramList <- initParameter(initStruct = paramList, path_id=113)
  checkEquals(length(paramList$Parameters$Path_ID), 1)
  checkEquals(paramList$Parameters$Path_ID[1], 113)
  paramList <- initParameter(initStruct = paramList, path_id=113)
  checkEquals(length(paramList$Parameters$Path_ID), 2)
  checkEquals(paramList$Parameters$Path_ID[2], 113)
}

test.UsingParameterInSimulation <- function() {
  paramList <- {}
  paramList <- initParameter(initStruct = paramList, path_id=113)
  dci_info <- initSimulation(XML = simModelXML, ParamList = paramList)
  checkEqualsNumeric(length(dci_info$InputTab$VariableParameters$Path), 1)
  checkEquals(dci_info$InputTab$VariableParameters$ID[1], 113)
}