#require(RUnit, quietly=TRUE)
#require(MoBiToolboxForR, quietly=TRUE)
simModelXML <- "./tests/models/black american girl.xml"
standard_dci_info <- initSimulation(XML=simModelXML, whichInitParam="none")

test.EmptyPathID <- function() {
  dci_info <- standard_dci_info
  checkException(existsParameter(path_id="", DCI_Info = dci_info))
}

test.EmptyDCI_Info <- function() {
  checkException(existsParameter(path_id=113, DCI_Info = {}))
}

test.CheckParameterInSimulation <- function() {
  paramList <- {}
  paramList <- initParameter(initStruct = paramList, path_id=113)
  dci_info <- initSimulation(XML = simModelXML, ParamList = paramList)
  
  options <- list(Type="readonly")
  check <- existsParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkTrue(check$isExisting)
  checkEquals(check$ID, 113)
  checkEquals(check$ID, dci_info$InputTab$AllParameters$ID[check$Index])
  checkEquals(check$Path, dci_info$InputTab$AllParameters$Path[check$Index])
  
  options <- list(Type="variable")
  check <- existsParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkTrue(check$isExisting)
  checkEquals(check$ID, 113)
  checkEquals(check$ID, dci_info$InputTab$VariableParameters$ID[check$Index])
  checkEquals(check$Path, dci_info$InputTab$VariableParameters$Path[check$Index])
  
  options <- list(Type="readonly")
  check <- existsParameter(path_id=114, DCI_Info = dci_info, options=options)
  checkTrue(check$isExisting)
  checkEquals(check$ID, 114)
  checkEquals(check$ID, dci_info$InputTab$AllParameters$ID[check$Index])
  checkEquals(check$Path, dci_info$InputTab$AllParameters$Path[check$Index])
  
  options <- list(Type="variable")
  check <- existsParameter(path_id=114, DCI_Info = dci_info, options=options)
  checkTrue(!check$isExisting)
}

test.CheckParameterInSimulationNonevariables <- function() {
  dci_info <- standard_dci_info
  options <- list(Type="readonly")
  check <- existsParameter(path_id=113, DCI_Info = dci_info, options=options)
  checkTrue(check$isExisting)
  checkEquals(check$ID, 113)
  checkEquals(check$ID, dci_info$InputTab$AllParameters$ID[check$Index])
  checkEquals(check$Path, dci_info$InputTab$AllParameters$Path[check$Index])
}

test.CheckParameterInSimulationNonevariablesIsReference <- function() {
  dci_info <- standard_dci_info
  options <- list(Type="readonly", isReference=TRUE)
  check <- existsParameter(path_id=113, DCI_Info = dci_info, options = options)
  checkTrue(check$isExisting)
  checkEquals(check$ID, 113)
  checkEquals(check$ID, dci_info$InputTab$AllParameters$ID[check$Index])
  checkEquals(check$Path, dci_info$InputTab$AllParameters$Path[check$Index])
}

test.CheckParameterInSimulationNonevariablesInvariables <- function() {
  dci_info <- standard_dci_info
  options <- list(Type="variable")
  check <- existsParameter(path_id=113, DCI_Info = dci_info, options = options)
  checkTrue(!check$isExisting)
}

test.CheckParameterInSimulationNonevariablesInvariablesIsReference <- function() {
  dci_info <- standard_dci_info
  options <- list(Type="variable", isReference=TRUE)
  check <- existsParameter(path_id=113, DCI_Info = dci_info, options = options)
  checkTrue(!check$isExisting)
}
