require(RUnit, quietly=TRUE)
require(MoBiToolboxForR, quietly=TRUE)
simModelXML <- "./models/black american girl.xml"
standard_dci_info <- initSimulation(XML=simModelXML, whichInitParam="none")

test.EmptyPathID <- function() {
  dci_info <- standard_dci_info
  checkException(existsSpeciesInitialValue(path_id="", DCI_Info = dci_info))
}

test.EmptyDCI_Info <- function() {
  checkException(existsSpeciesInitialValue(path_id=134, DCI_Info = {}))
}

test.CheckParameterInSimulation <- function() {
  paramList <- {}
  paramList <- initSpeciesInitialValue(initStruct = paramList, path_id=134)
  dci_info <- initSimulation(XML = simModelXML, ParamList = paramList)
  
  options <- list(Type="readonly")
  check <- existsSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkTrue(check$isExisting)
  checkEquals(check$ID, 134)
  checkEquals(check$ID, dci_info$InputTab$AllSpecies$ID[check$Index])
  checkEquals(check$Path, dci_info$InputTab$AllSpecies$Path[check$Index])
  
  options <- list(Type="variable")
  check <- existsSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkTrue(check$isExisting)
  checkEquals(check$ID, 134)
  checkEquals(check$ID, dci_info$InputTab$VariableSpecies$ID[check$Index])
  checkEquals(check$Path, dci_info$InputTab$VariableSpecies$Path[check$Index])
  
  options <- list(Type="readonly")
  check <- existsSpeciesInitialValue(path_id=141, DCI_Info = dci_info, options=options)
  checkTrue(check$isExisting)
  checkEquals(check$ID, 141)
  checkEquals(check$ID, dci_info$InputTab$AllSpecies$ID[check$Index])
  checkEquals(check$Path, dci_info$InputTab$AllSpecies$Path[check$Index])
  
  options <- list(Type="variable")
  check <- existsSpeciesInitialValue(path_id=141, DCI_Info = dci_info, options=options)
  checkTrue(!check$isExisting)
}

test.CheckParameterInSimulationNonevariables <- function() {
  dci_info <- standard_dci_info
  options <- list(Type="readonly")
  check <- existsSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  checkTrue(check$isExisting)
  checkEquals(check$ID, 134)
  checkEquals(check$ID, dci_info$InputTab$AllSpecies$ID[check$Index])
  checkEquals(check$Path, dci_info$InputTab$AllSpecies$Path[check$Index])
}

test.CheckParameterInSimulationNonevariablesIsReference <- function() {
  dci_info <- standard_dci_info
  options <- list(Type="readonly", isReference=TRUE)
  check <- existsSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options = options)
  checkTrue(check$isExisting)
  checkEquals(check$ID, 134)
  checkEquals(check$ID, dci_info$InputTab$AllSpecies$ID[check$Index])
  checkEquals(check$Path, dci_info$InputTab$AllSpecies$Path[check$Index])
}

test.CheckParameterInSimulationNonevariablesInvariables <- function() {
  dci_info <- standard_dci_info
  options <- list(Type="variable")
  check <- existsSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options = options)
  checkTrue(!check$isExisting)
}

test.CheckParameterInSimulationNonevariablesInvariablesIsReference <- function() {
  dci_info <- standard_dci_info
  options <- list(Type="variable", isReference=TRUE)
  check <- existsSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options = options)
  checkTrue(!check$isExisting)
}
