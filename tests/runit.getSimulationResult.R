require(RUnit, quietly=TRUE)
require(MoBiToolboxForR, quietly=TRUE)
simModelXML <- "./models/black american girl.xml"

test.EmptyDCI_Info <- function() {
  checkException(getSimulationResult(DCI_Info = {}))
}

test.NotSimulated <- function() {
  standard_dci_info <- initSimulation(XML=simModelXML, whichInitParam="none")
  dci_info <- standard_dci_info
  checkException(getSimulationResult(DCI_Info=dci_info))
}

test.NoneVariables <- function() {
  dci_info <- initSimulation(XML=simModelXML, whichInitParam="none")
  dci_info_check <- processSimulation(DCI_Info=dci_info)

  checkEquals(names(dci_info_check), c("MOBI_SETTINGS", "Handle", "InputTab", "ReferenceTab", "OutputTabTime", "OutputTabValue"))
  checkEquals(names(dci_info_check$OutputTabTime), "Time")

  results <- getSimulationResult(DCI_Info=dci_info_check)
  time <- getSimulationTime(DCI_Info=dci_info_check)
  checkEqualsNumeric(time$Time, results[,"Time"])
  
  speciesIntialValues <- getSpeciesInitialValue(path_id="*", options=list(Type="readonly"), DCI_Info=dci_info_check)
  observers <- getObserverFormula(path_id="*", options=list(Type="readonly"), DCI_Info=dci_info_check)
  checkEquals(length(results[1,])-1, length(speciesIntialValues$Index) + length(observers$Index))
  
}

test.NoneVariablesNewTimepoints <- function() {
  dci_info <- initSimulation(XML=simModelXML, whichInitParam="none")
  timepoints <- c(0,15,30,45,60,120,240)
  dci_info <- setSimulationTime(timepoints= timepoints , DCI_Info=dci_info)
  dci_info_check <- processSimulation(DCI_Info=dci_info)
  
  checkEquals(names(dci_info_check), c("MOBI_SETTINGS", "Handle", "InputTab", "ReferenceTab", "OutputTabTime", "OutputTabValue"))
  checkEquals(names(dci_info_check$OutputTabTime), "Time")
  
  results <- getSimulationResult(DCI_Info=dci_info_check)
  time <- getSimulationTime(DCI_Info=dci_info_check)
  
  checkEqualsNumeric(time$Time, results[,"Time"])
  checkEqualsNumeric(dci_info_check$OutputTabTime$Time, time$Time)
  checkEqualsNumeric(dci_info_check$OutputTabTime$Time, timepoints)
  
  speciesIntialValues <- getSpeciesInitialValue(path_id="*", options=list(Type="readonly"), DCI_Info=dci_info_check)
  observers <- getObserverFormula(path_id="*", options=list(Type="readonly"), DCI_Info=dci_info_check)
  checkEquals(length(results[1,])-1, length(speciesIntialValues$Index) + length(observers$Index))
}

test.allNonVariablesNewTimepoints <- function() {
  dci_info <- initSimulation(XML=simModelXML, whichInitParam="allNonFormula")
  timepoints <- c(0,15,30,45,60,120,240)
  dci_info <- setSimulationTime(timepoints= timepoints , DCI_Info=dci_info)
  dci_info_check <- processSimulation(DCI_Info=dci_info)
  
  checkEquals(names(dci_info_check), c("MOBI_SETTINGS", "Handle", "InputTab", "ReferenceTab", "OutputTabTime", "OutputTabValue"))
  checkEquals(names(dci_info_check$OutputTabTime), "Time")
  
  results <- getSimulationResult(DCI_Info=dci_info_check)
  time <- getSimulationTime(DCI_Info=dci_info_check)
  
  checkEqualsNumeric(results[,"Time"], time$Time)
  checkEqualsNumeric(dci_info_check$OutputTabTime$Time, time$Time)
  checkEqualsNumeric(dci_info_check$OutputTabTime$Time, timepoints)
  
  speciesIntialValues <- getSpeciesInitialValue(path_id="*", options=list(Type="readonly"), DCI_Info=dci_info_check)
  observers <- getObserverFormula(path_id="*", options=list(Type="readonly"), DCI_Info=dci_info_check)
  checkEquals(length(results[1,])-1, length(speciesIntialValues$Index) + length(observers$Index))
}