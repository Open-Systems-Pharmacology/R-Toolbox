#require(RUnit, quietly=TRUE)
#require(MoBiToolboxForR, quietly=TRUE)
simModelXML <- "./tests/models/black american girl.xml"

test.EmptyDCI_Info <- function() {
  checkException(setParameterStatus(DCI_Info = {}))
}

test.InvalidDCIHandle <- function() {
  standard_dci_info <- initSimulation(XML=simModelXML, whichInitParam="none")
  all_dci_info <- initSimulation(XML=simModelXML, whichInitParam="all", SimulationNumber=2)
  standard_dci_info <- initSimulation(XML=simModelXML, whichInitParam="none")
  dci_info <- all_dci_info
  checkException(processSimulation(DCI_Info=dci_info))
}

test.NoneVariables <- function() {
  dci_info <- initSimulation(XML=simModelXML, whichInitParam="none")
  dci_info_check <- processSimulation(DCI_Info=dci_info)
  
  checkEquals(names(dci_info_check), c("MOBI_SETTINGS", "Handle", "InputTab", "ReferenceTab", "OutputTabTime", "OutputTabValue"))
  checkEquals(names(dci_info_check$OutputTabTime), "Time")
  time <- getSimulationTime(DCI_Info=dci_info_check)
  checkEqualsNumeric(time$Time, dci_info_check$OutputTabTime$Time)
  
  speciesIntialValues <- getSpeciesInitialValue(path_id="*", options=list(Type="readonly"), DCI_Info=dci_info_check)
  observers <- getObserverFormula(path_id="*", options=list(Type="readonly"), DCI_Info=dci_info_check)
  #checkEquals(length(dci_info_check$OutputTabValue), length(speciesIntialValues$Index) + length(observers$Index))
}

test.NoneVariablesNewTimepoints <- function() {
  dci_info <- initSimulation(XML=simModelXML, whichInitParam="none")
  timepoints <- c(0,15,30,45,60,120,240)
  dci_info <- setSimulationTime(timepoints= timepoints , DCI_Info=dci_info)
  dci_info_check <- processSimulation(DCI_Info=dci_info)
  
  checkEquals(names(dci_info_check), c("MOBI_SETTINGS", "Handle", "InputTab", "ReferenceTab", "OutputTabTime", "OutputTabValue"))
  checkEquals(names(dci_info_check$OutputTabTime), "Time")
  time <- getSimulationTime(DCI_Info=dci_info_check)
  checkEqualsNumeric(dci_info_check$OutputTabTime$Time, time$Time)
  checkEqualsNumeric(dci_info_check$OutputTabTime$Time, timepoints)
  
  speciesIntialValues <- getSpeciesInitialValue(path_id="*", options=list(Type="readonly"), DCI_Info=dci_info_check)
  observers <- getObserverFormula(path_id="*", options=list(Type="readonly"), DCI_Info=dci_info_check)
  #checkEquals(length(dci_info_check$OutputTabValue), length(speciesIntialValues$Index) + length(observers$Index))
}

test.allNonVariablesNewTimepoints <- function() {
  dci_info <- initSimulation(XML=simModelXML, whichInitParam="allNonFormula")
  timepoints <- c(0,15,30,45,60,120,240)
  dci_info <- setSimulationTime(timepoints= timepoints , DCI_Info=dci_info)
  dci_info_check <- processSimulation(DCI_Info=dci_info)
  
  checkEquals(names(dci_info_check), c("MOBI_SETTINGS", "Handle", "InputTab", "ReferenceTab", "OutputTabTime", "OutputTabValue"))
  checkEquals(names(dci_info_check$OutputTabTime), "Time")
  time <- getSimulationTime(DCI_Info=dci_info_check)
  checkEqualsNumeric(dci_info_check$OutputTabTime$Time, time$Time)
  checkEqualsNumeric(dci_info_check$OutputTabTime$Time, timepoints)
  
  speciesIntialValues <- getSpeciesInitialValue(path_id="*", options=list(Type="readonly"), DCI_Info=dci_info_check)
  observers <- getObserverFormula(path_id="*", options=list(Type="readonly"), DCI_Info=dci_info_check)
  #checkEquals(length(dci_info_check$OutputTabValue), length(speciesIntialValues$Index) + length(observers$Index))
}