#require(RUnit, quietly=TRUE)
#require(MoBiToolboxForR, quietly=TRUE)
simModelXML <- "./tests/models/black american girl.xml"
standard_dci_info <- initSimulation(XML=simModelXML, whichInitParam="none")

test.EmptyPathID <- function() {
  dci_info <- standard_dci_info
  checkException(getObserverFormula(path_id="", DCI_Info = dci_info))
}

test.EmptyDCI_Info <- function() {
  checkException(getObserverFormula(path_id=97, DCI_Info = {}))
}

test.PropertyUnknown <- function() {
  dci_info <- standard_dci_info
  options <- list(Property="Unknown")
  checkException(getObserverFormula(path_id=97, options=options , DCI_Info = dci_info))
}

test.CheckObserverInSimulation <- function() {
  dci_info <- standard_dci_info
  
  parameter <- getObserverFormula(path_id=97, DCI_Info = dci_info)
  checkEquals(names(parameter), c("Value","Index"))
  checkEquals(dci_info$InputTab$AllObservers$Formula[parameter$Index], parameter$Value)


  options <- list(isReference = TRUE)
  parameter <- getObserverFormula(path_id=97, DCI_Info = dci_info, options=options)
  checkEquals(names(parameter), c("Value","Index"))
  checkEquals(dci_info$ReferenceTab$AllObservers$Formula[parameter$Index], parameter$Value)
}

test.CheckPropertiesDefault <- function() {
  dci_info <- standard_dci_info
  
  options <- list(Property = "ID")
  parameter <- getObserverFormula(path_id=97, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$AllObservers$ID[parameter$Index])
  
  options <- list(Property = "Path")
  parameter <- getObserverFormula(path_id=97, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$AllObservers$Path[parameter$Index])
  
  options <- list(Property = "Unit")
  parameter <- getObserverFormula(path_id=97, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$AllObservers$Unit[parameter$Index])
  
  options <- list(Property = "Formula")
  parameter <- getObserverFormula(path_id=97, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$InputTab$AllObservers$Formula[parameter$Index])
  
  options <- list(Property = "Unknown")
  checkException(getObserverFormula(path_id=97, DCI_Info = dci_info, options=options))
}  

test.CheckPropertiesReference <- function() {
  dci_info <- standard_dci_info
  
  options <- list(isReference = TRUE, Property = "ID")
  parameter <- getObserverFormula(path_id=97, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$ReferenceTab$AllObservers$ID[parameter$Index])
  
  options <- list(isReference = TRUE, Property = "Path")
  parameter <- getObserverFormula(path_id=97, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$ReferenceTab$AllObservers$Path[parameter$Index])
  
  options <- list(isReference = TRUE, Property = "Unit")
  parameter <- getObserverFormula(path_id=97, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$ReferenceTab$AllObservers$Unit[parameter$Index])
  
  options <- list(isReference = TRUE, Property = "Formula")
  parameter <- getObserverFormula(path_id=97, DCI_Info = dci_info, options=options)
  checkEquals(parameter$Value, dci_info$ReferenceTab$AllObservers$Formula[parameter$Index])
  
  options <- list(isReference = TRUE, Property = "Unknown")
  checkException(getObserverFormula(path_id=97, DCI_Info = dci_info, options=options))
}  


test.CheckEmptyOptions <- function() {
  dci_info <- standard_dci_info
  options <- list()
  parameter <- getObserverFormula(path_id=97, DCI_Info = dci_info, options=options)
  
  checkEquals(names(parameter), c("Value","Index"))
  checkEquals(dci_info$InputTab$AllObservers$Formula[parameter$Index], parameter$Value)
}
