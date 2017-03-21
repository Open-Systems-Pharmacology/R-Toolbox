require(RUnit, quietly=TRUE)
require(MoBiToolboxForR, quietly=TRUE)
simModelXML <- "./models/black american girl.xml"
standard_dci_info <- initSimulation(XML=simModelXML, whichInitParam="none")

test.EmptyPathID <- function() {
  dci_info <- standard_dci_info
  checkException(setRelativeSpeciesInitialValue(value="", path_id="", DCI_Info = dci_info))
}

test.EmptyDCI_Info <- function() {
  checkException(setRelativeSpeciesInitialValue(value="", path_id=134, DCI_Info = {}))
}

test.PropertyUnknown <- function() {
  dci_info <- standard_dci_info
  options <- list(Property="Unknown")
  checkException(setRelativeSpeciesInitialValue(value=4, path_id=134, options=options , DCI_Info = dci_info))
}

test.ValueNotNumeric <- function() {
  dci_info <- standard_dci_info
  checkException(setRelativeSpeciesInitialValue(value="", path_id=134, DCI_Info = dci_info))
}

test.CheckTypes <- function() {
  paramList <- {}
  paramList <- initSpeciesInitialValue(initStruct = paramList, path_id=134)
  dci_info <- initSimulation(XML = simModelXML, ParamList = paramList)
  
  options <- list(Type="readonly")
  checkException(setRelativeSpeciesInitialValue(value=5, path_id=134, DCI_Info = dci_info, options=options))
  
  options <- list(Type="current")
  checkException(setRelativeSpeciesInitialValue(value=5, path_id=134, DCI_Info = dci_info, options=options))
  
  options <- list(Type="variable")  
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  dci_info_check <- setRelativeSpeciesInitialValue(value=5, path_id=134, DCI_Info = dci_info, options=options)  
  checkEquals(dci_info_check$InputTab$VariableSpecies$InitialValue[parameter$Index], 5*parameter$Value)
  
  options <- list(Type="reference")  
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  dci_info_check <- setRelativeSpeciesInitialValue(value=5, path_id=134, DCI_Info = dci_info, options=options)  
  checkEquals(dci_info_check$ReferenceTab$VariableSpecies$InitialValue[parameter$Index], 5*parameter$Value)  
}


test.CheckParameterInSimulation <- function() {
  paramList <- {}
  paramList <- initSpeciesInitialValue(initStruct = paramList, path_id=134)
  dci_info <- initSimulation(XML = simModelXML, ParamList = paramList)
  
  options <- list(Type="readonly")
  checkException(setRelativeSpeciesInitialValue(value=5, path_id=134, DCI_Info = dci_info, options=options))
  
  options <- list(Type="variable")  
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  dci_info_check <- setRelativeSpeciesInitialValue(value=5, path_id=134, DCI_Info = dci_info, options=options)  
  checkEquals(dci_info_check$InputTab$VariableSpecies$InitialValue[parameter$Index], 5*parameter$Value)

  options <- list(Type="reference")  
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  dci_info_check <- setRelativeSpeciesInitialValue(value=5, path_id=134, DCI_Info = dci_info, options=options)  
  checkEquals(dci_info_check$ReferenceTab$VariableSpecies$InitialValue[parameter$Index], 5*parameter$Value)

  options <- list(Type="variable", Property="ScaleFactor")  
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  dci_info_check <- setRelativeSpeciesInitialValue(value=5, path_id=134, DCI_Info = dci_info, options=options)  
  checkEquals(dci_info_check$InputTab$VariableSpecies$ScaleFactor[parameter$Index], 5*parameter$Value)
  
  options <- list(Type="reference", Property="ScaleFactor")  
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  dci_info_check <- setRelativeSpeciesInitialValue(value=5, path_id=134, DCI_Info = dci_info, options=options)  
  checkEquals(dci_info_check$ReferenceTab$VariableSpecies$ScaleFactor[parameter$Index], 5*parameter$Value)
  
}

test.CheckProperties <- function() {
  paramList <- {}
  paramList <- initSpeciesInitialValue(initStruct = paramList, path_id=134)
  dci_info <- initSimulation(XML = simModelXML, ParamList = paramList)
  
  options <- list(Type="variable", Property = "InitialValue")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  dci_info_check <- setRelativeSpeciesInitialValue(value=5, path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(dci_info_check$InputTab$VariableSpecies$InitialValue[parameter$Index], 5*parameter$Value)

  options <- list(Type="variable", Property = "ScaleFactor")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  dci_info_check <- setRelativeSpeciesInitialValue(value=5, path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(dci_info_check$InputTab$VariableSpecies$ScaleFactor[parameter$Index], 5*parameter$Value)
  
  options <- list(Type="variable", Property = "ID")
  checkException(setRelativeSpeciesInitialValue(value = 555, path_id=134, DCI_Info = dci_info, options=options))
  
  options <- list(Type="variable", Property = "Path")
  checkException(setRelativeSpeciesInitialValue(value="Dummy", path_id=134, DCI_Info = dci_info, options=options))
  
  options <- list(Type="variable", Property = "Unit")
  checkException(setRelativeSpeciesInitialValue(value="Dummy", path_id=134, DCI_Info = dci_info, options=options))
  
  options <- list(Type="variable", Property = "Formula")
  checkException(setRelativeSpeciesInitialValue(value="A*B", path_id=134, DCI_Info = dci_info, options=options))
  
  options <- list(Type="variable", Property = "IsFormula")
  checkException(setRelativeSpeciesInitialValue(value=1, path_id=134, DCI_Info = dci_info, options=options))
  
  options <- list(Type="variable", Property = "Unknown")
  checkException(setRelativeSpeciesInitialValue(value=1, path_id=134, DCI_Info = dci_info, options=options))
}

test.CheckParameterInSimulationNoneVariables <- function() {
  dci_info <- standard_dci_info
  options <- list(Type="variable")
  
  checkException(getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options))
  
  checkException(setRelativeSpeciesInitialValue(value=5, path_id=134, DCI_Info = dci_info, options=options))

  options <- list(Type="variable", Property="ScaleFactor")
  checkException(setRelativeSpeciesInitialValue(value=5, path_id=134, DCI_Info = dci_info, options=options))
}