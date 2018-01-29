#require(RUnit, quietly=TRUE)
#require(MoBiToolboxForR, quietly=TRUE)
simModelXML <- "./tests/models/black american girl.xml"
standard_dci_info <- initSimulation(XML=simModelXML, whichInitParam="none")

test.EmptyPathID <- function() {
  dci_info <- standard_dci_info
  checkException(setSpeciesInitialValue(value="", path_id="", DCI_Info = dci_info))
}

test.EmptyDCI_Info <- function() {
  checkException(setSpeciesInitialValue(value="", path_id=134, DCI_Info = {}))
}

test.PropertyUnknown <- function() {
  dci_info <- standard_dci_info
  options <- list(Property="Unknown")
  checkException(setSpeciesInitialValue(value=5, path_id=134, options=options , DCI_Info = dci_info))
}

test.ValueNotNumeric <- function() {
  dci_info <- standard_dci_info
  checkException(setSpeciesInitialValue(value="", path_id=134, DCI_Info = dci_info))
}

test.CheckTypes <- function() {
  paramList <- {}
  paramList <- initSpeciesInitialValue(initStruct = paramList, path_id=134)
  dci_info <- initSimulation(XML = simModelXML, ParamList = paramList)
  
  options <- list(Type="readonly")
  checkException(setSpeciesInitialValue(value=5,path_id=134, DCI_Info = dci_info, options=options))

  options <- list(Type="current")
  checkException(setSpeciesInitialValue(value=5,path_id=134, DCI_Info = dci_info, options=options))
  
  options <- list(Type="variable")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  dci_info_check <- setSpeciesInitialValue(value=5,path_id=134, DCI_Info = dci_info, options=options)  
  checkEquals(dci_info_check$InputTab$VariableSpecies$InitialValue[parameter$Index], 5)

  options <- list(Type="reference")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  dci_info_check <- setSpeciesInitialValue(value=5,path_id=134, DCI_Info = dci_info, options=options)  
  checkEquals(dci_info_check$ReferenceTab$VariableSpecies$InitialValue[parameter$Index], 5)
}

test.CheckParameterInSimulation <- function() {
  paramList <- {}
  paramList <- initSpeciesInitialValue(initStruct = paramList, path_id=134)
  dci_info <- initSimulation(XML = simModelXML, ParamList = paramList)
  
  options <- list(Type="readonly")
  checkException(setSpeciesInitialValue(value=5,path_id=134, DCI_Info = dci_info, options=options))
  
  options <- list(Type="variable")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  dci_info_check <- setSpeciesInitialValue(value=5,path_id=134, DCI_Info = dci_info, options=options)  
  checkEquals(dci_info_check$InputTab$VariableSpecies$InitialValue[parameter$Index], 5)
}

test.CheckProperties <- function() {
  paramList <- {}
  paramList <- initSpeciesInitialValue(initStruct = paramList, path_id=134)
  dci_info <- initSimulation(XML = simModelXML, ParamList = paramList)
  
  options <- list(Type="variable", Property = "InitialValue")
  parameter <- getSpeciesInitialValue(path_id=134, DCI_Info = dci_info, options=options)
  dci_info_check <- setSpeciesInitialValue(value=5, path_id=134, DCI_Info = dci_info, options=options)
  checkEquals(dci_info_check$InputTab$VariableSpecies$InitialValue[parameter$Index], 5)
  
  options <- list(Type="variable", Property = "ID")
  checkException(setSpeciesInitialValue(value = 555, path_id=134, DCI_Info = dci_info, options=options))
  
  options <- list(Type="variable", Property = "Path")
  checkException(setSpeciesInitialValue(value="Dummy", path_id=134, DCI_Info = dci_info, options=options))
  
  options <- list(Type="variable", Property = "Unit")
  checkException(setSpeciesInitialValue(value="Dummy", path_id=134, DCI_Info = dci_info, options=options))
  
  options <- list(Type="variable", Property = "Formula")
  checkException(setSpeciesInitialValue(value="A*B", path_id=134, DCI_Info = dci_info, options=options))
  
  options <- list(Type="variable", Property = "IsFormula")
  checkException(setSpeciesInitialValue(value=1, path_id=134, DCI_Info = dci_info, options=options))
  
  options <- list(Type="variable", Property = "Unknown")
  checkException(setSpeciesInitialValue(value=1, path_id=134, DCI_Info = dci_info, options=options))
}

test.CheckParameterInSimulationNoneVariables <- function() {
  dci_info <- standard_dci_info
  options <- list(Type="variable")
  path_id_test = 134;
  parameter = getSpeciesInitialValue(path_id = path_id_test, DCI_Info = dci_info, options=options);
  checkEquals(names(parameter), c("Value","Index"))
  checkEquals(length(parameter$Index), 0);
  
  options(warn = 2);
  checkException(getSpeciesInitialValue(path_id = path_id_test, DCI_Info = dci_info, options=options))
  options(warn = 0);

  checkException(setSpeciesInitialValue(value=5, path_id=134, DCI_Info = dci_info, options=options))
}