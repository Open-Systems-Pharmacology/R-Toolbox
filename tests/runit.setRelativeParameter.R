#require(RUnit, quietly=TRUE)
#require(MoBiToolboxForR, quietly=TRUE)
simModelXML <- "./tests/models/black american girl.xml"
standard_dci_info <- initSimulation(XML=simModelXML, whichInitParam="none")

test.EmptyPathID <- function() {
  dci_info <- standard_dci_info
  checkException(setRelativeParameter(value="", path_id="", DCI_Info = dci_info))
}

test.EmptyDCI_Info <- function() {
  checkException(setRelativeParameter(value="", path_id=113, DCI_Info = {}))
}

test.PropertyUnknown <- function() {
  dci_info <- standard_dci_info
  options <- list(Property="Unknown")
  checkException(setRelativeParameter(value=4, path_id=113, options=options , DCI_Info = dci_info))
}

test.ValueNotNumeric <- function() {
  dci_info <- standard_dci_info
  checkException(setRelativeParameter(value="", path_id=113, DCI_Info = dci_info))
}

test.CheckTypes <- function() {
  paramList <- {}
  paramList <- initParameter(initStruct = paramList, path_id=113)
  dci_info <- initSimulation(XML = simModelXML, ParamList = paramList)
  
  options <- list(Type="readonly")
  checkException(setRelativeParameter(value=5, path_id=113, DCI_Info = dci_info, options=options))
  
  options <- list(Type="current")
  checkException(setRelativeParameter(value=5, path_id=113, DCI_Info = dci_info, options=options))

  options <- list(Type="variable")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  dci_info_check <- setRelativeParameter(value=5, path_id=113, DCI_Info = dci_info, options=options)  
  checkEquals(dci_info_check$InputTab$VariableParameters$Value[parameter$Index], 5*parameter$Value)

  options <- list(Type="reference")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  dci_info_check <- setRelativeParameter(value=5, path_id=113, DCI_Info = dci_info, options=options)  
  checkEquals(dci_info_check$ReferenceTab$VariableParameters$Value[parameter$Index], 5*parameter$Value)
}

test.CheckParameterInSimulation <- function() {
  paramList <- {}
  paramList <- initParameter(initStruct = paramList, path_id=113)
  dci_info <- initSimulation(XML = simModelXML, ParamList = paramList)
  
  options <- list(Type="readonly")
  checkException(setRelativeParameter(value=5, path_id=113, DCI_Info = dci_info, options=options))
  
  options <- list(Type="variable")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  dci_info_check <- setRelativeParameter(value=5, path_id=113, DCI_Info = dci_info, options=options)  
  checkEquals(dci_info_check$InputTab$VariableParameters$Value[parameter$Index], 5*parameter$Value)
}

test.CheckProperties <- function() {
  paramList <- {}
  paramList <- initParameter(initStruct = paramList, path_id=113)
  dci_info <- initSimulation(XML = simModelXML, ParamList = paramList)
  
  options <- list(Type="variable", Property = "Value")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  dci_info_check <- setRelativeParameter(value=5, path_id=113, DCI_Info = dci_info, options=options)
  checkEquals(dci_info_check$InputTab$VariableParameters$Value[parameter$Index], 5*parameter$Value)
  
  options <- list(Type="variable", Property = "ID")
  checkException(setRelativeParameter(value = 555, path_id=113, DCI_Info = dci_info, options=options))
  
  options <- list(Type="variable", Property = "Path")
  checkException(setRelativeParameter(value="Dummy", path_id=113, DCI_Info = dci_info, options=options))
  
  options <- list(Type="variable", Property = "Unit")
  checkException(setRelativeParameter(value="Dummy", path_id=113, DCI_Info = dci_info, options=options))
  
  options <- list(Type="variable", Property = "Formula")
  checkException(setRelativeParameter(value="A*B", path_id=113, DCI_Info = dci_info, options=options))
  
  options <- list(Type="variable", Property = "IsFormula")
  checkException(setRelativeParameter(value=1, path_id=113, DCI_Info = dci_info, options=options))
  
  options <- list(Type="variable", Property = "Unknown")
  checkException(setRelativeParameter(value=1, path_id=113, DCI_Info = dci_info, options=options))
}

test.CheckParameterInSimulationNoneVariables <- function() {
  dci_info <- standard_dci_info
  options <- list(Type="variable")
  path_id_test = 113;
  parameter = getParameter(path_id = path_id_test, DCI_Info = dci_info, options=options);
  checkEquals(names(parameter), c("Value","Index"))
  checkEquals(length(parameter$Index), 0);
  
  options(warn = 2);
  checkException(getParameter(path_id = path_id_test, DCI_Info = dci_info, options=options))
  options(warn = 0);

  checkException(setRelativeParameter(value=5, path_id=113, DCI_Info = dci_info, options=options))
}