#require(RUnit, quietly=TRUE)
#require(MoBiToolboxForR, quietly=TRUE)
simModelXML <- "./tests/models/black american girl.xml"
simModelTableXML = "./tests/models/TableParameters.xml"
standard_dci_info <- initSimulation(XML=simModelXML, whichInitParam="none")

test.EmptyPathID <- function() {
  dci_info <- standard_dci_info
  checkException(setParameter(value="", path_id="", DCI_Info = dci_info))
}

test.EmptyDCI_Info <- function() {
  checkException(setParameter(value="", path_id=113, DCI_Info = {}))
}

test.PropertyUnknown <- function() {
  dci_info <- standard_dci_info
  options <- list(Property="Unknown")
  checkException(setParameter(value=5, path_id=113, options=options , DCI_Info = dci_info))
}

test.ValueNotNumeric <- function() {
  dci_info <- standard_dci_info
  checkException(setParameter(value="", path_id=113, DCI_Info = dci_info))
}

test.CheckTypes <- function() {
  paramList <- {}
  paramList <- initParameter(initStruct = paramList, path_id=113)
  dci_info <- initSimulation(XML = simModelXML, ParamList = paramList)
  
  options <- list(Type="readonly")
  checkException(setParameter(value=5,path_id=113, DCI_Info = dci_info, options=options))
  
  options <- list(Type="current")
  checkException(setParameter(value=5,path_id=113, DCI_Info = dci_info, options=options))

  options <- list(Type="variable")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  dci_info_check <- setParameter(value=5,path_id=113, DCI_Info = dci_info, options=options)  
  checkEquals(dci_info_check$InputTab$VariableParameters$Value[parameter$Index], 5)

  options <- list(Type="reference")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  dci_info_check <- setParameter(value=5,path_id=113, DCI_Info = dci_info, options=options)  
  checkEquals(dci_info_check$ReferenceTab$VariableParameters$Value[parameter$Index], 5)
}

test.CheckParameterInSimulation <- function() {
  paramList <- {}
  paramList <- initParameter(initStruct = paramList, path_id=113)
  dci_info <- initSimulation(XML = simModelXML, ParamList = paramList)
  
  options <- list(Type="readonly")
  checkException(setParameter(value=5,path_id=113, DCI_Info = dci_info, options=options))
  
  options <- list(Type="variable")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  dci_info_check <- setParameter(value=5,path_id=113, DCI_Info = dci_info, options=options)  
  checkEquals(dci_info_check$InputTab$VariableParameters$Value[parameter$Index], 5)
}

test.CheckProperties <- function() {
  paramList <- {}
  paramList <- initParameter(initStruct = paramList, path_id=113)
  dci_info <- initSimulation(XML = simModelXML, ParamList = paramList)
  
  options <- list(Type="variable", Property = "Value")
  parameter <- getParameter(path_id=113, DCI_Info = dci_info, options=options)
  dci_info_check <- setParameter(value=5, path_id=113, DCI_Info = dci_info, options=options)
  checkEquals(dci_info_check$InputTab$VariableParameters$Value[parameter$Index], 5)
  
  options <- list(Type="variable", Property = "ID")
  checkException(setParameter(value = 555, path_id=113, DCI_Info = dci_info, options=options))
  
  options <- list(Type="variable", Property = "Path")
  checkException(setParameter(value="Dummy", path_id=113, DCI_Info = dci_info, options=options))
  
  options <- list(Type="variable", Property = "Unit")
  checkException(setParameter(value="Dummy", path_id=113, DCI_Info = dci_info, options=options))
  
  options <- list(Type="variable", Property = "Formula")
  checkException(setParameter(value="A*B", path_id=113, DCI_Info = dci_info, options=options))
  
  options <- list(Type="variable", Property = "IsFormula")
  checkException(setParameter(value=1, path_id=113, DCI_Info = dci_info, options=options))
  
  options <- list(Type="variable", Property = "Unknown")
  checkException(setParameter(value=1, path_id=113, DCI_Info = dci_info, options=options))
}

test.CheckParameterInSimulationNoneVariables <- function() {
  dci_info <- standard_dci_info
  options <- list(Type="variable")
  path_id_test = 113;
  #The parameter with the given ID is not variable - an error must be thrown
  options(warn = 2);
  checkException(getParameter(path_id = path_id_test, DCI_Info = dci_info, options=options))
  options(warn = 0);

  checkException(setParameter(value=5, path_id=113, DCI_Info = dci_info, options=options))
}

test.SetParameterEffectOnFormular <- function(){
  
  initStruct <- initParameter(initStruct = list(), path_id = "*|my Compound|Effective molecular weight", initializeIfFormula = "withWarning")
  initStruct <- initParameter(initStruct = initStruct, path_id = "*|my Compound|Br", initializeIfFormula = "withWarning")
  
  dci_info <- initSimulation(XML = simModelXML, ParamList = initStruct)
  v = getParameter("*|Effective molecular weight", DCI_Info = dci_info)$Value
  dci_info = setParameter(value = 10, "*|Br", DCI_Info = dci_info)
  
  #after setting the number of Br to 10, the effective weight should decrease
  checkTrue (getParameter("*|Effective molecular weight", DCI_Info = dci_info)$Value < v)
}

test.CheckRowIndexWithTableParameters <- function() {
  dci_info <- initSimulation(XML = simModelTableXML, whichInitParam="all")
  
  options <- list(Type="variable", Property = "Value")
  parameter <- getParameter(path_id=61, DCI_Info = dci_info, options=options)
  dci_info_check <- setParameter(value=5, path_id=61, DCI_Info = dci_info, options=options)
  checkEquals(dci_info_check$InputTab$VariableParameters$Value[parameter$Index], 5)
  
  options <- list(Type="variable", Property = "Value")
  parameter <- getParameter(path_id=c(61, 244, 308), DCI_Info = dci_info, options=options)
  dci_info_check <- setParameter(value=c(5, 10, 6), path_id=c(61, 244, 308), DCI_Info = dci_info, options=options)
  checkEquals(dci_info_check$InputTab$VariableParameters$Value[parameter$Index], c(5, 10, 6))

  options <- list(Type="variable", Property = "Value", Index=c(17,100,131))
  parameter <- getParameter(path_id="*", DCI_Info = dci_info, options=options)
  dci_info_check <- setParameter(value=c(5, 10, 6), path_id="*", DCI_Info = dci_info, options=options)
  checkEquals(dci_info_check$InputTab$VariableParameters$Value[parameter$Index], c(5, 10, 6))
  
  #1923 is a table parameter and should be ignored
  options <- list(Type="variable", Property = "Value", Index=c(17,100,131,1923))
  parameter <- suppressWarnings(getParameter(path_id="*", DCI_Info = dci_info, options=options))
  dci_info_check <- suppressWarnings(setParameter(value=c(5, 10, 6, 12), path_id="*", DCI_Info = dci_info, options=options))
  checkEquals(dci_info_check$InputTab$VariableParameters$Value[parameter$Index], c(5, 10, 6, NaN))

  #1923 is a table parameter and should be ignored
  options <- list(Type="variable", Property = "Value", Index=c(17,100,131,1923, 488))
  parameter <- suppressWarnings(getParameter(path_id="*", DCI_Info = dci_info, options=options))
  dci_info_check <- suppressWarnings(setParameter(value=c(5, 10, 6, 12, 24), path_id="*", DCI_Info = dci_info, options=options))
  checkEquals(dci_info_check$InputTab$VariableParameters$Value[parameter$Index], c(5, 10, 6, NaN, 24))
  
}