#require(RUnit, quietly=TRUE)
#require(MoBiToolboxForR, quietly=TRUE)
simModelXML <- "./tests/models/TableParameters.xml"
standard_dci_info <- initSimulation(XML=simModelXML, whichInitParam="all")

test.EmptyPathID <- function() {
  dci_info <- standard_dci_info
  checkException(getTableParameter(path_id="", DCI_Info = dci_info))
}

test.EmptyDCI_Info <- function() {
  checkException(getTableParameter(path_id=5738, DCI_Info = {}))
}

test.CheckParameterInSimulation <- function() {
  paramList <- {}
  paramList <- initParameter(initStruct = paramList, path_id=5738)
  dci_info <- initSimulation(XML = simModelXML, ParamList = paramList)
  
  options <- list(Type="readonly")
  parameter <- getTableParameter(path_id=5738, DCI_Info = dci_info, options=options)
  checkEquals(names(parameter), c("ID", "Time", "Value", "RestartSolver"))
  idx <- which(dci_info$InputTab$AllTableParameters$ID == 5738)
  idx2 <- which(parameter$ID == 5738)
  checkEquals(dci_info$InputTab$AllTableParameters$ID[idx], parameter$ID[idx2])
  checkEquals(dci_info$InputTab$AllTableParameters$Time[idx], parameter$Time[idx2])
  checkEquals(dci_info$InputTab$AllTableParameters$Value[idx], parameter$Value[idx2])
  checkEquals(dci_info$InputTab$AllTableParameters$RestartSolver[idx], parameter$RestartSolver[idx2])
  
  options <- list(Type="current")
  parameter <- getTableParameter(path_id=5738, DCI_Info = dci_info, options=options)
  checkEquals(names(parameter), c("ID", "Time", "Value", "RestartSolver"))
  idx <- which(dci_info$InputTab$VariableTableParameters$ID == 5738)
  idx2 <- which(parameter$ID == 5738)
  checkEquals(dci_info$InputTab$VariableTableParameters$ID[idx], parameter$ID[idx2])
  checkEquals(dci_info$InputTab$VariableTableParameters$Time[idx], parameter$Time[idx2])
  checkEquals(dci_info$InputTab$VariableTableParameters$Value[idx], parameter$Value[idx2])
  checkEquals(dci_info$InputTab$VariableTableParameters$RestartSolver[idx], parameter$RestartSolver[idx2])
  
  options <- list(Type="variable")
  parameter <- getTableParameter(path_id=5738, DCI_Info = dci_info, options=options)
  checkEquals(names(parameter), c("ID", "Time", "Value", "RestartSolver"))
  idx <- which(dci_info$InputTab$VariableTableParameters$ID == 5738)
  idx2 <- which(parameter$ID == 5738)
  checkEquals(dci_info$InputTab$VariableTableParameters$ID[idx], parameter$ID[idx2])
  checkEquals(dci_info$InputTab$VariableTableParameters$Time[idx], parameter$Time[idx2])
  checkEquals(dci_info$InputTab$VariableTableParameters$Value[idx], parameter$Value[idx2])
  checkEquals(dci_info$InputTab$VariableTableParameters$RestartSolver[idx], parameter$RestartSolver[idx2])
  
  options <- list(Type="reference")
  parameter <- getTableParameter(path_id=5738, DCI_Info = dci_info, options=options)
  checkEquals(names(parameter), c("ID", "Time", "Value", "RestartSolver"))
  idx <- which(dci_info$InputTab$VariableTableParameters$ID == 5738)
  idx2 <- which(parameter$ID == 5738)
  checkEquals(dci_info$ReferenceTab$VariableTableParameters$ID[idx], parameter$ID[idx2])
  checkEquals(dci_info$ReferenceTab$VariableTableParameters$Time[idx], parameter$Time[idx2])
  checkEquals(dci_info$ReferenceTab$VariableTableParameters$Value[idx], parameter$Value[idx2])
  checkEquals(dci_info$ReferenceTab$VariableTableParameters$RestartSolver[idx], parameter$RestartSolver[idx2])
  
  options <- list(Type="variable")
  path_id_test = 5822;
  parameter = getTableParameter(path_id = path_id_test, DCI_Info = dci_info, options=options);
  checkEquals(names(parameter), c("ID", "Time", "Value", "RestartSolver"));
  checkEquals(length(parameter$ID), 0);
  
  options(warn = 2);
  checkException(getTableParameter(path_id = path_id_test, DCI_Info = dci_info, options=options))
  options(warn = 0);

  options <- list(Type="current")
  parameter <- getTableParameter(path_id=5822, DCI_Info = dci_info, options=options)
  checkEquals(names(parameter), c("ID", "Time", "Value", "RestartSolver"))
  idx <- which(dci_info$InputTab$AllTableParameters$ID == 5822)
  idx2 <- which(parameter$ID == 5822)
  checkEquals(dci_info$InputTab$AllTableParameters$ID[idx], parameter$ID[idx2])
  checkEquals(dci_info$InputTab$AllTableParameters$Time[idx], parameter$Time[idx2])
  checkEquals(dci_info$InputTab$AllTableParameters$Value[idx], parameter$Value[idx2])
  checkEquals(dci_info$InputTab$AllTableParameters$RestartSolver[idx], parameter$RestartSolver[idx2])
}

test.CheckParameterVectorInSimulation <- function() {
  paramList <- {}
  paramList <- initParameter(initStruct = paramList, path_id= 5738)
  paramList <- initParameter(initStruct = paramList, path_id= 5822)
  dci_info <- initSimulation(XML = simModelXML, ParamList = paramList)
  
  options <- list(Type="readonly")
  parameter <- getTableParameter(path_id=c(5738, 5822), DCI_Info = dci_info, options=options)
  checkEquals(names(parameter), c("ID", "Time", "Value", "RestartSolver"))
  idx <- which(dci_info$InputTab$AllTableParameters$ID %in% c(5738, 5822))
  idx2 <- which(parameter$ID %in% c(5738, 5822))
  checkEquals(dci_info$InputTab$AllTableParameters$ID[idx], parameter$ID[idx2])
  checkEquals(dci_info$InputTab$AllTableParameters$Time[idx], parameter$Time[idx2])
  checkEquals(dci_info$InputTab$AllTableParameters$Value[idx], parameter$Value[idx2])
  checkEquals(dci_info$InputTab$AllTableParameters$RestartSolver[idx], parameter$RestartSolver[idx2])
  
  options <- list(Type="current")
  parameter <- getTableParameter(path_id=c(5738, 5822), DCI_Info = dci_info, options=options)
  checkEquals(names(parameter), c("ID", "Time", "Value", "RestartSolver"))
  idx <- which(dci_info$InputTab$VariableTableParameters$ID %in% c(5738, 5822))
  idx2 <- which(parameter$ID %in% c(5738, 5822))
  checkEquals(dci_info$InputTab$VariableTableParameters$ID[idx], parameter$ID[idx2])
  checkEquals(dci_info$InputTab$VariableTableParameters$Time[idx], parameter$Time[idx2])
  checkEquals(dci_info$InputTab$VariableTableParameters$Value[idx], parameter$Value[idx2])
  checkEquals(dci_info$InputTab$VariableTableParameters$RestartSolver[idx], parameter$RestartSolver[idx2])
  
  options <- list(Type="variable")
  parameter <- getTableParameter(path_id=c(5738, 5822), DCI_Info = dci_info, options=options)
  checkEquals(names(parameter), c("ID", "Time", "Value", "RestartSolver"))
  idx <- which(dci_info$InputTab$VariableTableParameters$ID %in% c(5738, 5822))
  idx2 <- which(parameter$ID %in% c(5738, 5822))
  checkEquals(dci_info$InputTab$VariableTableParameters$ID[idx], parameter$ID[idx2])
  checkEquals(dci_info$InputTab$VariableTableParameters$Time[idx], parameter$Time[idx2])
  checkEquals(dci_info$InputTab$VariableTableParameters$Value[idx], parameter$Value[idx2])
  checkEquals(dci_info$InputTab$VariableTableParameters$RestartSolver[idx], parameter$RestartSolver[idx2])
  
  options <- list(Type="reference")
  parameter <- getTableParameter(path_id=c(5738, 5822), DCI_Info = dci_info, options=options)
  checkEquals(names(parameter), c("ID", "Time", "Value", "RestartSolver"))
  idx <- which(dci_info$ReferenceTab$VariableTableParameters$ID %in% c(5738, 5822))
  idx2 <- which(parameter$ID %in% c(5738, 5822))
  checkEquals(dci_info$ReferenceTab$VariableTableParameters$ID[idx], parameter$ID[idx2])
  checkEquals(dci_info$ReferenceTab$VariableTableParameters$Time[idx], parameter$Time[idx2])
  checkEquals(dci_info$ReferenceTab$VariableTableParameters$Value[idx], parameter$Value[idx2])
  checkEquals(dci_info$ReferenceTab$VariableTableParameters$RestartSolver[idx], parameter$RestartSolver[idx2])
}

test.CheckParameterVectorIndexInSimulation <- function() {
  paramList <- {}
  paramList <- initParameter(initStruct = paramList, path_id= 5738)
  paramList <- initParameter(initStruct = paramList, path_id= 5822)
  dci_info <- initSimulation(XML = simModelXML, ParamList = paramList)
  
  options <- list(Type="readonly")
  index <- existsParameter(path_id=c(5738,5822), options=options, DCI_Info=dci_info)
  options <- list(Type="readonly", Index=index$Index)
  parameter <- getTableParameter(DCI_Info = dci_info, options=options)
  checkEquals(names(parameter), c("ID", "Time", "Value", "RestartSolver"))
  idx <- which(dci_info$InputTab$AllTableParameters$ID %in% c(5738, 5822))
  idx2 <- which(parameter$ID %in% c(5738, 5822))
  checkEquals(dci_info$InputTab$AllTableParameters$ID[idx], parameter$ID[idx2])
  checkEquals(dci_info$InputTab$AllTableParameters$Time[idx], parameter$Time[idx2])
  checkEquals(dci_info$InputTab$AllTableParameters$Value[idx], parameter$Value[idx2])
  checkEquals(dci_info$InputTab$AllTableParameters$RestartSolver[idx], parameter$RestartSolver[idx2])
  
  options <- list(Type="variable")
  index <- existsParameter(path_id=c(5738,5822), options=options, DCI_Info=dci_info)
  options <- list(Type="current", Index=index$Index)
  parameter <- getTableParameter(DCI_Info = dci_info, options=options)
  checkEquals(names(parameter), c("ID", "Time", "Value", "RestartSolver"))
  idx <- which(dci_info$InputTab$VariableTableParameters$ID %in% c(5738, 5822))
  idx2 <- which(parameter$ID %in% c(5738, 5822))
  checkEquals(dci_info$InputTab$VariableTableParameters$ID[idx], parameter$ID[idx2])
  checkEquals(dci_info$InputTab$VariableTableParameters$Time[idx], parameter$Time[idx2])
  checkEquals(dci_info$InputTab$VariableTableParameters$Value[idx], parameter$Value[idx2])
  checkEquals(dci_info$InputTab$VariableTableParameters$RestartSolver[idx], parameter$RestartSolver[idx2])
  
  options <- list(Type="variable")
  index <- existsParameter(path_id=c(5738,5822), options=options, DCI_Info=dci_info)
  options <- list(Type="variable", Index=index$Index)
  parameter <- getTableParameter(DCI_Info = dci_info, options=options)
  checkEquals(names(parameter), c("ID", "Time", "Value", "RestartSolver"))
  idx <- which(dci_info$InputTab$VariableTableParameters$ID %in% c(5738, 5822))
  idx2 <- which(parameter$ID %in% c(5738, 5822))
  checkEquals(dci_info$InputTab$VariableTableParameters$ID[idx], parameter$ID[idx2])
  checkEquals(dci_info$InputTab$VariableTableParameters$Time[idx], parameter$Time[idx2])
  checkEquals(dci_info$InputTab$VariableTableParameters$Value[idx], parameter$Value[idx2])
  checkEquals(dci_info$InputTab$VariableTableParameters$RestartSolver[idx], parameter$RestartSolver[idx2])
  
  options <- list(Type="variable", isReference=TRUE)
  index <- existsParameter(path_id=c(5738,5822), options=options, DCI_Info=dci_info)
  options <- list(Type="reference", Index=index$Index)
  parameter <- getTableParameter(DCI_Info = dci_info, options=options)
  checkEquals(names(parameter), c("ID", "Time", "Value", "RestartSolver"))
  idx <- which(dci_info$ReferenceTab$VariableTableParameters$ID %in% c(5738, 5822))
  idx2 <- which(parameter$ID %in% c(5738, 5822))
  checkEquals(dci_info$ReferenceTab$VariableTableParameters$ID[idx], parameter$ID[idx2])
  checkEquals(dci_info$ReferenceTab$VariableTableParameters$Time[idx], parameter$Time[idx2])
  checkEquals(dci_info$ReferenceTab$VariableTableParameters$Value[idx], parameter$Value[idx2])
  checkEquals(dci_info$ReferenceTab$VariableTableParameters$RestartSolver[idx], parameter$RestartSolver[idx2])
}

test.CheckCurrent <- function() {
  paramList <- {}
  paramList <- initParameter(initStruct = paramList, path_id=5738)
  dci_info <- initSimulation(XML = simModelXML, ParamList = paramList)
  
  options <- list(Type="current")
  parameter <- getTableParameter(path_id=5738, DCI_Info = dci_info, options=options)
  checkEquals(names(parameter), c("ID", "Time", "Value", "RestartSolver"))
  idx <- which(dci_info$InputTab$VariableTableParameters$ID == 5738)
  idx2 <- which(parameter$ID == 5738)
  checkEquals(dci_info$InputTab$VariableTableParameters$ID[idx], parameter$ID[idx2])
  checkEquals(dci_info$InputTab$VariableTableParameters$Time[idx], parameter$Time[idx2])
  checkEquals(dci_info$InputTab$VariableTableParameters$Value[idx], parameter$Value[idx2])
  checkEquals(dci_info$InputTab$VariableTableParameters$RestartSolver[idx], parameter$RestartSolver[idx2])
}  

test.CheckVariable <- function() {
  paramList <- {}
  paramList <- initParameter(initStruct = paramList, path_id=5738)
  dci_info <- initSimulation(XML = simModelXML, ParamList = paramList)
  
  options <- list(Type="variable")
  parameter <- getTableParameter(path_id=5738, DCI_Info = dci_info, options=options)
  checkEquals(names(parameter), c("ID", "Time", "Value", "RestartSolver"))
  idx <- which(dci_info$InputTab$VariableTableParameters$ID == 5738)
  idx2 <- which(parameter$ID == 5738)
  checkEquals(dci_info$InputTab$VariableTableParameters$ID[idx], parameter$ID[idx2])
  checkEquals(dci_info$InputTab$VariableTableParameters$Time[idx], parameter$Time[idx2])
  checkEquals(dci_info$InputTab$VariableTableParameters$Value[idx], parameter$Value[idx2])
  checkEquals(dci_info$InputTab$VariableTableParameters$RestartSolver[idx], parameter$RestartSolver[idx2])
}  

test.CheckReference <- function() {
  paramList <- {}
  paramList <- initParameter(initStruct = paramList, path_id=5738)
  dci_info <- initSimulation(XML = simModelXML, ParamList = paramList)
  
  options <- list(Type="reference")
  parameter <- getTableParameter(path_id=5738, DCI_Info = dci_info, options=options)
  checkEquals(names(parameter), c("ID", "Time", "Value", "RestartSolver"))
  idx <- which(dci_info$ReferenceTab$VariableTableParameters$ID == 5738)
  idx2 <- which(parameter$ID == 5738)
  checkEquals(dci_info$ReferenceTab$VariableTableParameters$ID[idx], parameter$ID[idx2])
  checkEquals(dci_info$ReferenceTab$VariableTableParameters$Time[idx], parameter$Time[idx2])
  checkEquals(dci_info$ReferenceTab$VariableTableParameters$Value[idx], parameter$Value[idx2])
  checkEquals(dci_info$ReferenceTab$VariableTableParameters$RestartSolver[idx], parameter$RestartSolver[idx2])
}  

test.CheckReadonly <- function() {
  dci_info <- standard_dci_info
  
  options <- list(Type="readonly")
  parameter <- getTableParameter(path_id=5738, DCI_Info = dci_info, options=options)
  checkEquals(names(parameter), c("ID", "Time", "Value", "RestartSolver"))
  idx <- which(dci_info$InputTab$AllTableParameters$ID == 5738)
  idx2 <- which(parameter$ID == 5738)
  checkEquals(dci_info$InputTab$AllTableParameters$ID[idx], parameter$ID[idx2])
  checkEquals(dci_info$InputTab$AllTableParameters$Time[idx], parameter$Time[idx2])
  checkEquals(dci_info$InputTab$AllTableParameters$Value[idx], parameter$Value[idx2])
  checkEquals(dci_info$InputTab$AllTableParameters$RestartSolver[idx], parameter$RestartSolver[idx2])
}  

test.CheckParameterInSimulationNoneVariables <- function() {
  dci_info <- initSimulation(XML = simModelXML, whichInitParam="none")
  options <- list(Type="current")
  parameter <- getTableParameter(path_id=5738, DCI_Info = dci_info, options=options)
  
  checkEquals(names(parameter), c("ID", "Time", "Value", "RestartSolver"))
  idx <- which(dci_info$InputTab$AllTableParameters$ID == 5738)
  idx2 <- which(parameter$ID == 5738)
  checkEquals(dci_info$InputTab$AllTableParameters$ID[idx], parameter$ID[idx2])
  checkEquals(dci_info$InputTab$AllTableParameters$Time[idx], parameter$Time[idx2])
  checkEquals(dci_info$InputTab$AllTableParameters$Value[idx], parameter$Value[idx2])
  checkEquals(dci_info$InputTab$AllTableParameters$RestartSolver[idx], parameter$RestartSolver[idx2])

  options <- list(Type="variable")
  path_id_test = 5738;
  parameter = getTableParameter(path_id = path_id_test, DCI_Info = dci_info, options=options);
  checkEquals(names(parameter), c("ID", "Time", "Value", "RestartSolver"));
  checkEquals(length(parameter$ID), 0);
  
  options(warn = 2);
  checkException(getTableParameter(path_id = path_id_test, DCI_Info = dci_info, options=options))
  options(warn = 0);
}
