require(RUnit, quietly=TRUE)
require(MoBiToolboxForR, quietly=TRUE)
simModelXML <- "./models/TableParameters.xml"
standard_dci_info <- initSimulation(XML=simModelXML, whichInitParam="all")

test.EmptyPathID <- function() {
  dci_info <- standard_dci_info
  checkException(setTableParameter(value="", path_id="", DCI_Info = dci_info))
}

test.EmptyDCI_Info <- function() {
  checkException(setTableParameter(value="", path_id=5738, DCI_Info = {}))
}

test.InvalidTable <- function() {
  dci_info <- standard_dci_info
  checkException(setTableParameter(table="", DCI_Info = dci_info))
}

test.InvalidTableColumnIDIsMissing <- function() {
  dci_info <- standard_dci_info
  values <- getTableParameter(DCI_Info=dci_info)
  names(values)[1] <- "ID2"
  checkException(setTableParameter(table=values, DCI_Info = dci_info))
}

test.InvalidTableColumnTimeIsMissing <- function() {
  dci_info <- standard_dci_info
  values <- getTableParameter(DCI_Info=dci_info)
  names(values)[2] <- "Time2"
  checkException(setTableParameter(table=values, DCI_Info = dci_info))
}

test.InvalidTableColumnValueIsMissing <- function() {
  dci_info <- standard_dci_info
  values <- getTableParameter(DCI_Info=dci_info)
  names(values)[3] <- "Value2"
  checkException(setTableParameter(table=values, DCI_Info = dci_info))
}

test.InvalidTableColumnRestartSolverIsMissing <- function() {
  dci_info <- standard_dci_info
  values <- getTableParameter(DCI_Info=dci_info)
  names(values)[4] <- "RestartSolver2"
  checkException(setTableParameter(table=values, DCI_Info = dci_info))
}

test.InvalidTableColumnIDIsMissing2 <- function() {
  dci_info <- standard_dci_info
  values <- getTableParameter(DCI_Info=dci_info)
  values <- as.list(as.data.frame(values)[, c(2, 2, 3, 4)])
  checkException(setTableParameter(table=values, DCI_Info = dci_info))
}
test.InvalidTableColumnTimeIsMissing2 <- function() {
  dci_info <- standard_dci_info
  values <- getTableParameter(DCI_Info=dci_info)
  values <- as.list(as.data.frame(values)[, c(1, 3, 3, 4)])
  checkException(setTableParameter(table=values, DCI_Info = dci_info))
}
test.InvalidTableColumnValueIsMissing2 <- function() {
  dci_info <- standard_dci_info
  values <- getTableParameter(DCI_Info=dci_info)
  values <- as.list(as.data.frame(values)[, c(1, 2, 4, 4)])
  checkException(setTableParameter(table=values, DCI_Info = dci_info))
}
test.InvalidTableColumnRestartSolverIsMissing2 <- function() {
  dci_info <- standard_dci_info
  values <- getTableParameter(DCI_Info=dci_info)
  values <- as.list(as.data.frame(values)[, c(1, 2, 3, 3)])
  checkException(setTableParameter(table=values, DCI_Info = dci_info))
}

test.CheckTypes <- function() {
  paramList <- {}
  paramList <- initParameter(initStruct = paramList, path_id=5738)
  dci_info <- initSimulation(XML = simModelXML, ParamList = paramList)
  values <- getTableParameter(DCI_Info=dci_info)
  
  options <- list(Type="readonly")
  checkException(setTableParameter(table=values, DCI_Info = dci_info, options=options))
  
  options <- list(Type="current")
  checkException(setTableParameter(table=values, DCI_Info = dci_info, options=options))
  
  options <- list(Type="variable")
  parameter <- getTableParameter(path_id=5738, DCI_Info = dci_info, options=options)
  dci_info_check <- setTableParameter(table=values, DCI_Info = dci_info, options=options) 
  idx <- which(dci_info$InputTab$VariableTableParameters$ID == 5738)
  idx2 <- which(parameter$ID == 5738)
  checkEquals(dci_info_check$InputTab$VariableTableParameters$ID[idx], parameter$ID[idx2])
  checkEquals(dci_info_check$InputTab$VariableTableParameters$Time[idx], parameter$Time[idx2])
  checkEquals(dci_info_check$InputTab$VariableTableParameters$Value[idx], parameter$Value[idx2])
  checkEquals(dci_info_check$InputTab$VariableTableParameters$RestartSolver[idx], parameter$RestartSolver[idx2])
  
  options <- list(Type="reference")
  parameter <- getTableParameter(path_id=5738, DCI_Info = dci_info, options=options)
  dci_info_check <- setTableParameter(table=values, DCI_Info = dci_info, options=options)  
  idx <- which(dci_info$ReferenceTab$VariableTableParameters$ID == 5738)
  idx2 <- which(parameter$ID == 5738)
  checkEquals(dci_info_check$ReferenceTab$VariableTableParameters$ID[idx], parameter$ID[idx2])
  checkEquals(dci_info_check$ReferenceTab$VariableTableParameters$Time[idx], parameter$Time[idx2])
  checkEquals(dci_info_check$ReferenceTab$VariableTableParameters$Value[idx], parameter$Value[idx2])
  checkEquals(dci_info_check$ReferenceTab$VariableTableParameters$RestartSolver[idx], parameter$RestartSolver[idx2])
}

test.CheckParameterInSimulation <- function() {
  paramList <- {}
  paramList <- initParameter(initStruct = paramList, path_id=5738)
  dci_info <- initSimulation(XML = simModelXML, ParamList = paramList)
  values <- getTableParameter(DCI_Info=dci_info)
  
  options <- list(Type="readonly")
  checkException(setTableParameter(table=values, DCI_Info = dci_info, options=options))
  
  options <- list(Type="variable")
  parameter <- getTableParameter(path_id=5738, DCI_Info = dci_info, options=options)
  dci_info_check <- setTableParameter(table=values, DCI_Info = dci_info, options=options)  
  idx <- which(dci_info$ReferenceTab$VariableTableParameters$ID == 5738)
  idx2 <- which(parameter$ID == 5738)
  checkEquals(dci_info_check$ReferenceTab$VariableTableParameters$ID[idx], parameter$ID[idx2])
  checkEquals(dci_info_check$ReferenceTab$VariableTableParameters$Time[idx], parameter$Time[idx2])
  checkEquals(dci_info_check$ReferenceTab$VariableTableParameters$Value[idx], parameter$Value[idx2])
  checkEquals(dci_info_check$ReferenceTab$VariableTableParameters$RestartSolver[idx], parameter$RestartSolver[idx2])
}

test.CheckParameterInSimulationNoneVariables <- function() {
  dci_info <- initSimulation(XML = simModelXML, whichInitParam="none")
  options <- list(Type="readonly")
  values <- getTableParameter(path_id=5738, DCI_Info = dci_info, options=options)
  
  options <- list(Type="variable")
  checkException(getTableParameter(path_id=5738, DCI_Info = dci_info, options=options))  
  checkException(setTableParameter(table=values, DCI_Info = dci_info, options=options))
}