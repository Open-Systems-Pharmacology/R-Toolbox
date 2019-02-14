#require(RUnit, quietly=TRUE)
#require(MoBiToolboxForR, quietly=TRUE)
simModelXML <- "./tests/models/TableParameters.xml"
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
  path_id_test = 5738;
  parameter = getTableParameter(path_id = path_id_test, DCI_Info = dci_info, options=options);
  checkEquals(names(parameter), c("ID", "Time", "Value", "RestartSolver"));
  checkEquals(length(parameter$ID), 0);
  
  options(warn = 2);
  checkException(getTableParameter(path_id = path_id_test, DCI_Info = dci_info, options=options))
  options(warn = 0);

  checkException(setTableParameter(table=values, DCI_Info = dci_info, options=options))
}

#Test for new values applied in the simulation
test.CheckNewValues = function(){
  paramPath = "*|Applications|T1|Fraction (dose)";
  #Initialize the table parameter
  initStruct = list();
  initStruct = initParameter(initStruct = initStruct, path_id = paramPath);
  #Initialize simulation
  myDCI = initSimulation(XML = simModelXML, ParamList = initStruct);
  
  #Simulate and plot the value defined by the table parameter
  myDCI = processSimulation(DCI_Info = myDCI);

  #First, get the current table of the parameter of interest. This will be the table we will edit and re-write into the DCI.
  tableParam_old = getTableParameter(path_id = paramPath, DCI_Info = myDCI);
  
  #Change the table
  #Get the length of the table
  table_length = length(tableParam_old$Time);
  new_time = c(0, 30, 250);
  new_values = c(2, 1.5,7);
  #Change the time and values of the table
  for (i in 1:table_length){
    tableParam_old$Time[i] = new_time[i];
    tableParam_old$Value[i] = new_values[i];
  }
  
  #Update the table of a parameter
  myDCI = setTableParameter(table = tableParam_old, DCI_Info = myDCI);
  
  #Simulate with new table values
  myDCI = processSimulation(DCI_Info = myDCI);

  #Check the new table parameter
  tableParam_new = getTableParameter(path_id = paramPath, DCI_Info = myDCI);
  checkEqualsNumeric(tableParam_new$Time, new_time);
  checkEqualsNumeric(tableParam_new$Value, new_values);
}