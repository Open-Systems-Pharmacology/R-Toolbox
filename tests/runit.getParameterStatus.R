#require(RUnit, quietly=TRUE)
#require(MoBiToolboxForR, quietly=TRUE)
simModelXML <- "./tests/models/TableParameters.xml"
standard_dci_info <- initSimulation(XML=simModelXML, whichInitParam="none")

test.EmptyDCI_Info <- function() {
  checkException(getParameterStatus(DCI_Info = {}))
}

test.CheckTypes <- function() {
  dci_info <- standard_dci_info
  checkException(getParameterStatus(DCI_Info = dci_info, options=list(Type="Unknown")))
  checkException(getParameterStatus(DCI_Info = dci_info, options=list(Type="readonly")))
  checkException(getParameterStatus(DCI_Info = dci_info, options=list(Type="current")))
}

test.DefaultOptions <- function() {
  dci_info <- standard_dci_info
  parameterStatus <- getParameterStatus(DCI_Info=dci_info)
  
  checkTrue(parameterStatus$ParameterType == "variable")
  checkEquals(names(parameterStatus), c("ParameterType", "Parameters", "TableParameters", "SpeciesInitialValues", "SpeciesScaleFactors", "SimulationTime"))
  checkEquals(names(parameterStatus$Parameters), c("Value", "Index"))
  checkEquals(names(parameterStatus$TableParameters), c("ID", "Time", "Value", "RestartSolver"))
  checkEquals(names(parameterStatus$SpeciesInitialValues), c("Value", "Index"))
  checkEquals(names(parameterStatus$SpeciesScaleFactors), c("Value", "Index"))
  checkEquals(names(parameterStatus$SimulationTime), c("Time", "Pattern"))
  
  checkEquals(length(parameterStatus$Parameters$Index), 0)
  checkEquals(length(parameterStatus$TableParameters$ID), 0)
  checkEquals(length(parameterStatus$TableParameters$Time), 0)
  checkEquals(length(parameterStatus$TableParameters$Value), 0)
  checkEquals(length(parameterStatus$TableParameters$RestartSolver), 0)
  checkEquals(length(parameterStatus$SpeciesInitialValues$Value), 0)
  checkEquals(length(parameterStatus$SpeciesInitialValues$Index), 0)
  checkEquals(length(parameterStatus$SpeciesScaleFactors$Value), 0)
  checkEquals(length(parameterStatus$SpeciesScaleFactors$Index), 0)
  
  time <- getSimulationTime(DCI_Info=dci_info)
  checkEquals(parameterStatus$SimulationTime$Time, time$Time)
  checkEquals(parameterStatus$SimulationTime$Pattern, time$Pattern)
}

test.NoneVariablesVariable <- function() {
  dci_info <- standard_dci_info
  options <- list(Type="variable")
  parameterStatus <- getParameterStatus(DCI_Info=dci_info, options=options)
  
  checkTrue(parameterStatus$ParameterType == "variable")
  checkEquals(names(parameterStatus), c("ParameterType", "Parameters", "TableParameters", "SpeciesInitialValues", "SpeciesScaleFactors", "SimulationTime"))
  checkEquals(names(parameterStatus$Parameters), c("Value", "Index"))
  checkEquals(names(parameterStatus$TableParameters), c("ID", "Time", "Value", "RestartSolver"))
  checkEquals(names(parameterStatus$SpeciesInitialValues), c("Value", "Index"))
  checkEquals(names(parameterStatus$SpeciesScaleFactors), c("Value", "Index"))
  checkEquals(names(parameterStatus$SimulationTime), c("Time", "Pattern"))
  
  checkEquals(length(parameterStatus$Parameters$Value), 0)
  checkEquals(length(parameterStatus$Parameters$Index), 0)
  checkEquals(length(parameterStatus$TableParameters$ID), 0)
  checkEquals(length(parameterStatus$TableParameters$Time), 0)
  checkEquals(length(parameterStatus$TableParameters$Value), 0)
  checkEquals(length(parameterStatus$TableParameters$RestartSolver), 0)
  checkEquals(length(parameterStatus$SpeciesInitialValues$Value), 0)
  checkEquals(length(parameterStatus$SpeciesInitialValues$Index), 0)
  checkEquals(length(parameterStatus$SpeciesScaleFactors$Value), 0)
  checkEquals(length(parameterStatus$SpeciesScaleFactors$Index), 0)
  
  time <- getSimulationTime(DCI_Info=dci_info)
  checkEquals(parameterStatus$SimulationTime$Time, time$Time)
  checkEquals(parameterStatus$SimulationTime$Pattern, time$Pattern)
}

test.NoneVariablesReference <- function() {
  dci_info <- standard_dci_info
  options <- list(Type="reference")
  parameterStatus <- getParameterStatus(DCI_Info=dci_info, options=options)
  
  checkTrue(parameterStatus$ParameterType == "reference")
  checkEquals(names(parameterStatus), c("ParameterType", "Parameters", "TableParameters", "SpeciesInitialValues", "SpeciesScaleFactors", "SimulationTime"))
  checkEquals(names(parameterStatus$Parameters), c("Value", "Index"))
  checkEquals(names(parameterStatus$TableParameters), c("ID", "Time", "Value", "RestartSolver"))
  checkEquals(names(parameterStatus$SpeciesInitialValues), c("Value", "Index"))
  checkEquals(names(parameterStatus$SpeciesScaleFactors), c("Value", "Index"))
  checkEquals(names(parameterStatus$SimulationTime), c("Time", "Pattern"))
  
  checkEquals(length(parameterStatus$Parameters$Value), 0)
  checkEquals(length(parameterStatus$Parameters$Index), 0)
  checkEquals(length(parameterStatus$TableParameters$ID), 0)
  checkEquals(length(parameterStatus$TableParameters$Time), 0)
  checkEquals(length(parameterStatus$TableParameters$Value), 0)
  checkEquals(length(parameterStatus$TableParameters$RestartSolver), 0)
  checkEquals(length(parameterStatus$SpeciesInitialValues$Value), 0)
  checkEquals(length(parameterStatus$SpeciesInitialValues$Index), 0)
  checkEquals(length(parameterStatus$SpeciesScaleFactors$Value), 0)
  checkEquals(length(parameterStatus$SpeciesScaleFactors$Index), 0)
  
  time <- getSimulationTime(DCI_Info=dci_info)
  checkEquals(parameterStatus$SimulationTime$Time, time$Time)
  checkEquals(parameterStatus$SimulationTime$Pattern, time$Pattern)
}

test.AllVariablesVariable <- function() {
  dci_info <- initSimulation(XML=simModelXML, whichInitParam="all")
  options <- list(Type="variable")
  parameterStatus <- getParameterStatus(DCI_Info=dci_info, options=options)
  
  checkTrue(parameterStatus$ParameterType == "variable")
  checkEquals(names(parameterStatus), c("ParameterType", "Parameters", "TableParameters", "SpeciesInitialValues", "SpeciesScaleFactors", "SimulationTime"))
  checkEquals(names(parameterStatus$Parameters), c("Value", "Index"))
  checkEquals(names(parameterStatus$TableParameters), c("ID", "Time", "Value", "RestartSolver"))
  checkEquals(names(parameterStatus$SpeciesInitialValues), c("Value", "Index"))
  checkEquals(names(parameterStatus$SpeciesScaleFactors), c("Value", "Index"))
  checkEquals(names(parameterStatus$SimulationTime), c("Time", "Pattern"))
  
  idx <- which(dci_info$InputTab$VariableParameters$ParameterType != "Table")
  checkEqualsNumeric(parameterStatus$Parameters$Value[idx], dci_info$InputTab$VariableParameters$Value[idx])
  idx <- which(dci_info$InputTab$VariableParameters$ParameterType == "Table")  
  checkEqualsNumeric(parameterStatus$Parameters$Value[idx], 
                     dci_info$InputTab$VariableTableParameters$Value[dci_info$InputTab$VariableTableParameters$Time == 0])
  checkEqualsNumeric(length(parameterStatus$Parameters$Index), length(dci_info$InputTab$VariableParameters$ID))
  checkEqualsNumeric(length(parameterStatus$TableParameters$ID), length(dci_info$InputTab$VariableTableParameters$ID))
  checkEqualsNumeric(length(parameterStatus$TableParameters$Time), length(dci_info$InputTab$VariableTableParameters$Time))
  checkEqualsNumeric(length(parameterStatus$TableParameters$Value), length(dci_info$InputTab$VariableTableParameters$Value))
  checkEqualsNumeric(length(parameterStatus$TableParameters$RestartSolver), length(dci_info$InputTab$VariableTableParameters$RestartSolver))
  checkEqualsNumeric(parameterStatus$SpeciesInitialValues$Value, dci_info$InputTab$VariableSpecies$InitialValue)
  checkEqualsNumeric(length(parameterStatus$SpeciesInitialValues$Index), length(dci_info$InputTab$VariableSpecies$ID))
  checkEqualsNumeric(parameterStatus$SpeciesScaleFactors$Value, dci_info$InputTab$VariableSpecies$ScaleFactor)
  checkEqualsNumeric(length(parameterStatus$SpeciesScaleFactors$Index), length(dci_info$InputTab$VariableSpecies$ID))
  
  time <- getSimulationTime(DCI_Info=dci_info)
  checkEquals(parameterStatus$SimulationTime$Time, time$Time)
  checkEquals(parameterStatus$SimulationTime$Pattern, time$Pattern)
}

test.AllVariablesReference <- function() {
  dci_info <- initSimulation(XML=simModelXML, whichInitParam="all")
  options <- list(Type="reference")
  parameterStatus <- getParameterStatus(DCI_Info=dci_info, options=options)
  
  checkTrue(parameterStatus$ParameterType == "reference")
  checkEquals(names(parameterStatus), c("ParameterType", "Parameters", "TableParameters", "SpeciesInitialValues", "SpeciesScaleFactors", "SimulationTime"))
  checkEquals(names(parameterStatus$Parameters), c("Value", "Index"))
  checkEquals(names(parameterStatus$TableParameters), c("ID", "Time", "Value", "RestartSolver"))
  checkEquals(names(parameterStatus$SpeciesInitialValues), c("Value", "Index"))
  checkEquals(names(parameterStatus$SpeciesScaleFactors), c("Value", "Index"))
  checkEquals(names(parameterStatus$SimulationTime), c("Time", "Pattern"))
  
  idx <- which(dci_info$ReferenceTab$VariableParameters$ParameterType != "Table")
  checkEqualsNumeric(parameterStatus$Parameters$Value[idx], dci_info$ReferenceTab$VariableParameters$Value[idx])
  idx <- which(dci_info$ReferenceTab$VariableParameters$ParameterType == "Table")  
  checkEqualsNumeric(parameterStatus$Parameters$Value[idx], 
                     dci_info$ReferenceTab$VariableTableParameters$Value[dci_info$ReferenceTab$VariableTableParameters$Time == 0])
  checkEqualsNumeric(length(parameterStatus$Parameters$Index), length(dci_info$ReferenceTab$VariableParameters$ID))
  checkEqualsNumeric(length(parameterStatus$TableParameters$ID), length(dci_info$ReferenceTab$VariableTableParameters$ID))
  checkEqualsNumeric(length(parameterStatus$TableParameters$Time), length(dci_info$ReferenceTab$VariableTableParameters$Time))
  checkEqualsNumeric(length(parameterStatus$TableParameters$Value), length(dci_info$ReferenceTab$VariableTableParameters$Value))
  checkEqualsNumeric(length(parameterStatus$TableParameters$RestartSolver), length(dci_info$ReferenceTab$VariableTableParameters$RestartSolver))
  checkEqualsNumeric(parameterStatus$SpeciesInitialValues$Value, dci_info$ReferenceTab$VariableSpecies$InitialValue)
  checkEqualsNumeric(length(parameterStatus$SpeciesInitialValues$Index), length(dci_info$ReferenceTab$VariableSpecies$ID))
  checkEqualsNumeric(parameterStatus$SpeciesScaleFactors$Value, dci_info$ReferenceTab$VariableSpecies$ScaleFactor)
  checkEqualsNumeric(length(parameterStatus$SpeciesScaleFactors$Index), length(dci_info$ReferenceTab$VariableSpecies$ID))
  
  time <- getSimulationTime(DCI_Info=dci_info)
  checkEquals(parameterStatus$SimulationTime$Time, time$Time)
  checkEquals(parameterStatus$SimulationTime$Pattern, time$Pattern)
}