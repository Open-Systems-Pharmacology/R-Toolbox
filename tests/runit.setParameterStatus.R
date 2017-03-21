require(RUnit, quietly=TRUE)
require(MoBiToolboxForR, quietly=TRUE)
simModelXML <- "./models/TableParameters.xml"
standard_dci_info <- initSimulation(XML=simModelXML, whichInitParam="none")
all_dci_info <- initSimulation(XML=simModelXML, whichInitParam="all")

test.EmptyDCI_Info <- function() {
  checkException(setParameterStatus(DCI_Info = {}))
}

test.EmptyParameterStatus <- function() {
  dci_info <- standard_dci_info
  checkException(setParameterStatus(ParameterStatus={}, DCI_Info = dci_info))
}

test.InvalidParameterStatus <- function() {
  dci_info <- all_dci_info
  parameterStatus <- getParameterStatus(options=list(Type="variable"), DCI_Info=dci_info)
  for (i in 1:length(parameterStatus)) {
    checkException(setParameterStatus(ParameterStatus=parameterStatus[-i], DCI_Info = dci_info))
  }
}

test.NoneVariables <- function() {
  dci_info <- standard_dci_info
  parameterStatus <- getParameterStatus(options=list(Type="variable"), DCI_Info=dci_info)
  dci_info_check <- setParameterStatus(ParameterStatus=parameterStatus, DCI_Info = dci_info)
  checkEquals(dci_info, dci_info_check)
  
  parameterStatus <- getParameterStatus(options=list(Type="reference"), DCI_Info=dci_info)
  dci_info_check <- setParameterStatus(ParameterStatus=parameterStatus, DCI_Info = dci_info)
  checkEquals(dci_info, dci_info_check)
}

test.AllChangedVariable <- function() {
  dci_info <- all_dci_info
  parameterStatus <- getParameterStatus(options=list(Type="variable"), DCI_Info=dci_info)
  
  #double all values
  parameterStatus$Parameters$Value <- parameterStatus$Parameters$Value * 2
  parameterStatus$TableParameters$Value <-   parameterStatus$TableParameters$Value*2
  parameterStatus$SpeciesInitialValues$Value <- parameterStatus$SpeciesInitialValues$Value * 2
  parameterStatus$SpeciesScaleFactors$Value <- parameterStatus$SpeciesScaleFactors$Value * 2
  parameterStatus$SimulationTime$Time <- parameterStatus$SimulationTime$Time * 2
  
  dci_info_check <- setParameterStatus(ParameterStatus=parameterStatus, DCI_Info=dci_info)
  newParameterStatus <- getParameterStatus(options=list(Type="variable"), DCI_Info=dci_info_check)
  
  #check parameters
  idx <- which(dci_info$InputTab$VariableParameters$ParameterType != "Table")
  checkEqualsNumeric(parameterStatus$Parameters$Value[idx], dci_info$InputTab$VariableParameters$Value[idx]*2)
  checkEqualsNumeric(newParameterStatus$Parameters$Value[idx], dci_info_check$InputTab$VariableParameters$Value[idx])
  checkEqualsNumeric(newParameterStatus$Parameters$Value[idx], dci_info$InputTab$VariableParameters$Value[idx]*2)
  idx <- which(dci_info$ReferenceTab$VariableParameters$ParameterType == "Table")
  checkEqualsNumeric(parameterStatus$Parameters$Value[idx], 
                     dci_info$InputTab$VariableTableParameters$Value[dci_info$InputTab$VariableTableParameters$Time == 0]*2)
  checkEqualsNumeric(newParameterStatus$Parameters$Value[idx], 
                     dci_info_check$InputTab$VariableTableParameters$Value[dci_info$InputTab$VariableTableParameters$Time == 0])
  checkEqualsNumeric(newParameterStatus$Parameters$Value[idx], 
                     dci_info$InputTab$VariableTableParameters$Value[dci_info$InputTab$VariableTableParameters$Time == 0]*2)
  
  #check table parameters
  checkEqualsNumeric(parameterStatus$TableParameters$Value, dci_info$InputTab$VariableTableParameters$Value*2)
  checkEqualsNumeric(newParameterStatus$TableParameters$Value, dci_info_check$InputTab$VariableTableParameters$Value)
  checkEqualsNumeric(newParameterStatus$TableParameters$Value, dci_info$InputTab$VariableTableParameters$Value*2)
  
  #check species initial values
  checkEqualsNumeric(parameterStatus$SpeciesInitialValues$Value, dci_info$InputTab$VariableSpecies$InitialValue*2)
  checkEqualsNumeric(newParameterStatus$SpeciesInitialValues$Value, dci_info_check$InputTab$VariableSpecies$InitialValue)
  checkEqualsNumeric(newParameterStatus$SpeciesInitialValues$Value, dci_info$InputTab$VariableSpecies$InitialValue*2)
  
  #check species scale factors
  checkEqualsNumeric(parameterStatus$SpeciesScaleFactors$Value, dci_info$InputTab$VariableSpecies$ScaleFactor*2)
  checkEqualsNumeric(newParameterStatus$SpeciesScaleFactors$Value, dci_info_check$InputTab$VariableSpecies$ScaleFactor)
  checkEqualsNumeric(newParameterStatus$SpeciesScaleFactors$Value, dci_info$InputTab$VariableSpecies$ScaleFactor*2)
  
  #check parameters
  time <- getSimulationTime(DCI_Info=dci_info)
  checkEqualsNumeric(parameterStatus$SimulationTime$Time, time$Time*2)
  newTime <- getSimulationTime(DCI_Info=dci_info_check)
  checkEqualsNumeric(newParameterStatus$SimulationTime$Time, newTime$Time)
  checkEqualsNumeric(newParameterStatus$SimulationTime$Time, time$Time*2)
}

test.AllChangedReference <- function() {
  dci_info <- all_dci_info
  parameterStatus <- getParameterStatus(options=list(Type="reference"), DCI_Info=dci_info)
  
  #double all values
  parameterStatus$Parameters$Value <- parameterStatus$Parameters$Value * 2
  parameterStatus$TableParameters$Value <-   parameterStatus$TableParameters$Value*2
  parameterStatus$SpeciesInitialValues$Value <- parameterStatus$SpeciesInitialValues$Value * 2
  parameterStatus$SpeciesScaleFactors$Value <- parameterStatus$SpeciesScaleFactors$Value * 2
  parameterStatus$SimulationTime$Time <- parameterStatus$SimulationTime$Time * 2
  
  dci_info_check <- setParameterStatus(ParameterStatus=parameterStatus, DCI_Info=dci_info)
  newParameterStatus <- getParameterStatus(options=list(Type="reference"), DCI_Info=dci_info_check)
  
  #check parameters
  idx <- which(dci_info$ReferenceTab$VariableParameters$ParameterType != "Table")
  checkEqualsNumeric(parameterStatus$Parameters$Value[idx], dci_info$ReferenceTab$VariableParameters$Value[idx]*2)
  checkEqualsNumeric(newParameterStatus$Parameters$Value[idx], dci_info_check$ReferenceTab$VariableParameters$Value[idx])
  checkEqualsNumeric(newParameterStatus$Parameters$Value[idx], dci_info$ReferenceTab$VariableParameters$Value[idx]*2)
  idx <- which(dci_info$ReferenceTab$VariableParameters$ParameterType == "Table")
  checkEqualsNumeric(parameterStatus$Parameters$Value[idx], 
                     dci_info$ReferenceTab$VariableTableParameters$Value[dci_info$ReferenceTab$VariableTableParameters$Time == 0]*2)
  checkEqualsNumeric(newParameterStatus$Parameters$Value[idx], 
                     dci_info_check$ReferenceTab$VariableTableParameters$Value[dci_info$ReferenceTab$VariableTableParameters$Time == 0])
  checkEqualsNumeric(newParameterStatus$Parameters$Value[idx], 
                     dci_info$ReferenceTab$VariableTableParameters$Value[dci_info$ReferenceTab$VariableTableParameters$Time == 0]*2)
  
  #check table parameters
  checkEqualsNumeric(parameterStatus$TableParameters$Value, dci_info$ReferenceTab$VariableTableParameters$Value*2)
  checkEqualsNumeric(newParameterStatus$TableParameters$Value, dci_info_check$ReferenceTab$VariableTableParameters$Value)
  checkEqualsNumeric(newParameterStatus$TableParameters$Value, dci_info$ReferenceTab$VariableTableParameters$Value*2)
  
  #check species initial values
  checkEqualsNumeric(parameterStatus$SpeciesInitialValues$Value, dci_info$ReferenceTab$VariableSpecies$InitialValue*2)
  checkEqualsNumeric(newParameterStatus$SpeciesInitialValues$Value, dci_info_check$ReferenceTab$VariableSpecies$InitialValue)
  checkEqualsNumeric(newParameterStatus$SpeciesInitialValues$Value, dci_info$ReferenceTab$VariableSpecies$InitialValue*2)
  
  #check species scale factors
  checkEqualsNumeric(parameterStatus$SpeciesScaleFactors$Value, dci_info$ReferenceTab$VariableSpecies$ScaleFactor*2)
  checkEqualsNumeric(newParameterStatus$SpeciesScaleFactors$Value, dci_info_check$ReferenceTab$VariableSpecies$ScaleFactor)
  checkEqualsNumeric(newParameterStatus$SpeciesScaleFactors$Value, dci_info$ReferenceTab$VariableSpecies$ScaleFactor*2)
  
  #check parameters
  time <- getSimulationTime(DCI_Info=dci_info)
  checkEqualsNumeric(parameterStatus$SimulationTime$Time, time$Time*2)
  newTime <- getSimulationTime(DCI_Info=dci_info_check)
  checkEqualsNumeric(newParameterStatus$SimulationTime$Time, newTime$Time)
  checkEqualsNumeric(newParameterStatus$SimulationTime$Time, time$Time*2)
}
