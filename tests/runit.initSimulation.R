#require(RUnit, quietly=TRUE)
#require(MoBiToolboxForR, quietly=TRUE)
simModelXML <- "./tests/models/black american girl.xml"
standard_dci_info <- initSimulation(XML=simModelXML, whichInitParam="none")

test.inputXMLIsMissing <- function() {
  errorMessage <- "Input \"XML\" is missing."
  checkException(initSimulation(XML = ), msg="TEST")
}

test.ParamListIsEmpty <- function() {
  errorMessage <- "No parameters can be initialized. Please provide either 'ParamList' or 'whichInitParam' with one the following valid keywords: all, none, allnonFormula."
  checkException(initSimulation(XML = simModelXML, ParamList = {}))
}

test.whichInitParam_Unknown <- function() {
  checkException(initSimulation(XML = simModelXML, whichInitParam="Unknown"))
}

test.whichInitParam_all <- function() {
  dci_info <- initSimulation(XML = simModelXML, whichInitParam="all")
  checkEqualsNumeric(length(dci_info$InputTab$VariableParameters$Path), length(dci_info$InputTab$AllParameters$Path))
}

test.whichInitParam_All <- function() {
  dci_info <- initSimulation(XML = simModelXML, whichInitParam="All")
  checkEqualsNumeric(length(dci_info$InputTab$VariableParameters$Path), length(dci_info$InputTab$AllParameters$Path))
}

test.whichInitParam_None <- function() {
  dci_info <- initSimulation(XML = simModelXML, whichInitParam="None")
  checkEqualsNumeric(length(dci_info$InputTab$VariableParameters$Path), 0)
}

test.whichInitParam_none <- function() {
  dci_info <- initSimulation(XML = simModelXML, whichInitParam="none")
  checkEqualsNumeric(length(dci_info$InputTab$VariableParameters$Path), 0)
}

# test.whichInitParam_allNonFormula <- function() {
#   dci_info <- initSimulation(XML = simModelXML, whichInitParam="allNonFormula")
#   checkEqualsNumeric(length(dci_info$InputTab$VariableParameters$Path), length(dci_info$InputTab$AllParameters$Path[which(dci_info$InputTab$AllParameters$ParameterType != "Formula")]))
# }

test.Version_6_0 <- function() {
  dci_info <- initSimulation(XML = simModelXML, whichInitParam="none", Version="6_0")
  checkEqualsNumeric(length(dci_info$InputTab$VariableParameters$Path), 0)
}

test.Parameter_List <- function() {
  paramList <- {}
  paramList <- initParameter(initStruct = paramList, path_id=113)
  dci_info <- initSimulation(XML = simModelXML, ParamList = paramList)
  checkEqualsNumeric(length(dci_info$InputTab$VariableParameters$Path), 1)
  checkEquals(dci_info$InputTab$VariableParameters$ID[1], 113)
}

test.CheckDCIInfoStructure <- function() {
  dci_info <- standard_dci_info
  checkEquals(names(dci_info), c("MOBI_SETTINGS", "Handle", "InputTab", "ReferenceTab"))
  checkEquals(names(dci_info$MOBI_SETTINGS), c("SimModelSchema","SimModelComp","RInterface"))
  checkTrue(file.exists(dci_info$MOBI_SETTINGS$SimModelSchema))
  checkTrue(file.exists(dci_info$MOBI_SETTINGS$SimModelComp))
  checkTrue(file.exists(dci_info$MOBI_SETTINGS$RInterface))
  checkEquals(names(dci_info$InputTab), c("AllParameters","VariableParameters","AllSpecies","VariableSpecies", "AllObservers","TimeSchema","SpeciesTimeSchema", "AllTableParameters", "VariableTableParameters"))
  checkEquals(names(dci_info$InputTab$AllParameters),c("ID","Path","Value","Unit","ParameterType","Formula","Description", "UsedInSimulation"))
  checkEquals(names(attributes(dci_info$InputTab$AllParameters$ID)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$AllParameters$Path)), c("Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$AllParameters$Value)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$AllParameters$Unit)), c("Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$AllParameters$ParameterType)), c("Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$AllParameters$Formula)), c("Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$AllParameters$Description)), c("Name","Description"))
  checkEquals(names(dci_info$InputTab$VariableParameters),c("ID","Path","Value","Unit","ParameterType","Formula","Description", "UsedInSimulation"))
  checkEquals(names(attributes(dci_info$InputTab$VariableParameters$ID)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$VariableParameters$Path)), c("Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$VariableParameters$Value)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$VariableParameters$Unit)), c("Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$VariableParameters$ParameterType)), c("Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$VariableParameters$Formula)), c("Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$VariableParameters$Description)), c("Name","Description"))
  checkEquals(names(dci_info$InputTab$AllSpecies),c("ID","Path","InitialValue", "ScaleFactor","Unit","IsFormula","Formula","Description"))
  checkEquals(names(attributes(dci_info$InputTab$AllSpecies$ID)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$AllSpecies$Path)), c("Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$AllSpecies$InitialValue)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$AllSpecies$ScaleFactor)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$AllSpecies$Unit)), c("Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$AllSpecies$IsFormula)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$AllSpecies$Formula)), c("Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$AllSpecies$Description)), c("Name","Description"))
  checkEquals(names(dci_info$InputTab$VariableSpecies),c("ID","Path","InitialValue", "ScaleFactor","Unit","IsFormula","Formula","Description"))
  checkEquals(names(attributes(dci_info$InputTab$VariableSpecies$ID)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$VariableSpecies$Path)), c("Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$VariableSpecies$InitialValue)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$VariableSpecies$ScaleFactor)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$VariableSpecies$Unit)), c("Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$VariableSpecies$IsFormula)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$VariableSpecies$Formula)), c("Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$VariableSpecies$Description)), c("Name","Description"))
  checkEquals(names(dci_info$InputTab$AllObservers),c("ID","Path","Unit","Formula","Description"))
  checkEquals(names(attributes(dci_info$InputTab$AllObservers$ID)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$AllObservers$Path)), c("Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$AllObservers$Unit)), c("Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$AllObservers$Formula)), c("Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$AllObservers$Description)), c("Name","Description"))
  checkEquals(names(dci_info$InputTab$TimeSchema),c("StartTime","EndTime","Unit","NoOfTimePoints","Distribution"))
  checkEquals(names(attributes(dci_info$InputTab$TimeSchema$StartTime)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$TimeSchema$EndTime)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$TimeSchema$Unit)), c("Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$TimeSchema$NoOfTimePoints)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$TimeSchema$Distribution)), c("Name","Description"))
  checkEquals(names(dci_info$InputTab$SpeciesTimeSchema),c("ID","Path","StartTime","EndTime","Unit","NoOfTimePoints","Distribution"))
  checkEquals(names(attributes(dci_info$InputTab$SpeciesTimeSchema$StartTime)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$SpeciesTimeSchema$EndTime)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$SpeciesTimeSchema$Unit)), c("Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$SpeciesTimeSchema$NoOfTimePoints)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$SpeciesTimeSchema$Distribution)), c("Name","Description"))
  checkEquals(names(dci_info$InputTab$AllTableParameters),c("ID", "Time", "Value", "RestartSolver"))
  checkEquals(names(attributes(dci_info$InputTab$AllTableParameters$ID)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$AllTableParameters$Time)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$AllTableParameters$Value)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$AllTableParameters$RestartSolver)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(dci_info$InputTab$VariableTableParameters),c("ID", "Time", "Value", "RestartSolver"))
  checkEquals(names(attributes(dci_info$InputTab$VariableTableParameters$ID)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$VariableTableParameters$Time)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$VariableTableParameters$Value)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$InputTab$VariableTableParameters$RestartSolver)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  
  checkEquals(names(dci_info$ReferenceTab), c("AllParameters","VariableParameters","AllSpecies","VariableSpecies", "AllObservers","TimeSchema","SpeciesTimeSchema", "AllTableParameters", "VariableTableParameters"))
  checkEquals(names(dci_info$ReferenceTab$AllParameters),c("ID","Path","Value","Unit","ParameterType","Formula","Description", "UsedInSimulation"))
  checkEquals(names(attributes(dci_info$ReferenceTab$AllParameters$ID)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$AllParameters$Path)), c("Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$AllParameters$Value)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$AllParameters$Unit)), c("Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$AllParameters$ParameterType)), c("Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$AllParameters$Formula)), c("Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$AllParameters$Description)), c("Name","Description"))
  checkEquals(names(dci_info$ReferenceTab$VariableParameters),c("ID","Path","Value","Unit","ParameterType","Formula","Description", "UsedInSimulation"))
  checkEquals(names(attributes(dci_info$ReferenceTab$VariableParameters$ID)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$VariableParameters$Path)), c("Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$VariableParameters$Value)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$VariableParameters$Unit)), c("Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$VariableParameters$ParameterType)), c("Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$VariableParameters$Formula)), c("Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$VariableParameters$Description)), c("Name","Description"))
  checkEquals(names(dci_info$ReferenceTab$AllSpecies),c("ID","Path","InitialValue", "ScaleFactor","Unit","IsFormula","Formula","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$AllSpecies$ID)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$AllSpecies$Path)), c("Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$AllSpecies$InitialValue)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$AllSpecies$ScaleFactor)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$AllSpecies$Unit)), c("Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$AllSpecies$IsFormula)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$AllSpecies$Formula)), c("Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$AllSpecies$Description)), c("Name","Description"))
  checkEquals(names(dci_info$ReferenceTab$VariableSpecies),c("ID","Path","InitialValue", "ScaleFactor","Unit","IsFormula","Formula","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$VariableSpecies$ID)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$VariableSpecies$Path)), c("Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$VariableSpecies$InitialValue)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$VariableSpecies$ScaleFactor)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$VariableSpecies$Unit)), c("Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$VariableSpecies$IsFormula)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$VariableSpecies$Formula)), c("Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$VariableSpecies$Description)), c("Name","Description"))
  checkEquals(names(dci_info$ReferenceTab$AllObservers),c("ID","Path","Unit","Formula","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$AllObservers$ID)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$AllObservers$Path)), c("Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$AllObservers$Unit)), c("Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$AllObservers$Formula)), c("Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$AllObservers$Description)), c("Name","Description"))
  checkEquals(names(dci_info$ReferenceTab$TimeSchema),c("StartTime","EndTime","Unit","NoOfTimePoints","Distribution"))
  checkEquals(names(attributes(dci_info$ReferenceTab$TimeSchema$StartTime)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$TimeSchema$EndTime)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$TimeSchema$Unit)), c("Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$TimeSchema$NoOfTimePoints)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$TimeSchema$Distribution)), c("Name","Description"))
  checkEquals(names(dci_info$ReferenceTab$SpeciesTimeSchema),c("ID","Path","StartTime","EndTime","Unit","NoOfTimePoints","Distribution"))
  checkEquals(names(attributes(dci_info$ReferenceTab$SpeciesTimeSchema$StartTime)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$SpeciesTimeSchema$EndTime)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$SpeciesTimeSchema$Unit)), c("Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$SpeciesTimeSchema$NoOfTimePoints)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$SpeciesTimeSchema$Distribution)), c("Name","Description"))
  checkEquals(names(dci_info$ReferenceTab$AllTableParameters),c("ID", "Time", "Value", "RestartSolver"))
  checkEquals(names(attributes(dci_info$ReferenceTab$AllTableParameters$ID)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$AllTableParameters$Time)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$AllTableParameters$Value)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$AllTableParameters$RestartSolver)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(dci_info$ReferenceTab$VariableTableParameters),c("ID", "Time", "Value", "RestartSolver"))
  checkEquals(names(attributes(dci_info$ReferenceTab$VariableTableParameters$ID)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$VariableTableParameters$Time)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$VariableTableParameters$Value)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dci_info$ReferenceTab$VariableTableParameters$RestartSolver)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  
}