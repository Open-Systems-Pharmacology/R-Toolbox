#require(RUnit, quietly=TRUE)
#require(MoBiToolboxForR, quietly=TRUE)
simModelXML1 <- "./tests/models/black american girl.xml";
simModelXML2 = "./tests/models/default man.xml";
dci_info1 = initSimulation(simModelXML1, whichInitParam = "none");
dci_info2 = initSimulation(simModelXML1, whichInitParam = "none");
dci_info3 = initSimulation(simModelXML2, whichInitParam = "none");
resultsPath = file.path(getwd(), "CurrentComparison.csv");

#Check for both DCI_Info!
test.EmptyDCI_Info <- function() {
  #Empty DCI_Info1
  checkException(compareSimulations(DCI_Info2 = dci_info1));
  #Emtpy DCI_Info2
  checkException(compareSimulations(DCI_Info1 = dci_info1));
  #Both DCI_Info emtpy
  checkException(compareSimulations());
}

#Check for tolerance positive
test.numericalTolerance = function(){
  checkException(compareSimulations(DCI_Info1 = dci_info1, DCI_Info2 = dci_info1, numTolerance = -1));
}

test.OverwrittingExistingFile <- function() {
  diffs = compareSimulations(DCI_Info1 = dci_info1, DCI_Info2 = dci_info1, resultsCSV_path = resultsPath);
  checkTrue(file.exists(resultsPath));
  
  diffs = compareSimulations(DCI_Info1 = dci_info1, DCI_Info2 = dci_info1, resultsCSV_path = resultsPath);
  checkTrue(file.exists(resultsPath))
  file.remove(resultsPath)
}

test.CreateNonExistingFolder <- function() {
  path <- file.path(getwd(), "NewFolder", "CurrentComparison.csv")
  
  checkTrue(!file.exists(dirname(path)))
  diffs = compareSimulations(DCI_Info1 = dci_info1, DCI_Info2 = dci_info1, resultsCSV_path = path);
  checkTrue(file.exists(path))

  file.remove(path)
  unlink(dirname(path), recursive=TRUE)
}

#TO DO
test.resultsCSV_path_invalid = function(){
}

#non-boolean ingnoreFormula
test.ignoreFormulainvalid = function(){
  checkException(compareSimulations(DCI_Info1 = dci_info1, DCI_Info2 = dci_info1, ignoreFormula = "-1"));
}

#Same DCI - results identical
test.sameDCI = function(){
  diffs = compareSimulations(DCI_Info1 = dci_info1, DCI_Info2 = dci_info1);#, resultsCSV_path = resultsPath);
  checkEquals(diffs$SimNames[1], diffs$SimNames[2]);
  checkEquals(length(diffs$Params_diff), 0);
  checkEquals(length(diffs$Species_diff), 0);
  checkEquals(length(diffs$Observer_diff), 0);
  checkEquals(diffs$TimeIdentical, TRUE);
}

#Different DCI, same simulation, ignoreFormula = TRUE, results should be identical
test.simulationsIdentical_ignoreFormula = function(){
  diffs = compareSimulations(DCI_Info1 = dci_info1, DCI_Info2 = dci_info2, ignoreFormula = TRUE);#, resultsCSV_path = resultsPath);
  checkEquals(diffs$SimNames[1], diffs$SimNames[2]);
  checkEquals(length(diffs$Params_diff), 0);
  checkEquals(length(diffs$Species_diff), 0);
  checkEquals(length(diffs$Observer_diff), 0);
  checkEquals(diffs$TimeIdentical, TRUE);
}

#Different DCI, same simulation, ignoreFormula = FALSE, results should be identical
test.simulationsIdentical_notIgnoreFormula = function(){
  diffs = compareSimulations(DCI_Info1 = dci_info1, DCI_Info2 = dci_info2, ignoreFormula = FALSE);#, resultsCSV_path = resultsPath);
  checkEquals(diffs$SimNames[1], diffs$SimNames[2]);
  checkEquals(length(diffs$Params_diff), 0);
  checkEquals(length(diffs$Species_diff), 0);
  checkEquals(length(diffs$Observer_diff), 0);
  checkEquals(diffs$TimeIdentical, TRUE);
}

#Different DCI, same simulation, but one simulation with initialized formulas, ignoreFormula = FALSE, results should not be identical
test.simulationsIdentical_InitAll_notIgnoreFormula = function(){
  dci_info2_diff = initSimulation(simModelXML1, whichInitParam = "all");
  diffs = compareSimulations(DCI_Info1 = dci_info1, DCI_Info2 = dci_info2_diff, ignoreFormula = FALSE, resultsCSV_path = resultsPath);
  checkEquals(diffs$SimNames[1], diffs$SimNames[2]);
  checkEquals(length(diffs$Params_diff), 9);
  checkEquals(length(diffs$Species_diff), 9);
  checkEquals(length(diffs$Observer_diff), 9);
  checkEquals(diffs$TimeIdentical, TRUE);
}

#Different DCI, same simulation, but one simulation with initialized formulas, ignoreFormula = TRUE, results should be identical
test.simulationsIdentical_InitAll_ignoreFormula = function(){
  dci_info2_diff = initSimulation(simModelXML1, whichInitParam = "all");
  diffs = compareSimulations(DCI_Info1 = dci_info1, DCI_Info2 = dci_info2_diff, ignoreFormula = TRUE);#, resultsCSV_path = resultsPath);
  checkEquals(diffs$SimNames[1], diffs$SimNames[2]);
  checkEquals(length(diffs$Params_diff), 0);
  checkEquals(length(diffs$Species_diff), 0);
  checkEquals(length(diffs$Observer_diff), 0);
  checkEquals(diffs$TimeIdentical, TRUE);
}

#Different DCI, same simulation, same parameter initialized, same value set, results should be identical
test.simulationsIdentical_InitParam_sameValue = function(){
  paramPath = "*|Organism|Weight";
  initStruct = list();
  initStruct = initParameter(initStruct = initStruct, path_id = paramPath, initializeIfFormula = "always");
  dci_info1_diff = initSimulation(simModelXML1, ParamList = initStruct);
  dci_info2_diff = initSimulation(simModelXML1, ParamList = initStruct);
  
  dci_info1_diff = setParameter(80, paramPath, DCI_Info = dci_info1_diff);
  dci_info2_diff = setParameter(80, paramPath, DCI_Info = dci_info2_diff);
  
  diffs = compareSimulations(DCI_Info1 = dci_info1_diff, DCI_Info2 = dci_info2_diff, ignoreFormula = TRUE);#, resultsCSV_path = resultsPath);
  checkEquals(diffs$SimNames[1], diffs$SimNames[2]);
  checkEquals(length(diffs$Params_diff), 0);
  checkEquals(length(diffs$Species_diff), 0);
  checkEquals(length(diffs$Observer_diff), 0);
  checkEquals(diffs$TimeIdentical, TRUE);
  
  diffs = compareSimulations(DCI_Info1 = dci_info1_diff, DCI_Info2 = dci_info2_diff, ignoreFormula = FALSE);#, resultsCSV_path = resultsPath);
  checkEquals(diffs$SimNames[1], diffs$SimNames[2]);
  checkEquals(length(diffs$Params_diff), 0);
  checkEquals(length(diffs$Species_diff), 0);
  checkEquals(length(diffs$Observer_diff), 0);
  checkEquals(diffs$TimeIdentical, TRUE);
}

#Different DCI, same simulation, same parameter initialized, different values set, results should not be identical
test.simulationsIdentical_InitParam_diffValue = function(){
  paramPath = "*|Organism|Weight";
  initStruct = list();
  initStruct = initParameter(initStruct = initStruct, path_id = paramPath, initializeIfFormula = "always");
  dci_info1_diff = initSimulation(simModelXML1, ParamList = initStruct);
  dci_info2_diff = initSimulation(simModelXML1, ParamList = initStruct);
  
  dci_info1_diff = setParameter(80, paramPath, DCI_Info = dci_info1_diff);
  dci_info2_diff = setParameter(70, paramPath, DCI_Info = dci_info2_diff);
  
  diffs = compareSimulations(DCI_Info1 = dci_info1_diff, DCI_Info2 = dci_info2_diff, ignoreFormula = TRUE);#, resultsCSV_path = resultsPath);
  checkEquals(diffs$SimNames[1], diffs$SimNames[2]);
  checkEquals(length(diffs$Params_diff), 9);
  checkEquals(length(diffs$Species_diff), 0);
  checkEquals(length(diffs$Observer_diff), 0);
  checkEquals(diffs$TimeIdentical, TRUE);
  
  diffs = compareSimulations(DCI_Info1 = dci_info1_diff, DCI_Info2 = dci_info2_diff, ignoreFormula = FALSE);#, resultsCSV_path = resultsPath);
  checkEquals(diffs$SimNames[1], diffs$SimNames[2]);
  checkEquals(length(diffs$Params_diff), 9);
  checkEquals(length(diffs$Species_diff), 0);
  checkEquals(length(diffs$Observer_diff), 0);
  checkEquals(diffs$TimeIdentical, TRUE);
}

test.timePaternChanged = function(){
  current_dci_info1 = dci_info1;
  current_dci_info1 = setSimulationTime(timepoints = c(0, 10), DCI_Info = current_dci_info1);
  diffs = compareSimulations(DCI_Info1 = current_dci_info1, DCI_Info2 = dci_info2, ignoreFormula = FALSE);#, resultsCSV_path = resultsPath);
  checkEquals(diffs$SimNames[1], diffs$SimNames[2]);
  checkEquals(length(diffs$Params_diff), 0);
  checkEquals(length(diffs$Species_diff), 0);
  checkEquals(length(diffs$Observer_diff), 0);
  checkEquals(diffs$TimeIdentical, FALSE);
}