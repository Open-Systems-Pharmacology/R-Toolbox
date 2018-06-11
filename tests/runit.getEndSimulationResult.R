#require(RUnit, quietly=TRUE)
#require(MoBiToolboxForR, quietly=TRUE)
simModelXML <- "./tests/models/black american girl.xml"

obsPath1 = "black american girl|Organism|PeripheralVenousBlood|MoleculeProperties|my Compound|OBSBloodCells";
obsID1 = 97;
obsPath2 = "black american girl|Organism|PeripheralVenousBlood|MoleculeProperties|my Compound|OBSPlasma";
obsID2 = 109;

speciesPath1 = "black american girl|Organism|VenousBlood|Plasma|my Compund";
speciesID1 = 134;
speciesPath2 = "black american girl|Organism|VenousBlood|Plasma|CYP3A4";
speciesID2 = 141;

test.EmptyDCI_Info <- function() {
  checkException(getSimulationResult(DCI_Info = {}))
}

test.NotSimulated <- function() {
  standard_dci_info <- initSimulation(XML=simModelXML, whichInitParam="none")
  dci_info <- standard_dci_info
  checkException(getSimulationResult(DCI_Info=dci_info))
}

test.byPathSpecies = function(){
  dci_info = processSimulation(DCI_Info = dci_info);
  #existent
  getEndSimulationResult(path_id = speciesPath1, DCI_Info = dci_info);
  #existent multiple
  #non-existent
  #non-existent multiple
}

test.byIDSpecies = function(){
  #existent
  #existent multiple
  #non-existent
  #non-existent multiple
}

test.byIndexSpecies = function(){
  #existent
  #existent multiple
  #non-existent
  #non-existent multiple
}

test.byPathObserver = function(){
  #existent
  endResult = getEndSimulationResult(path_id = obsPath1, DCI_Info = dci_info);
  result = getSimulationResult(path_id = obsPath1, DCI_Info = dci_info);
  checkEquals(endResult$Path, colnames(result)[2]);
  checkEquals(endResult$Value, result[length(result)]);
  #non-existent
  checkException(endResult <- getEndSimulationResult(path_id = "obsPath1", DCI_Info = dci_info));
}

test.byIDObserver = function(){
  #existent
  endResult = getEndSimulationResult(path_id = obsID1, DCI_Info = dci_info);
  result = getSimulationResult(path_id = obsID1, DCI_Info = dci_info);
  checkEquals(endResult$Path, colnames(result)[2]);
  checkEquals(endResult$Value, result[,2][length(result[,2])]);
  #existent multiple
  endResult = getEndSimulationResult(path_id = c(obsID1, obsID2), DCI_Info = dci_info);
  result = getSimulationResult(path_id = c(obsID1, obsID2), DCI_Info = dci_info);
  checkEquals(endResult$Path, c(colnames(result)[2], colnames(result)[3]));
  checkEquals(endResult$Value, c(result[,2][length(result[,2])], result[,3][length(result[,3])]));
  #non-existent
  checkException(endResult <- getEndSimulationResult(path_id = 0, DCI_Info = dci_info));
  #non-existent and existent mixed
  endResult <- getEndSimulationResult(path_id = c(0, obsID1), DCI_Info = dci_info)
  result = getSimulationResult(path_id = obsID1, DCI_Info = dci_info);
  checkEquals(endResult$Path, colnames(result)[2]);
  checkEquals(endResult$Value, result[,2][length(result[,2])]);
}

test.byIndexObserver = function(){
  obsIdx1 = existsObserver(path_id = obsPath2, DCI_Info = dci_info);
  #existent
  #existent multiple
  #non-existent
  #non-existent multiple
}