#require(RUnit, quietly=TRUE)
#require(MoBiToolboxForR, quietly=TRUE)
simModelXML <- "./tests/models/black american girl.xml"

test.EmptyDCI_Info <- function() {
  checkException(saveSimulationToXML(XML=file.path(getwd(), "Test.xml"), DCI_Info = {}))
}

test.CreationWithoutXMLParameter <- function() {
  dci_info <- initSimulation(XML=simModelXML, whichInitParam="none")
  file <- saveSimulationToXML(DCI_Info = dci_info)
  checkTrue(file.exists(file))
  checkEquals(file, file.path(getwd(), "CurrentSimulation.xml"))
  file.remove(file)
}

test.OverwrittingExistingFile <- function() {
  dci_info <- initSimulation(XML=simModelXML, whichInitParam="none")
  path <- file.path(getwd(), "CurrentSimulation.xml")
  
  file <- saveSimulationToXML(DCI_Info = dci_info)
  checkTrue(file.exists(file))
  checkEquals(file, path)
  options(warn = 2);
  checkException(file <- saveSimulationToXML(DCI_Info = dci_info))
  options(warn = 0);
  file <- saveSimulationToXML(DCI_Info = dci_info)
  
  checkTrue(file.exists(file))
  checkEquals(file, path)
  file.remove(file)
}

test.CreateNonExistingFolder <- function() {
  dci_info <- initSimulation(XML=simModelXML, whichInitParam="none")
  path <- file.path(getwd(), "Willi", "TestSim.xml")
  checkTrue(!file.exists(dirname(path)))
  options(warn = 2);
  checkException(file <- saveSimulationToXML(XML=path, DCI_Info = dci_info))
  options(warn = 0);
  
  file <- saveSimulationToXML(XML=path, DCI_Info = dci_info)
  checkTrue(file.exists(file))
  checkEquals(file, path)

  file.remove(file)
  unlink(dirname(file), recursive=TRUE)
}

test.CheckXMLNoneVariables <- function () {
  dci_info <- initSimulation(XML=simModelXML, whichInitParam="none")
  path <- file.path(getwd(), "TestSim.xml")
 
  savedFile <- saveSimulationToXML(XML=path, DCI_Info = dci_info)
  savedDCI <- initSimulation(XML=savedFile, whichInitParam="none")
  
  idx <- which(names(dci_info) != "Handle")
  checkEquals(dci_info[idx], savedDCI[idx])
  
  file.remove(savedFile)
}

test.CheckXMLAllNonFormulaVariables <- function () {
  dci_info <- initSimulation(XML=simModelXML, whichInitParam="allnonFormula")
  path <- file.path(getwd(), "TestSim.xml")
  
  savedFile <- saveSimulationToXML(XML=path, DCI_Info = dci_info);
  savedDCI <- initSimulation(XML=savedFile, whichInitParam="allnonFormula");

  idx <- which(names(dci_info) != "Handle");
  checkEquals(dci_info[idx], savedDCI[idx]);
  
  file.remove(savedFile)
}

test.CheckXMLAllVariables <- function () {
  dci_info <- initSimulation(XML=simModelXML, whichInitParam="all")
  path <- file.path(getwd(), "TestSim.xml")
  
  savedFile <- saveSimulationToXML(XML=path, DCI_Info = dci_info)
  #Must process simulations before comparison - otherwise the formulas in the first simulation (dci_info) are not overwritten
  dci_info = processSimulation(DCI_Info = dci_info);
  
  savedDCI <- initSimulation(XML=savedFile, whichInitParam="all")
  
  idx <- which(names(dci_info) != "Handle")
  
  savedDCI = processSimulation(DCI_Info = savedDCI);
  
  checkEquals(dci_info[idx], savedDCI[idx])
  
  file.remove(savedFile);
}