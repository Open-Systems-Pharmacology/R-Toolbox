#require(RUnit, quietly=TRUE)
#require(MoBiToolboxForR, quietly=TRUE)
simModelXML <- "./tests/models/black american girl.xml"

#Test for empty DCI_Info
test.EmptyDCI_Info <- function() {
  checkException(getSimulationResult(DCI_Info = {}))
}

#Test for invalid handle
test.handleInvalid = function(){
  DCI_Info = list();
  DCI_Info$Handle = 0;
  warning = getSolverWarnings(DCI_Info);
  warningToExpect = paste("Component handle", DCI_Info$Handle, "is invalid!")
  checkEquals(warning, warningToExpect);
}

test.noWarnings <- function() {
  dci_info <- initSimulation(XML=simModelXML, whichInitParam="all");
  warning = getSolverWarnings(DCI_Info = dci_info);
  checkTrue(length(warning) == 0);
  
  dci_info = processSimulation(DCI_Info = dci_info);
  warning = getSolverWarnings(DCI_Info = dci_info);
  checkTrue(length(warning) == 0);
}

test.Warnings <- function() {
  dci_info <- initSimulation(XML=simModelXML, whichInitParam="all", further_options = list(STOPONWARNINGS = FALSE))
  dci_info = setParameter(9, "*|MxStep", DCI_Info = dci_info);
  
  #have to try-catch the processSimulation, as STOPONWARNINGS = FALSE seems not to work.
  tryCatch(
    {dci_info = processSimulation(DCI_Info = dci_info)},
    error = function(err){
    }
  )
  warning = getSolverWarnings(DCI_Info = dci_info);
  checkTrue(is.character(warning) & length(warning) > 0);
}