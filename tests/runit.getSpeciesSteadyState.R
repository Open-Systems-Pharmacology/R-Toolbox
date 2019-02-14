#require(RUnit, quietly=TRUE)
#require(MoBiToolboxForR, quietly=TRUE)
simModelXML <- "./tests/models/black american girl.xml"
standard_dci_info <- initSimulation(XML=simModelXML, whichInitParam="none");

test.EmptyDCI_Info <- function() {
  checkException(getSpeciesSteadyState(DCI_Info = {}))
}

#Check behavior when no species provided. ATM, steady-state for all species should be provided.
#However, the correct behavior would be only to return values of non-formula species.
test.emptySpecies = function(){
  allSpecies = existsSpeciesInitialValue(path_id = "*", DCI_Info = standard_dci_info);
  steadyState = getSpeciesSteadyState(DCI_Info = standard_dci_info, steadyStateTime = 1000);
  
  #The test is for the lenght of returned path - it should be identical to  the number of all species.
  checkEquals(length(allSpecies$Path), length(steadyState$moleculesSteadyState$`Container Path`) + length(steadyState$parametersSteadyState$`Container Path`));
}

test.speciesExistent = function(){
  steadyState = getSpeciesSteadyState(speciesNames = c("my Compound"), DCI_Info = standard_dci_info, steadyStateTime = 1000);
  #The test is for the number of returned columns - there should be 7 for "Container Path", "Molecule Name", "Is Present", "Value", "Scale Divisor", and "Neg. Values Allowed"
  checkTrue(length(steadyState$moleculesSteadyState) == 7);
}

#If no provided species found in the simulation, an emtpy data frame is returned
test.speciesNonExistent = function(){
  steadyState = getSpeciesSteadyState(speciesNames = c("doesNotExist"), DCI_Info = standard_dci_info, steadyStateTime = 1000);
  #The test is for the number of returned columns - there should be none as no species is found.
  checkEquals(length(steadyState$moleculesSteadyState), 0);
}

test.speciesNonExistentExistent = function(){
  steadyState = getSpeciesSteadyState(speciesNames = c("doesNotExist", "my Compound"), DCI_Info = standard_dci_info, steadyStateTime = 1000);
  #The test is for the number of returned columns - there should be 7 for "Container Path", "Molecule Name", "Is Present", "Value", "Scale Divisor", and "Neg. Values Allowed"
  checkTrue(length(steadyState$moleculesSteadyState) == 7);
}

test.ignoreIfFormulaTrue = function(){
  #Formula flag should not affect non-formula species
  steadyState = getSpeciesSteadyState(speciesNames = c("my Compound"), ignoreIfFormula = TRUE, DCI_Info = standard_dci_info, steadyStateTime = 1000);
  #The test is for the number of returned columns - there should be 7 for "Container Path", "Molecule Name", "Is Present", "Value", "Scale Divisor", and "Neg. Values Allowed"
  checkTrue(length(steadyState$moleculesSteadyState) == 7);
  
  #Species with initial values defined by formula should be ignored
  steadyState = getSpeciesSteadyState(speciesNames = c("Liquid"), ignoreIfFormula = TRUE, DCI_Info = standard_dci_info, steadyStateTime = 1000);
  #The test is for the number of returned columns - there should be none as no species is found.
  checkEquals(length(steadyState$parametersSteadyState), 0);
}

test.ignoreIfFormulaFalse = function(){
  #Formula flag should not affect non-formula species
  steadyState = getSpeciesSteadyState(speciesNames = c("my Compound"), ignoreIfFormula = FALSE, DCI_Info = standard_dci_info, steadyStateTime = 1000);
  #The test is for the number of returned columns - there should be 7 for "Container Path", "Molecule Name", "Is Present", "Value", "Scale Divisor", and "Neg. Values Allowed"
  checkTrue(length(steadyState$moleculesSteadyState) == 7);
  
  #Species with initial values defined by formula should be considered
  steadyState = getSpeciesSteadyState(speciesNames = c("Liquid"), ignoreIfFormula = FALSE, DCI_Info = standard_dci_info, steadyStateTime = 1000);
  #The test is for the number of returned columns - there should be 4 for "Container Path", "Parameter Name", "Value", and "Units"
  checkEquals(length(steadyState$parametersSteadyState), 4);
}
