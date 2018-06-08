#ScaleFactors_1 has all scale factors set to 1 and fails with AbsTol = 1e-10 but runs with AbsTol = 1e-15
#ScaleFactors_opt has scale factors as calculated by MoBi and runs with AbsTol = 1e-10

xmlSF_1 = "./tests/models/ScaleFactors_1.xml";
xmlSF_opt = "./tests/models/ScaleFactors_opt.xml";

DCI_Info_1 = initSimulation(xmlSF_1, whichInitParam = "none");
DCI_Info_1 = processSimulation(DCI_Info_1);
results_1 = getSimulationResult(path_id = "*|Organism|a", DCI_Info = DCI_Info_1);

#Change all scale factors of ScaleFactors_opt to 1 and run the simulation. If the scale factors are set properly,
#the simulation should fail.
test.CheckRunSimulationWithStandardAbsTol <- function() {
  DCI_Info_opt = initSimulation(xmlSF_opt, whichInitParam = "none");
  
  #Change scaling factors of ScaleFactors_opt to 1
  #First, initialize all species.
  allSpecies = getSpeciesInitialValue(path_id ="*", list(Type = "readonly"), DCI_Info = DCI_Info_opt);
  initStruct <- list()
  for (idx in allSpecies$Index){
    isFormula = getSpeciesInitialValue(path_id ="*", list(Type = "readonly", Property = "IsFormula", Index = idx), DCI_Info=DCI_Info_opt)$Value;
    if (!isFormula){
      path_id = getSpeciesInitialValue(path_id ="*", list(Type = "readonly", Property = "Path", Index = idx), DCI_Info=DCI_Info_opt)$Value;
      initStruct <- initSpeciesInitialValue(initStruct, path_id, initializeIfFormula = "withWarning")
    }
  }

  DCI_Info_opt <- initSimulation(xmlSF_opt, ParamList = initStruct)
  
  #Set scaling factors to 1
  for (idx in allSpecies$Index){
    DCI_Info_opt = setSpeciesInitialValue(1, options = list(Property = "ScaleFactor", Index = idx), DCI_Info=DCI_Info_opt)
  }
  
  #Simulation should fail. If it does not, scale factors were not set!
  checkException(processSimulation(DCI_Info_opt));
}

#Change all scale factors of ScaleFactors_opt to 1 and run the simulation with reduced AbsTol. The simulation should run and
#the results must be equal to the results of ScaleFactors_1.
test.CheckRunSimulationWithReducedAbsTol <- function() {
  DCI_Info_opt = initSimulation(xmlSF_opt, whichInitParam = "none");
  
  #Change scaling factors of ScaleFactors_opt to 1
  #First, initialize all species.
  allSpecies = getSpeciesInitialValue(path_id ="*", list(Type = "readonly"), DCI_Info = DCI_Info_opt);
  initStruct <- list()
  initStruct = initParameter(path_id = "AbsTol", initStruct = initStruct, initializeIfFormula = "always");
  for (idx in allSpecies$Index){
    isFormula = getSpeciesInitialValue(path_id ="*", list(Type = "readonly", Property = "IsFormula", Index = idx), DCI_Info=DCI_Info_opt)$Value;
    if (!isFormula){
      path_id = getSpeciesInitialValue(path_id ="*", list(Type = "readonly", Property = "Path", Index = idx), DCI_Info=DCI_Info_opt)$Value;
      initStruct <- initSpeciesInitialValue(initStruct, path_id, initializeIfFormula = "withWarning")
    }
  }
  
  DCI_Info_opt <- initSimulation(xmlSF_opt, ParamList = initStruct)
  
  #Set scaling factors to 1
  for (idx in allSpecies$Index){
    DCI_Info_opt = setSpeciesInitialValue(1, options = list(Property = "ScaleFactor", Index = idx), DCI_Info=DCI_Info_opt)
  }
  
  #Reduce AbsTol to enable simulation run.
  DCI_Info_opt = setParameter(1e-15, path_id = "AbsTol", DCI_Info = DCI_Info_opt);
  DCI_Info_opt = processSimulation(DCI_Info_opt)
  results_opt = getSimulationResult(path_id = "*|Organism|a", DCI_Info = DCI_Info_opt);
  
  #Check if results are correct
  checkEquals(results_1[,2], results_opt[,2])
}