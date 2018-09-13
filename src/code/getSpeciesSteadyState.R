#' Get the steady-state values of species. The steady-state is considered to be the last value of the simulation with sufficiently
#' long simulation time, i.e., where the rates of the processes do not (significantly) change.
#'
#' @param speciesNames Vector of species names which steady-state values will be returned. By default, values of all species are returned. This also includes
#' values of "state variable" parameters, as these are treated like species.
#' @param steadyStateTime Simulation time (minites). Must be long enough for system to reach a steady-state.
#' @param ignoreIfFormula If "TRUE", species with initial values defined by formula are not included. Default is "TRUE".
#' @param DCI_Info The DCI Info structure containing the DCI handle and all settings.
#'
#' @return A list containing two data frames - moleculesSteadyState and parametersSteadyState. The data frame moleculesSteadyState contains columns
#' "Container Path", "Molecule Name", "Is Present", "Value", "Units", "Scale Divisor", "Neg. Values Allowed".
#' The data frame parametersSteadyState. contains columns
#' "Container Path", "Parameter Name", "Value", "Units".
#' All species with the unit "µmol" that are located within the "Organism"-structure are treated as molecules and stored in the moleculesSteadyState-data frame.
#' That means that local state variable parameters with the dimension "Amount" are treated as molecules.
#' All species with units other then "µmol" OR the top container being not "Organism" are stored in the parametersSteadyState-data frame.
#' Example: the local state variable parameter "Organism|Fat|Intracellular|myReaction|ParamName" will be stored in the moleculesSteadyState-data frame.
#' The global parameter "myReaction|ParamName" will be stored in the parametersSteadyState-data frame.
#' The Container Path does not include the name of the simulation.
#' If not entries matching the criteria (speciesNames and ignoreIfFormula) could be generated, an emtpy data frame is returned (length = zero).
#' @export
#'
#' @examples
#'   #Initialize the given simulation with no variable parameters
#' myDCI = initSimulation(modelPath, whichInitParam = "none");
#' Get species steady-state
#' initialValues = getSpeciesSteadyState(DCI_Info = myDCI, ignoreIfFormula = FALSE, steadyStateTime = 1000);
#' Write the results into an excel file. (example with package "openxlsx"
#' write.xlsx(list("Molecules" = (initialValues$moleculesSteadyState), "Parameters" = initialValues$parametersSteadyState), resultsXLS, colNames = TRUE)
getSpeciesSteadyState = function(speciesNames = c("*"), steadyStateTime, ignoreIfFormula = TRUE, DCI_Info = {}){
  #Perform initial input check
  if (length(DCI_Info) == 0)
  {
    stop("No DCI_Info provided.")
  }
  
  if (steadyStateTime <= 0){
    stop("steadyStateTime must be >0!");
  }
  
  #Set simulation time to the steady-state value.
  DCI_Info = setSimulationTime(timepoints = seq(0, steadyStateTime, length.out = 2), DCI_Info = DCI_Info);
  #Simulate.
  DCI_Info = processSimulation(DCI_Info = DCI_Info);

  #For each simulated species, the output contains the path, species name, the "isPresetn"-flag, the value, the unit, the scale divisor value, and the "negative values allowed"-flag.
  moleculeContainerPath = c();
  moleculeName = c();
  moleculeIsPresent = c();
  moleculeValue = c();
  moleculeUnits = c();
  moleculeScaleDivisor = c();
  moleculeNegValsAllowed = c();
  
  #Some parameters are treated as species in the simulation. The "steady-state"-value must be returned, too.
  parameterContainerPath = c();
  parameterName = c();
  parameterValue = c();
  parameterUnits = c();
  
  #Get a vector of IDs of all species for which the steady-state values will be returned.
  allIDs = c();
  for (name in speciesNames){
    allIDs = c(allIDs, existsSpeciesInitialValue(path_id = paste("*", name, sep = "|"), options = list(Type = "readOnly"), DCI_Info = DCI_Info)$ID);
  }
  allIDs = unique(allIDs);
  
  #Iterate through all species.
  for (ID in allIDs){
    isFormula = getSpeciesInitialValue(path_id = ID, list(Type = "readonly", Property = "IsFormula"), DCI_Info=DCI_Info)$Value;
    unit = getSpeciesInitialValue(path_id = ID, list(Type = "readonly", Property = "Unit"), DCI_Info=DCI_Info)$Value;
    
    #Add the species to the output list if it initial value is not defined by a formula OR formula defined species should not be ignored
    if (!isFormula || !ignoreIfFormula){
      #Split the path of the name into container path and species name.
      fullPathParts = strsplit(existsSpeciesInitialValue(path_id = ID, options = list(Type = "readOnly"), DCI_Info = DCI_Info)$Path, "|", fixed = TRUE);
      #Get the top container name
      topContainer = fullPathParts[[1]][2];
      containerPath_curr = topContainer;
      #If the path has more than 3 entries, append them to containerPath_curr. First entry is simulation name, second the topContainer, and last ist the name of the parameter/species.
      if (length(fullPathParts[[1]]) > 3){
        for (i in 3 : (length(fullPathParts[[1]]) - 1)){
          containerPath_curr = paste(containerPath_curr, fullPathParts[[1]][i], sep = "|");
        }
      }
      entryName = fullPathParts[[1]][length(fullPathParts[[1]])];
      
      #If the unit is "µmol" and the top container is "Organism", the current entry is a molecule
      if  (unit == "µmol" && topContainer == "Organism"){
        moleculeContainerPath = c(moleculeContainerPath, containerPath_curr);
        moleculeName = c(moleculeName, entryName);
        moleculeIsPresent = c(moleculeIsPresent, TRUE);
        moleculeValue = c(moleculeValue, getEndSimulationResult(path_id = ID, DCI_Info = DCI_Info)$Value);
        moleculeUnits = c(moleculeUnits, unit);
        moleculeScaleDivisor = c(moleculeScaleDivisor, getSpeciesInitialValue(path_id = ID, list(Type = "readonly", Property = "ScaleFactor"), DCI_Info=DCI_Info)$Value);
        moleculeNegValsAllowed = c(moleculeNegValsAllowed, "");
      }
      #Otherwise, it is a parameter.
      else{
        parameterContainerPath = c(parameterContainerPath, containerPath_curr);
        parameterName = c(parameterName, entryName);
        parameterValue = c(parameterValue, getEndSimulationResult(path_id = ID, DCI_Info = DCI_Info)$Value);
        parameterUnits = c(parameterUnits, unit);
      }
    }
  }
  
  speciesInitVals = data.frame(moleculeContainerPath, moleculeName, moleculeIsPresent, moleculeValue, moleculeUnits, moleculeScaleDivisor, moleculeNegValsAllowed);
  if (length(speciesInitVals) > 0){
    colnames(speciesInitVals) = c("Container Path", "Molecule Name", "Is Present", "Value", "Units", "Scale Divisor", "Neg. Values Allowed");
  }
  
  parameterInitVals = data.frame(parameterContainerPath, parameterName, parameterValue, parameterUnits);
  if (length(parameterInitVals) > 0){
    colnames(parameterInitVals) = c("Container Path", "Parameter Name", "Value", "Units");
  }
  
  return(list(moleculesSteadyState = speciesInitVals, parametersSteadyState = parameterInitVals));
}