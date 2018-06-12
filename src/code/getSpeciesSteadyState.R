
#' Get the steady-state values of species. The steady-state is considered to be the last value of the simulation with sufficiently
#' long simulation time, i.e., where the rates of the processes do not change.
#'
#' @param speciesNames Vector of species names which steady-state values will be returned. By default, values of all species are returned. This also includes
#' values of "state variable" parameters, as these are treated like species.
#' @param steadyStateTime Simulation time (minites). Must be long enough for system to reach a steady-state. Default is 1000 minutes.
#' @param ignoreIfFormula If "TRUE", species with initial values defined by formula are not included. Default is "TRUE".
#' @param DCI_Info The DCI Info structure containing the DCI handle and all settings.
#'
#' @return A data frame with columns  "Container Path", "Molecule Name", "Is Present", "Value", "Units", "Scale Divisor", "Neg. Values Allowed".
#' The Container Path does not include the name of the simulation.
#' If not entries matching the criteria (speciesNames and ignoreIfFormula) could be generated, an emtpy data frame is returned (length = zero).
#' @export
#'
#' @examples
getSpeciesSteadyState = function(speciesNames = c("*"), steadyStateTime = 1000, ignoreIfFormula = TRUE, DCI_Info = {}){
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

  DCI_Info = processSimulation(DCI_Info = DCI_Info);

  #Create a list of the end values of the steady-state simulation.
  containerPath = c();
  moleculeName = c();
  isPresent = c();
  value = c();
  units = c();
  scaleDivisor = c();
  negVals = c();
  
  #Get a vector of IDs of all species for which the steady-state values will be returned.
  allIDs = c();
  for (name in speciesNames){
    allIDs = c(allIDs, existsSpeciesInitialValue(path_id = paste("*", name, sep = "|"), options = list(Type = "readOnly"), DCI_Info = DCI_Info)$ID);
  }
  allIDs = unique(allIDs);
  
  #Iterate through all species.
  for (ID in allIDs){
    isFormula = getSpeciesInitialValue(path_id = ID, list(Type = "readonly", Property = "IsFormula"), DCI_Info=DCI_Info)$Value;
    #Add the species to the output list if it initial value is not defined by a formula OR formula defined species should not be ignored
    if (!isFormula | !ignoreIfFormula){
      #Split the path of the name into container path and species name.
      fullPathParts = strsplit(existsSpeciesInitialValue(path_id = ID, options = list(Type = "readOnly"), DCI_Info = DCI_Info)$Path, "|", fixed = TRUE);
      containerPath_curr = fullPathParts[[1]][2];
      for (i in 3 : length(fullPathParts[[1]]) - 1){
        containerPath_curr = paste(containerPath_curr, fullPathParts[[1]][i], sep = "|");
      }
      moleculeName_curr = fullPathParts[[1]][length(fullPathParts[[1]])];
      
      containerPath = c(containerPath, containerPath_curr);
      moleculeName = c(moleculeName, moleculeName_curr);
      isPresent = c(isPresent, TRUE);
      value = c(value, getEndSimulationResult(path_id = ID, DCI_Info = DCI_Info)$Value);
      units = c(units, getSpeciesInitialValue(path_id = ID, list(Type = "readonly", Property = "Unit"), DCI_Info=DCI_Info)$Value);
      scaleDivisor = c(scaleDivisor, getSpeciesInitialValue(path_id = ID, list(Type = "readonly", Property = "ScaleFactor"), DCI_Info=DCI_Info)$Value);
      negVals = c(negVals, "");
    }
  }
  
  speciesInitVals = data.frame(containerPath, moleculeName, isPresent, value, units, scaleDivisor, negVals);
  if (length(speciesInitVals) > 0){
    colnames(speciesInitVals) = c("Container Path", "Molecule Name", "Is Present", "Value", "Units", "Scale Divisor", "Neg. Values Allowed");
  }
  
  return(speciesInitVals);
}