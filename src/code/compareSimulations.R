#objectType can be "Parameter", "Species", or "Observer"
compareStructures = function(objectType, DCI_Info1, DCI_Info2, simName1, simName2, numTolerance, ignoreFormula){
  #This list holds information about the differences between simulations objects.
  objects_diff = list();
  
  #Get existence information about all objects depending on their type.
  if(objectType == "Parameter"){
    allObjects1 = existsParameter(path_id = "*", options = list(Type = "readonly"), DCI_Info = DCI_Info1);
    allObjects2 = existsParameter(path_id = "*", options = list(Type = "readonly"), DCI_Info = DCI_Info2);
  }
  if(objectType == "Species"){
    allObjects1 = existsSpeciesInitialValue(path_id = "*", options = list(Type = "readonly"), DCI_Info = DCI_Info1);
    allObjects2 = existsSpeciesInitialValue(path_id = "*", options = list(Type = "readonly"), DCI_Info = DCI_Info2);
  }
  if(objectType == "Observer"){
    allObjects1 = existsObserver(path_id = "*", DCI_Info = DCI_Info1);
    allObjects2 = existsObserver(path_id = "*", DCI_Info = DCI_Info2);
  }
  
  objectsPaths = c();
  existanceInFirst = c();
  existanceInSecond = c();
  valuesFirst = c();
  valuesSecond = c();
  formulaFlagsFirst = c();
  formulaFlagsSecond = c();
  formulaStringsFirst = c();
  formulaStringsSecond = c();
  
  #Mark indices in the second simulation which have been compared
  idxComparedInSecond = c();
  
  #Iterate through all objects from the first simulation and compare them to the objects from the second simulation.
  for (firstIdx in allObjects1$Index){
    #Flag determining whether the objects are identical in both simulations.
    identical = TRUE;
    #Flag determining whether the object exists in the first simulation
    existsInFirst = TRUE;
    #Get the path of the entry.
    objectInfo = splitSimNameFromPath(allObjects1$Path[firstIdx]);
    objectPath = objectInfo$Path;
    #Some parameters (e.g., 'AbsTol') do not require the name of the simulation as path prefix.
    #To correctly construct the path in the other simulation, mark whether addition of simulation name is required.
    pathIncludesSimName = TRUE;
    if (! (nchar(objectInfo$SimName) > 0) ){
      pathIncludesSimName = FALSE;
    }
    #ID of the object in the first simulation
    idFirst = allObjects1$ID[firstIdx];
    
    #Value of the object in the first simulation
    firstValue = NaN;
    if(objectType == "Parameter"){
      firstValue = getParameter(path_id = idFirst, options = list(Type = "current"), DCI_Info = DCI_Info1)$Value;
    }
    if(objectType == "Species"){
      firstValue = getSpeciesInitialValue(path_id = idFirst, options = list(Type = "current"), DCI_Info = DCI_Info1)$Value;
    }
    
    #Is the object defined by a formula in the first simulation?
    isFormulaInFirst = TRUE;
    if(objectType == "Parameter"){
      isFormulaInFirst = as.logical(getParameter(path_id = idFirst, options = list(Type = "current", Property = "IsFormula"), DCI_Info = DCI_Info1)$Value);
    }
    if(objectType == "Species"){
      isFormulaInFirst = as.logical(getSpeciesInitialValue(path_id = idFirst, options = list(Type = "current", Property = "IsFormula"), DCI_Info = DCI_Info1)$Value);
    }
    #Formula string (empty if the object is non-formula)
    formulaStringFirst = "";
    if(isFormulaInFirst){
      if(objectType == "Parameter"){
        formulaStringFirst = getParameter(path_id = idFirst, options = list(Type = "current", Property = "Formula"), DCI_Info = DCI_Info1)$Value;
      }
      if(objectType == "Species"){
        formulaStringFirst = getSpeciesInitialValue(path_id = idFirst, options = list(Type = "current", Property = "Formula"), DCI_Info = DCI_Info1)$Value;
      }
      if(objectType == "Observer"){
        formulaStringFirst = getObserverFormula(path_id = idFirst, DCI_Info = DCI_Info1)$Value;
      }
    }
    
    #Find the entry with the same path in the second simulation.
    simNameForSearch = "";
    if (pathIncludesSimName){
      simNameForSearch = simName2;
    }
    if(objectType == "Parameter"){
      secondObject = existsParameter(path_id = paste(simNameForSearch, objectPath, sep = "|"), options = list(Type = "readonly"), DCI_Info = DCI_Info2);
    }
    if(objectType == "Species"){
      secondObject = existsSpeciesInitialValue(path_id = paste(simNameForSearch, objectPath, sep = "|"), options = list(Type = "readonly"), DCI_Info = DCI_Info2);
    }
    if(objectType == "Observer"){
      secondObject = existsObserver(path_id = paste(simNameForSearch, objectPath, sep = "|"), DCI_Info = DCI_Info2);
    }
    if((existsInSecond = secondObject$isExisting)){
      #ID of the object in the second simulation
      idSecond = allObjects2$ID[secondObject$Index];
      #Value of the object in the second simulation
      secondValue = NaN;
      if(objectType == "Parameter"){
        secondValue = getParameter(path_id = idSecond, options = list(Type = "current"), DCI_Info = DCI_Info2)$Value;
      }
      if(objectType == "Species"){
        secondValue = getSpeciesInitialValue(path_id = idSecond, options = list(Type = "current"), DCI_Info = DCI_Info2)$Value;
      }
      #Is the object defined by a formula in the second simulation?
      isFormulaInSecond = TRUE;
      if(objectType == "Parameter"){
        isFormulaInSecond = as.logical(getParameter(path_id = idSecond, options = list(Type = "current", Property = "IsFormula"), DCI_Info = DCI_Info2)$Value);
      }
      if(objectType == "Species"){
        isFormulaInSecond = as.logical(getSpeciesInitialValue(path_id = idSecond, options = list(Type = "current", Property = "IsFormula"), DCI_Info = DCI_Info2)$Value);
      }
      #Formula string (empty if the object is non-formula)
      formulaStringSecond = "";
      if(isFormulaInSecond){
        if(objectType == "Parameter"){
          formulaStringSecond = getParameter(path_id = idSecond, options = list(Type = "current", Property = "Formula"), DCI_Info = DCI_Info2)$Value;
        }
        if(objectType == "Species"){
          formulaStringSecond = getSpeciesInitialValue(path_id = idSecond, options = list(Type = "current", Property = "Formula"), DCI_Info = DCI_Info2)$Value;
        }
        if(objectType == "Observer"){
          formulaStringSecond = getObserverFormula(path_id = idSecond, DCI_Info = DCI_Info2)$Value;
        }
      }
      
      #Compare objects
      identical = (isTRUE(all.equal.numeric(firstValue, secondValue, tolerance = numTolerance)));
      if (!ignoreFormula){
        identical = (identical & (isFormulaInFirst == isFormulaInSecond) & isTRUE(all.equal.character(formulaStringFirst, formulaStringSecond)));
      }
      #Mark the object idnex as compared.
      idxComparedInSecond = c(idxComparedInSecond, secondObject$Index);
    }
    #Object is not in second simulation
    else{
      identical = FALSE;
      secondValue = NaN;
      isFormulaInSecond = NaN;
      formulaStringSecond = "";
    }
    
    #If the objects are not identical, create an entry in differences-list.
    if( !identical ){
      objectsPaths = c(objectsPaths, objectPath);
      existanceInFirst = c(existanceInFirst, existsInFirst);
      existanceInSecond = c(existanceInSecond, existsInSecond);
      valuesFirst = c(valuesFirst, firstValue);
      valuesSecond = c(valuesSecond, secondValue);
      formulaFlagsFirst = c(formulaFlagsFirst, isFormulaInFirst);
      formulaFlagsSecond = c(formulaFlagsSecond, isFormulaInSecond);
      formulaStringsFirst = c(formulaStringsFirst, formulaStringFirst);
      formulaStringsSecond = c(formulaStringsSecond, formulaStringSecond);
    }
  }
  
  #Iterate through objects from the second simulation that have not been compared and compare them to the objects from the first simulation.
  for (secondIdx in setdiff(allObjects2$Index, idxComparedInSecond)){
    #Flag determining whether the objects are identical in both simulations.
    identical = TRUE;
    #Flag determining whether the object exists in the second simulation
    existsInSecond = TRUE;
    #Get the path of the object
    objectInfo = splitSimNameFromPath(allObjects1$Path[firstIdx]);
    objectPath = objectInfo$Path;
    #Some parameters (e.g., 'AbsTol') do not require the name of the simulation as path prefix.
    #To correctly construct the path in the other simulation, mark whether addition of simulation name is required.
    pathIncludesSimName = TRUE;
    if (! (nchar(objectInfo$SimName) > 0) ){
      pathIncludesSimName = FALSE;
    }
    #ID of the object in the second simulation
    idSecond = allObjects2$ID[secondIdx];
    
    #Value of the object in the second simulation
    secondValue = NaN;
    if(objectType == "Parameter"){
      secondValue = getParameter(path_id = idSecond, options = list(Type = "current"), DCI_Info = DCI_Info2)$Value;
    }
    if(objectType == "Species"){
      secondValue = getSpeciesInitialValue(path_id = idSecond, options = list(Type = "current"), DCI_Info = DCI_Info2)$Value;
    }
    
    #Is the object defined by a formula in the second simulation?
    isFormulaInSecond = TRUE;
    if(objectType == "Parameter"){
      isFormulaInSecond = as.logical(getParameter(path_id = idSecond, options = list(Type = "current", Property = "IsFormula"), DCI_Info = DCI_Info2)$Value);
    }
    if(objectType == "Species"){
      isFormulaInSecond = as.logical(getSpeciesInitialValue(path_id = idSecond, options = list(Type = "current", Property = "IsFormula"), DCI_Info = DCI_Info2)$Value);
    }
    #Formula string (empty if the object is non-formula)
    formulaStringSecond = "";
    if(isFormulaInSecond){
      if(objectType == "Parameter"){
        formulaStringSecond = getParameter(path_id = idSecond, options = list(Type = "current", Property = "Formula"), DCI_Info = DCI_Info2)$Value;
      }
      if(objectType == "Species"){
        formulaStringSecond = getSpeciesInitialValue(path_id = idSecond, options = list(Type = "current", Property = "Formula"), DCI_Info = DCI_Info2)$Value;
      }
      if(objectType == "Observer"){
        formulaStringSecond = getObserverFormula(path_id = idSecond, DCI_Info = DCI_Info2)$Value;
      }
    }
    
    #Find the object with the same path in the first simulation.
    simNameForSearch = "";
    if (pathIncludesSimName){
      simNameForSearch = simName1;
    }
    if(objectType == "Parameter"){
      firstObject = existsParameter(path_id = paste(simNameForSearch, objectPath, sep = "|"), options = list(Type = "readonly"), DCI_Info = DCI_Info1);
    }
    if(objectType == "Species"){
      firstObject = existsSpeciesInitialValue(path_id = paste(simNameForSearch, objectPath, sep = "|"), options = list(Type = "readonly"), DCI_Info = DCI_Info1);
    }
    if(objectType == "Observer"){
      firstObject = existsObserver(path_id = paste(simNameForSearch, objectPath, sep = "|"), DCI_Info = DCI_Info1);
    }
    
    if((existsInFirst = firstObject$isExisting)){
      #ID of the object in the first simulation
      idFirst = allObjects1$ID[firstObject$Index];
      #Value of the object in the first simulation
      firstValue = NaN;
      if(objectType == "Parameter"){
        firstValue = getParameter(path_id = idFirst, options = list(Type = "current"), DCI_Info = DCI_Info1)$Value;
      }
      if(objectType == "Species"){
        firstValue = getSpeciesInitialValue(path_id = idFirst, options = list(Type = "current"), DCI_Info = DCI_Info1)$Value;
      }
      
      #Is the object defined by a formula in the first simulation?
      isFormulaInFirst = TRUE;
      if(objectType == "Parameter"){
        isFormulaInFirst = as.logical(getParameter(path_id = idFirst, options = list(Type = "current", Property = "IsFormula"), DCI_Info = DCI_Info1)$Value);
      }
      if(objectType == "Species"){
        isFormulaInFirst = as.logical(getSpeciesInitialValue(path_id = idFirst, options = list(Type = "current", Property = "IsFormula"), DCI_Info = DCI_Info1)$Value);
      }
      #Formula string (empty if parameter is non-formula)
      formulaStringFirst = "";
      if(isFormulaInFirst){
        if(objectType == "Parameter"){
          formulaStringFirst = getParameter(path_id = idFirst, options = list(Type = "current", Property = "Formula"), DCI_Info = DCI_Info1)$Value;
        }
        if(objectType == "Species"){
          formulaStringFirst = getSpeciesInitialValue(path_id = idFirst, options = list(Type = "current", Property = "Formula"), DCI_Info = DCI_Info1)$Value;
        }
        if(objectType == "Observer"){
          formulaStringFirst = getObserverFormula(path_id = idFirst, DCI_Info = DCI_Info1)$Value;
        }
      }
      
      #Compare the objects
      identical = (isTRUE(all.equal.numeric(firstValue, secondValue, tolerance = numTolerance)));
      if (!ignoreFormula){
        identical = (identical & (isFormulaInFirst == isFormulaInSecond) & isTRUE(all.equal.character(formulaStringFirst, formulaStringSecond)));
      }
    }
    #Object is not in first simulation
    else{
      identical = FALSE;
      firstValue = NaN;
      isFormulaInFirst = NaN;
      formulaStringFirst = "";
    }
    
    #If the objects are not identical, create an entry in differences-list.
    if( !identical ){
      objectsPaths = c(objectsPaths, objectPath);
      existanceInFirst = c(existanceInFirst, existsInFirst);
      existanceInSecond = c(existanceInSecond, existsInSecond);
      valuesFirst = c(valuesFirst, firstValue);
      valuesSecond = c(valuesSecond, secondValue);
      formulaFlagsFirst = c(formulaFlagsFirst, isFormulaInFirst);
      formulaFlagsSecond = c(formulaFlagsSecond, isFormulaInSecond);
      formulaStringsFirst = c(formulaStringsFirst, formulaStringFirst);
      formulaStringsSecond = c(formulaStringsSecond, formulaStringSecond);
    }
  }
  
  objects_diff$Path = objectsPaths;
  objects_diff$isExistingFirst = existanceInFirst;
  objects_diff$isExistingSecond = existanceInSecond;
  objects_diff$ValueFirst = valuesFirst;
  objects_diff$ValueSecond = valuesSecond;
  objects_diff$IsFormulaFirst = formulaFlagsFirst;
  objects_diff$IsFormulaSecond = formulaFlagsSecond;
  objects_diff$FormulaFirst = formulaStringsFirst;
  objects_diff$FormulaSecond = formulaStringsSecond;
  return(objects_diff);
}

compareSimulationTime = function(simTime1, simTime2){
  identical = TRUE;
  #Compare time pattern by size and element-wise
  if ( !(length(simTime1$Time) == (length(simTime2$Time))) | !isTRUE(all.equal.numeric(simTime1$Time, simTime2$Time)) ){
    identical = FALSE;
  }
  return(identical);
}

writeToCSV = function(CSV_path, results){
  dirname <- dirname(CSV_path)
  if (!file.exists(dirname)) {
    dir.create(dirname, recursive=TRUE)
  }
  outString = "Simulation names";
  outString = c(outString, paste(results$SimNames[1], results$SimNames[2], sep = "\t"));
  
  outString = c(outString, "Parameter differences:");
  outString = c(outString, paste("Path", "Exists in first", "Exists in second", "Value in first", "Value in second", "Is formula in first", "Is formula in second", "Formula string in first", "Formula string in second", sep = "\t"));
  if (length(results$Params_diff) > 0){
    for (i in 1 : length(results$Params_diff$Path)){
      outString = c(outString, paste(
        results$Params_diff$Path[i],
        results$Params_diff$isExistingFirst[i],
        results$Params_diff$isExistingSecond[i],
        results$Params_diff$ValueFirst[i],
        results$Params_diff$ValueSecond[i],
        results$Params_diff$IsFormulaFirst[i],
        results$Params_diff$IsFormulaSecond[i],
        results$Params_diff$FormulaFirst[i],
        results$Params_diff$FormulaSecond[i],
        sep = "\t"));
    }
  }
  
  outString = c(outString, "Species differences:");
  outString = c(outString, paste("Path", "Exists in first", "Exists in second", "Value in first", "Value in second", "Is formula in first", "Is formula in second", "Formula string in first", "Formula string in second", sep = "\t"));
  if (length(results$Species_diff) > 0){
    for (i in 1 : length(results$Species_diff$Path)){
      outString = c(outString, paste(
        results$Species_diff$Path[i],
        results$Species_diff$isExistingFirst[i],
        results$Species_diff$isExistingSecond[i],
        results$Species_diff$ValueFirst[i],
        results$Species_diff$ValueSecond[i],
        results$Species_diff$IsFormulaFirst[i],
        results$Species_diff$IsFormulaSecond[i],
        results$Species_diff$FormulaFirst[i],
        results$Species_diff$FormulaSecond[i],
        sep = "\t"));
    }
  }
  
  outString = c(outString, "Observer differences:");
  outString = c(outString, paste("Path", "Exists in first", "Exists in second", "Value in first", "Value in second", "Is formula in first", "Is formula in second", "Formula string in first", "Formula string in second", sep = "\t"));
  if (length(results$Observer_diff) > 0){
    for (i in 1 : length(results$Observer_diff$Path)){
      outString = c(outString, paste(
        results$Observer_diff$Path[i],
        results$Observer_diff$isExistingFirst[i],
        results$Observer_diff$isExistingSecond[i],
        results$Observer_diff$ValueFirst[i],
        results$Observer_diff$ValueSecond[i],
        results$Observer_diff$IsFormulaFirst[i],
        results$Observer_diff$IsFormulaSecond[i],
        results$Observer_diff$FormulaFirst[i],
        results$Observer_diff$FormulaSecond[i],
        sep = "\t"));
    }
  }
  outString = c(outString, paste0("Time pattern identical\t", results$TimeIdentical));
  
  
  write.table(x = outString, file = resultsCSV_path, row.names = FALSE, col.names = FALSE, dec = ",");
}

#' Compares two simulations for differences.
#' Comparison is done for:
#' parameter existence, value and formula, 
#' species initial value existence, value and formula, 
#' observer existence and formula,
#' time pattern identity.
#'
#' @param DCI_Info1 DCI_Info of the first simulation
#' @param DCI_Info2 DCI_Info of the second simulation
#' @param numTolerance Maximal numeric deviation which is accepted as equal. Default value is 1e-8.
#' @param resultsCSV_path A path to a file where the results of the comparison are writen to. If emtpy, no output into a file is performed. If a file with the given path exists, it will be 
#' overwritten; otherwise, a new file is created. Emtpy by default.
#' @param ignoreFormula If true, only values of parameters or molecule start values are compared. If false, formula strings are compared. FALSE by default.
#'
#' @return
#' A list containing the lists 'SimNames', 'Params_diff', 'Species_diff', 'Observer_diff', and a boolean 'TimeIdentical'.
#' 'SimNames' contains the names of the compared simulations.
#' 'Params_diff', 'Species_diff' and 'Observer_diff' contain columns 'Path' with the path of the compared object,
#' 'isExistingFirst' boolean value whether the object exists in the first simulation,
#' 'isExistingSecond' boolean value whether the object  exists in the second simulation,
#' 'ValueFirst' value of the object in the first simulation,
#' 'ValueSecond' value of the object in the second simulation,
#' 'IsFormulaFirst' boolean value whether the object is defined by a formula in the first simulation,
#' 'IsFormulaSecond' boolean value whether the object is defined by a formula in the second simulation,
#' 'FormulaFirst' string reprentation of the parameter value in the first simulation (empty if parameter is not defined by a formula),
#' 'FormulaSecond' string reprentation of the parameter value in the second simulation (empty if parameter is not defined by a formula).
#' 'TimeIdentical' is a boolean defining whether the time-patterns are equal in both simulations.
#' If DCI_Info1 and DCI_Info2 have the same handle-ID, identity of the simulations is assumed without comparison.
#' If no differences are found, an empty list is returned for the corresponding entry (parameters, species, observers).
#' @export
#'
#' @examples
#' dci_info1 = initSimulation(simModelXML1, whichInitParam = "none");
#' dci_info2_diff = initSimulation(simModelXML1, whichInitParam = "all");
#' diffs = compareSimulations(DCI_Info1 = dci_info1, DCI_Info2 = dci_info2_diff, ignoreFormula = TRUE, resultsCSV_path = "C:/diffs.csv");
#' 
compareSimulations = function(DCI_Info1 = {}, DCI_Info2 = {}, numTolerance = 1e-8, resultsCSV_path = "", ignoreFormula = FALSE){
  #Check for missing DCI_Info
  if (length(DCI_Info1) == 0 | length(DCI_Info2) == 0)
  {
    stop("One of the inputs 'DCI_Info' is missing.")
  }
  #Numerical tolerance sanity check
  if (numTolerance < 0){
    stop(paste("numTolerance must be positive, value", numTolerance, "is invalid!"));
  }
  #ignoreFormula non-boolean
  if (!is.logical(ignoreFormula)){
    stop(paste("ignoreFormula must be a boolean, value", ignoreFormula, "is invalid! Use TRUE of FALSE."));
  }
  
  #list of the results - this is the output of the function.
  results = list();
  
  #Try to get simulation names. Use any parameter path.
  if (length(DCI_Info1$ReferenceTab$AllParameters$Path) > 0){
    anyParam_1_path = DCI_Info1$ReferenceTab$AllParameters$Path[1];
    simName1 = splitSimNameFromPath(anyParam_1_path)$SimName;
    
    anyParam_1_path = "";
  }
  else{
    simName1 = "Not available";
  }
  #Name of the second simulation
  if (length(DCI_Info2$ReferenceTab$AllParameters$Path) > 0){
    anyParam_2_path = DCI_Info2$ReferenceTab$AllParameters$Path[1];
    simName2 = splitSimNameFromPath(anyParam_2_path)$SimName;
    
    anyParam_2_path = "";
  }
  else{
    simName2 = "Not available";
  }
  #Store simulation names into the output.
  results$SimNames = c(simName1, simName2);
  
  #If same DCI is compared, return no differences without comparison
  if (DCI_Info1$Handle == DCI_Info2$Handle){
    objects_diff = list();
    objects_diff$Path = c();
    objects_diff$isExistingFirst = c();
    objects_diff$isExistingSecond = c();
    objects_diff$ValueFirst = c();
    objects_diff$ValueSecond = c();
    objects_diff$IsFormulaFirst = c();
    objects_diff$IsFormulaSecond = c();
    objects_diff$FormulaFrist = c();
    objects_diff$FormulaSecond = c();
    results$Params_diff = objects_diff;
    results$Species_diff = objects_diff;
    results$Observer_diff = objects_diff;
    results$TimeIdentical = TRUE;
    
    #If a path to output file a specified, write the results into a CSV-file.
    if (!resultsCSV_path == "")
    {
      writeToCSV(CSV_path = resultsCSV_path, results = results);
    }
    return(results)
  }
  
  #Compare parameters, species, and observers.
  params_diff = compareStructures("Parameter", DCI_Info1, DCI_Info2, numTolerance = numTolerance, simName1 = simName1, simName2 = simName2, ignoreFormula = ignoreFormula);
  species_diff = compareStructures("Species", DCI_Info1, DCI_Info2, numTolerance = numTolerance, simName1 = simName1, simName2 = simName2, ignoreFormula = ignoreFormula);
  observer_diff = compareStructures("Observer", DCI_Info1, DCI_Info2,numTolerance = numTolerance, simName1 = simName1, simName2 = simName2, ignoreFormula = ignoreFormula);
  
  #Compare simulation time
  simTime1 = getSimulationTime(DCI_Info = DCI_Info1);
  simTime2 = getSimulationTime(DCI_Info = DCI_Info2);
  timeIdentical = compareSimulationTime(simTime1, simTime2);
  
  #Store parameter differences
  results$Params_diff = params_diff;
  results$Species_diff = species_diff;
  results$Observer_diff = observer_diff;
  results$TimeIdentical = timeIdentical;
  
  #If a path to output file a specified, write the results into a CSV-file.
  if (!resultsCSV_path == "")
  {
    writeToCSV(CSV_path = resultsCSV_path, results = results);
  }
  
  return(results)
}
