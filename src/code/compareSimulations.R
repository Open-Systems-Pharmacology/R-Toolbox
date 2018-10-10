getObjectProperty = function(objectType, path_id, DCI_Info, property){
  switch(objectType,
         Parameter = {
           value = getParameter(path_id = path_id, options = list(Type = "current", Property = property), DCI_Info = DCI_Info)$Value;
         },
         Species = {
           if (property == "Value"){
             property = "InitialValue";
           }
           value = getSpeciesInitialValue(path_id = path_id, options = list(Type = "current", Property = property), DCI_Info = DCI_Info)$Value;
         },
         Observer = {
           #No value is returned for observers.
           if (property == "Value"){
             value = NaN;
           }
           if (property == "IsFormula"){
             value = TRUE;
           }
           if (property == "Formula"){
             value = getObserverFormula(path_id = path_id, DCI_Info = DCI_Info)$Value;
           }
         },
         stop(paste0("getObjectValue: Unknown objectType passed: ", objectType, "; accepted values are 'Parameter', 'Species', 'Observer'"))
  );
  return(value);
}

getObjectExistance = function(objectType, path_id, DCI_Info){
  switch(objectType,
         Parameter = {
           value = existsParameter(path_id = path_id, options = list(Type = "readonly"), DCI_Info = DCI_Info);
         },
         Species = {
           value = existsSpeciesInitialValue(path_id = path_id, options = list(Type = "readonly"), DCI_Info = DCI_Info);
         },
         Observer = {
           value = existsObserver(path_id = path_id, DCI_Info = DCI_Info);
         },
         stop(paste0("getObjectExistance: Unknown objectType passed: ", objectType, "; accepted values are 'Parameter', 'Species', 'Observer'"))
  );
  return(value);
}

compareObjects = function(DCI_Info1, DCI_Info2, options, excludeIdx = c()){
  if (length(options) == 0){
    stop("compareObjects: options is not provided!");
  }
  
  #This list holds information about the differences between simulations objects.
  objects_diff = list();
  #Path of the object in the simulation.
  objects_diff$Path = c();
  #Does the object exist in the first simulation?
  objects_diff$isExistingFirst = c();
  #Does the object exist in the second simulation?
  objects_diff$isExistingSecond = c();
  #Numerical value of the object in the first simulation.
  objects_diff$ValueFirst = c();
  #Numerical value of the object in the second simulation.
  objects_diff$ValueSecond = c();
  #Is the objects value defined by a formula in the first simulation?
  objects_diff$IsFormulaFirst = c();
  #Is the objects value defined by a formula in the second simulation?
  objects_diff$IsFormulaSecond = c();
  #Formula string defining the value of the object in the first simulation. Empty if no formula used.
  objects_diff$FormulaFirst = c();
  #Formula string defining the value of the object in the second simulation. Empty if no formula used.
  objects_diff$FormulaSecond = c();
  
  #Get existence information about all objects.
  allObjects1 = getObjectExistance(objectType = options$objectType, path_id = "*", DCI_Info = DCI_Info1);
  allObjects2 = getObjectExistance(objectType = options$objectType, path_id = "*", DCI_Info = DCI_Info2);
  
  #Mark indices of the objects that have been compared in the second simulation.
  idxComparedInSecond = c();
  #Iterate through all objects from the first simulation and compare them to the objects from the second simulation.
  for (firstIdx in setdiff(allObjects1$Index, excludeIdx)){
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
    firstValue = getObjectProperty(objectType = options$objectType, path_id = idFirst, DCI_Info = DCI_Info1, property = "Value");
    
    #Is the object defined by a formula in the first simulation?
    isFormulaInFirst = as.logical(getObjectProperty(objectType = options$objectType, path_id = idFirst, DCI_Info = DCI_Info1, property = "IsFormula"));
    
    #Formula string (empty if the object is non-formula)
    formulaStringFirst = "";
    if(isFormulaInFirst){
      formulaStringFirst = getObjectProperty(objectType = options$objectType, path_id = idFirst, DCI_Info = DCI_Info1, property = "Formula");
    }
    
    #Find the entry with the same path in the second simulation.
    simNameForSearch = "";
    if (pathIncludesSimName){
      simNameForSearch = options$simName2;
    }
    secondObject = getObjectExistance(objectType = options$objectType, path_id = paste(simNameForSearch, objectPath, sep = "|"), DCI_Info = DCI_Info2);
    
    if((existsInSecond = secondObject$isExisting)){
      #ID of the object in the second simulation
      idSecond = allObjects2$ID[secondObject$Index];
      
      #Value of the object in the second simulation
      secondValue = getObjectProperty(objectType = options$objectType, path_id = idSecond, DCI_Info = DCI_Info2, property = "Value");
      
      #Is the object defined by a formula in the second simulation?
      isFormulaInSecond = as.logical(getObjectProperty(objectType = options$objectType, path_id = idSecond, DCI_Info = DCI_Info2, property = "IsFormula"));
      
      #Formula string (empty if the object is non-formula)
      formulaStringSecond = "";
      if(isFormulaInSecond){
        formulaStringSecond = getObjectProperty(objectType = options$objectType, path_id = idSecond, DCI_Info = DCI_Info2, property = "Formula")
      }
      
      #Compare objects
      identical = (isTRUE(all.equal.numeric(firstValue, secondValue, tolerance = options$numTolerance)));
      if (!options$ignoreFormula){
        identical = (identical & (isFormulaInFirst == isFormulaInSecond) & isTRUE(all.equal.character(formulaStringFirst, formulaStringSecond)));
      }
      #Mark the object's idnex as compared.
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
      objects_diff$Path = c(objects_diff$Path, objectPath);
      objects_diff$isExistingFirst = c(objects_diff$isExistingFirst, existsInFirst);
      objects_diff$isExistingSecond = c(objects_diff$isExistingSecond, existsInSecond);
      objects_diff$ValueFirst = c(objects_diff$ValueFirst, firstValue);
      objects_diff$ValueSecond = c(objects_diff$ValueSecond, secondValue);
      objects_diff$IsFormulaFirst = c(objects_diff$IsFormulaFirst, isFormulaInFirst);
      objects_diff$IsFormulaSecond = c(objects_diff$IsFormulaSecond, isFormulaInSecond);
      objects_diff$FormulaFirst  = c(objects_diff$FormulaFirst , formulaStringFirst);
      objects_diff$FormulaSecond = c(objects_diff$FormulaSecond, formulaStringSecond);
    }
  }
  
  return(list(diffs = objects_diff, comparedInSecond = idxComparedInSecond));
}

#objectType can be "Parameter", "Species", or "Observer"
compareStructures = function(DCI_Info1, DCI_Info2, options){
  #Compare all objects from the first simulation to the objects from the second simulation.
  comparisonResult_first = compareObjects(DCI_Info1 = DCI_Info1, DCI_Info2 = DCI_Info2, options = options);
  
  #Compare objects from the second simulation that have not been compared before to the objects from the first simulation.
  comparisonResult_second = compareObjects(DCI_Info1 = DCI_Info2, DCI_Info2 = DCI_Info1, options = options, excludeIdx = comparisonResult_first$comparedInSecond);

  #Combine results of the comparisons
  objects_diff = list();
  objects_diff$Path = c(comparisonResult_first$diffs$Path, comparisonResult_second$diffs$Path);
  objects_diff$isExistingFirst = c(comparisonResult_first$diffs$isExistingFirst, comparisonResult_second$diffs$isExistingSecond);
  #Does the object exist in the second simulation?
  objects_diff$isExistingSecond = c(comparisonResult_first$diffs$isExistingSecond, comparisonResult_second$diffs$isExistingFirst);
  #Numerical value of the object in the first simulation.
  objects_diff$ValueFirst = c(comparisonResult_first$diffs$ValueFirst, comparisonResult_second$diffs$ValueSecond);
  #Numerical value of the object in the second simulation.
  objects_diff$ValueSecond = c(comparisonResult_first$diffs$ValueSecond, comparisonResult_second$diffs$ValueFirst);
  #Is the objects value defined by a formula in the first simulation?
  objects_diff$IsFormulaFirst = c(comparisonResult_first$diffs$IsFormulaFirst, comparisonResult_second$diffs$IsFormulaSecond);
  #Is the objects value defined by a formula in the second simulation?
  objects_diff$IsFormulaSecond = c(comparisonResult_first$diffs$IsFormulaSecond, comparisonResult_second$diffs$IsFormulaFirst);
  #Formula string defining the value of the object in the first simulation. Empty if no formula used.
  objects_diff$FormulaFirst = c(comparisonResult_first$diffs$FormulaFirst, comparisonResult_second$diffs$FormulaSecond);
  #Formula string defining the value of the object in the second simulation. Empty if no formula used.
  objects_diff$FormulaSecond = c(comparisonResult_first$diffs$FormulaSecond, comparisonResult_second$diffs$FormulaFirst);

  return(objects_diff);
}

#Compare output patterns.
compareSimulationTime = function(simTime1, simTime2){
  identical = TRUE;
  #Compare time pattern by size and element-wise
  if ( !(length(simTime1$Time) == (length(simTime2$Time))) | !isTRUE(all.equal.numeric(simTime1$Time, simTime2$Time)) ){
    identical = FALSE;
  }
  return(identical);
}

#Append comparison results of a specific type to a string that will be written to a file.
appendResultsToString = function(outString, objectsResults){
  outString = c(outString, paste("Path", "Exists in first", "Exists in second", "Value in first", "Value in second", "Is formula in first", "Is formula in second", "Formula string in first", "Formula string in second", sep = "\t"));
  if (length(objectsResults) > 0){
    for (i in 1 : length(objectsResults$Path)){
      outString = c(outString, paste(
        objectsResults$Path[i],
        objectsResults$isExistingFirst[i],
        objectsResults$isExistingSecond[i],
        objectsResults$ValueFirst[i],
        objectsResults$ValueSecond[i],
        objectsResults$IsFormulaFirst[i],
        objectsResults$IsFormulaSecond[i],
        objectsResults$FormulaFirst[i],
        objectsResults$FormulaSecond[i],
        sep = "\t"));
    }
  }
  return(outString);
}

#Write results into a CSV file.
writeToCSV = function(CSV_path, results){
  dirname <- dirname(CSV_path)
  if (!file.exists(dirname)) {
    dir.create(dirname, recursive=TRUE)
  }
  
  #Build the output string.
  outString = "Simulation names";
  outString = c(outString, paste(results$SimNames[1], results$SimNames[2], sep = "\t"));
  
  outString = c(outString, "Parameter differences:");
  outString = appendResultsToString(outString = outString, objectsResults = results$Params_diff);

  outString = c(outString, "Species differences:");
  outString = appendResultsToString(outString = outString, objectsResults = results$Species_diff);
  
  outString = c(outString, "Observer differences:");
  outString = appendResultsToString(outString = outString, objectsResults = results$Observer_diff);
  
  outString = c(outString, paste0("Time pattern identical\t", results$TimeIdentical));
  
  write.table(x = outString, file = CSV_path, row.names = FALSE, col.names = FALSE, dec = ",");
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
#' @param resultsCSV_path A path to a tab-separated file where the results of the comparison are writen to. If emtpy, no output into a file is performed. If a file with the given path exists, it will be 
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
#' 'FormulaFirst' string reprentation of the object's formula in the first simulation (empty if object is not defined by a formula),
#' 'FormulaSecond' string reprentation of the object's formula in the second simulation (empty if object is not defined by a formula).
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
  comparisonOptions = list(objectType = "Parameter", simName1 = simName1, simName2 = simName2, ignoreFormula = ignoreFormula, numTolerance = numTolerance);
  results$Params_diff = compareStructures(DCI_Info1 = DCI_Info1, DCI_Info2 = DCI_Info2, options = comparisonOptions);
  comparisonOptions$objectType = "Species";
  results$Species_diff = compareStructures(DCI_Info1 = DCI_Info1, DCI_Info2 = DCI_Info2, options = comparisonOptions);
  comparisonOptions$objectType = "Observer";
  results$Observer_diff = compareStructures(DCI_Info1 = DCI_Info1, DCI_Info2 = DCI_Info2, options = comparisonOptions);
  
  #Compare simulation time
  simTime1 = getSimulationTime(DCI_Info = DCI_Info1);
  simTime2 = getSimulationTime(DCI_Info = DCI_Info2);
  results$TimeIdentical = compareSimulationTime(simTime1, simTime2);
  
  #If a path to output file a specified, write the results into a CSV-file.
  if (!resultsCSV_path == "")
  {
    writeToCSV(CSV_path = resultsCSV_path, results = results);
  }
  
  return(results)
}
