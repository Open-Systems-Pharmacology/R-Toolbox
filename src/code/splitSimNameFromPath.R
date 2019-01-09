#' Extract simulation name from the given path or a list of paths of parameter/species/observer and return
#' a list containing the name of the simulation and the path(s) without the simulation name.
#' Basically, the part of the path string before the first path separator ("|") is treated like the name of the simulation.
#' If a path is empty, an error is thrown.
#' If a path does not consist of sub-parts separated by '|', an error is thrown.
#'
#' @param path String or a list of strings of a path leading to a parameter/species/observer.
#'
#' @return A list with entries "SimName" and "Path".
#' @export
#'
#' @examples
splitSimNameFromPath = function(path = ""){
  #Check for non-character path
  if (!is.character(path)){
    stop(paste("splitSimNameFromPath: path must be a string, value of", path, "is invalid!"));
  }
  
  #Number of path entries
  nrOfPaths = length(path);
  
  #Name(s) of simulation(s)
  simNames = vector(mode = "character", length = nrOfPaths);
  #Path(s) of the object(s) without simulation name
  objectPaths = vector(mode = "character", length = nrOfPaths);
  
  #Iterate through all entries and separate the first part of the string from the rest.
  for(i in (1 : nrOfPaths)){
    currPath = path[i];
    #Check for empty path (only if one path is provided)
    if (nchar(currPath) == 0){
      stop(paste0("splitSimNameFromPath (entry ", i, "): path cannot be empty!"));
    }
    
    path_parts = strsplit(path[i], "|", fixed = TRUE);
    #If the number of path parts is less then two, no distiction between simulation name and path can be performed
    if (length(path_parts[[1]]) <= 1){
      stop(paste0("The path ", currPath, " has no sub-parts separated by '|', cannot extract simulation name!"));
    }
    simName = path_parts[[1]][1];
    objectPath = paste(path_parts[[1]][2 : length(path_parts[[1]])], sep = "|", collapse = "|");
    
    simNames[i] = simName;
    objectPaths[i] = objectPath;
  }
  
  return(list(SimName = simNames, Path = objectPaths));
}