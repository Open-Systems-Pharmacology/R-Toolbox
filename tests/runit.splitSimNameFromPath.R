test.EmptyPath <- function() {
  #Single path
  checkException(splitSimNameFromPath());
  
  #Multiple paths
  paths =  c("SimName1|Path1|Path2|AbsTol1", "", "SimName2|Path1|Path2|AbsTol3",
             "SimName3|Path1|Path2|AbsTol4", "SimName4|Path1|Path2|AbsTol5");
  checkException(splitSimNameFromPath(paths));
}

test.NonCharacterPath = function(){
  checkException(splitSimNameFromPath(2));
}

testPathWithoutSimName = function(){
  pathInfo = splitSimNameFromPath("|AbsTol");
  checkEquals(pathInfo$SimName, "");
  checkEquals(pathInfo$Path, "AbsTol");
}

testPathWithSimName = function(){
  pathInfo = splitSimNameFromPath("SimName|Path1|Path2|AbsTol");
  checkEquals(pathInfo$SimName, "SimName");
  checkEquals(pathInfo$Path, "Path1|Path2|AbsTol");
}

testNoSubParts = function(){
  #Singel path
  checkException(splitSimNameFromPath("AbsTol"));
  #Multiple paths
  paths =  c("SimName1|Path1|Path2|AbsTol1", "AbsTol", "SimName2|Path1|Path2|AbsTol3",
             "SimName3|Path1|Path2|AbsTol4", "SimName4|Path1|Path2|AbsTol5");
  checkException(splitSimNameFromPath(paths));
}

testMultiplePaths = function(){
  paths =  c("SimName1|Path1|Path2|AbsTol1", "SimName2|Path1|Path2|AbsTol2",
             "SimName3|Path1|Path2|AbsTol3", "SimName4|Path1|Path2|AbsTol4");
  
  pathInfo = splitSimNameFromPath(paths);
  
  for (i in 1 : length(paths)){
    checkEquals(pathInfo$SimName[i], paste0("SimName", i));
    checkEquals(pathInfo$Path[i], paste0("Path1|Path2|AbsTol", i));
  }
}