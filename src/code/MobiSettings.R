
MobiSettings <- function(libraryName = "MoBiToolboxForR")
{
  pathes <- .libPaths()
  for (i in 1:length(pathes)) {
    libpath <- file.path(pathes[i], libraryName, "libs", "i386")
    
    if (file.exists(libpath)) {
    
      Sys.setenv(path = paste(libpath, Sys.getenv("path"), sep =";"))
      MobiSettings <- list("SimModelSchema" = paste(libpath,"\\OSPSuite.SimModel.xsd", sep=""),
                           "SimModelComp" = paste(libpath, "\\OSPSuite_SimModelComp.xml", sep=""),
                           "RInterface" = paste(libpath,"\\DCIR6_0.dll", sep=""));
    }
  }

	return(MobiSettings) 
}

