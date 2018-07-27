
MobiSettings <- function(libraryName = "MoBiToolboxForR")
{
  paths <- .libPaths()
  for (i in 1:length(paths)) {
    libpath <- file.path(paths[i], libraryName, "libs", "x64");
    
    if (file.exists(libpath)) {
      
      Sys.setenv(path = paste(libpath, Sys.getenv("path"), sep =";"))
      return(list("SimModelSchema" = paste(libpath,"/OSPSuite.SimModel.xsd", sep=""),
                  "SimModelComp" = paste(libpath, "/OSPSuite_SimModelComp.xml", sep=""),
                  "RInterface" = paste(libpath,"/DCIR6_1.dll", sep="")));
    }
  }
  
  stop(paste("Library '", libraryName, "' not found in libPaths()!", sep=""));
}

