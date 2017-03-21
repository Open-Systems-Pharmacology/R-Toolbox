codepath <- "C:/VSS-Development/Projects/MoBi/ToolBoxforR/Dev/Basis Toolbox/Code"
rfiles <- dir(codepath, pattern="*.R")
for (i in 1:length(rfiles)) {
  rfile <- file.path(codepath,rfiles[i])
  if (!(file.info(rfile)$isdir)) {
    source(rfile)
  }
}