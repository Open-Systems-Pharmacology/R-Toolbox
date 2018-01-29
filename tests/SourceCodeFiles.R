rfiles <- dir(srcFolderDir, pattern = "*.R")
for (i in 1:length(rfiles)) {
    rfile <- file.path(srcFolderDir, rfiles[i])
  if (!(file.info(rfile)$isdir)) {
    source(rfile)
  }
}