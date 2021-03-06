# Helper
# Returns a consistent base-R flavoured path
.homogenizePath <- function(winPath) {
  if (is.na(winPath) || !nzchar(winPath))
    return(NA)
  
  # get consistency (unix speparators) with R-base 
  result <- normalizePath(winPath, winslash="/")
  
  # remove trailing slash for consistancy with R-base
  result <- sub("/$", "", result)
  
  return(result)  
}

#' Tries to find the installation path for a specific version of PK-Sim from an Windows registry entry.
#'
#' @param pksim.version The version number of Pk-Sim as a string.
#'
#' @return
#' The path to the PK-Sim installation for version pksim.version or NA if no path could be found.
#' The path is separated with slashes (unix-style) and in compilance with base-R without a trailing slash.
#'
#' @examples
#' path = .getpathToPKSimInstallDirFromRegistry("7.4")
#' 
.getpathToPKSimInstallDirFromRegistry <- function(pksim.version) {
  pksimVersion <- trimws(pksim.version)
  
  if (.Platform$OS.type != "windows")
    stop("Only Windows platforms are supported")
  
  suite.name <- "Open Systems Pharmacology"
  product.name <- "PK-Sim"
  reg.path <- file.path("SOFTWARE", 
                        suite.name, 
                        product.name,
                        pksim.version,
                        fsep = "\\")
  
  reg.entry <- NA
  try(reg.entry <- readRegistry(reg.path, hive = "HLM", maxdepth = 1, view = "64-bit"), 
      silent = TRUE)
  
  if ("InstallDir" %in% names(reg.entry))
    return(.homogenizePath(reg.entry$InstallDir))  
  
  return(NA)
}

#' Tries to find the installation path for a specific version of PK-Sim via the filesystem.
#' Searching is done in the following order:
#' 1. Search via filesystem in a guessed installation folder from the base.search.folder
#' 3. Search via filesystem for PKSim.exe recursivly from the defined base.search.folder (fallback)
#'
#' @param pksim.version The version number of Pk-Sim as a string.
#' @param base.search.folder The base folder for filesystem-lookup fallback (default: 64-bit program folder)
#'
#' @return
#' The path to the PK-Sim installation for version pksim.version or NA if no path could be found.
#' The path is separated with slashes (unix-style) and in compilance with base-R without a trailing slash.
#' If more than one matching path is found a warning is produced.
#'
#' @examples
#' path = .getpathToPKSimInstallDir("7.4")
#' path2 = .getpathToPKSimInstallDir("7.5", "C:/MyOSPFolder/")
#' 
.getpathToPKSimInstallDirFromFileSystem <- function(pksim.version, 
                                                    base.search.folder = Sys.getenv("ProgramW6432")) {
  pksim.version <- trimws(pksim.version)
  base.search.folder <- trimws(base.search.folder)
  base.search.folder <- normalizePath(base.search.folder)
  
  if (.Platform$OS.type != "windows")
    stop("Only Windows platforms are supported")
  
  if (!nzchar(base.search.folder))
    return(NA)
    
  # First guess: OSP/PK-Sim folder
  suite.name <- "Open Systems Pharmacology"
  product.name <- "PK-Sim"

  full.guess <- file.path(base.search.folder, suite.name, fsep="\\") 
  full.match <- dir(full.guess, pattern = product.name, full.names = TRUE, 
                    include.dirs = TRUE)
  if (length(full.match) == 0) {
    # Second guess: Search base folder recursivly for exe.name
    # This might be expensive !
    exe.name = "PKSim.exe$"
    full.match <- list.files(base.search.folder, pattern = exe.name, 
                             recursive = TRUE, full.names = TRUE)
    full.match <- dirname(full.match)
  }
  
  if (length(full.match) != 0) {
    # match version
    full.match <- grep(pksim.version, full.match, fixed=TRUE, value=TRUE)
    if (length(full.match) > 1)
      warning("Ambiguous matches for PK-Sim installation path found. First match is returned.")
    
    return(.homogenizePath(full.match[1]))
  }
  
  return(NA)
}

#' Tries to find the installation path for a specific version of PK-Sim.
#' Searching is done in the following order:
#' 1. Search via Windows registry entry 
#' 2. Search via filesystem in a guessed installation folder from the base.search.folder (fallback 1)
#' 3. Search via filesystem for PKSim.exe recursivly from the defined base.search.folder (fallback 2)
#'
#' @param pksim.version The version number of Pk-Sim as a string.
#' @param base.search.folder The base folder for filesystem-lookup fallback (default: 64-bit program folder)
#'
#' @return
#' The path to the PK-Sim installation for version pksim.version or NA if no path could be found.
#' The path is separated with slashes (unix-style) and in compilance with base-R without a trailing slash.
#' If more than one matching path is found a warning is produced.
#'
#' @examples
#' path = .getpathToPKSimInstallDir("7.4")
#' path2 = .getpathToPKSimInstallDir("7.5", "C:/MyOSPFolder/")
#'
.getpathToPKSimInstallDir <- function(pksim.version, 
                                      base.search.folder = Sys.getenv("ProgramW6432")) {
  
  pksim.path <- .getpathToPKSimInstallDirFromRegistry(pksim.version)
  if (!is.na(pksim.path))
    return(pksim.path)
  
  pksim.path <- .getpathToPKSimInstallDirFromFileSystem(pksim.version, base.search.folder)
  if (!is.na(pksim.path))
    return(pksim.path)
  
  return(NA)
}
