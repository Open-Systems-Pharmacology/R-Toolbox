#' Returns the list of warnings produced by the solver.
#'
#' @param DCI_Info 
#'
#' @return A list of warning strings. If no warnings were generated, an empty ist (character(0)) is returned.
#' @export
#'
#' @examples
getSolverWarnings <- function(DCI_Info = {}){
  if (length(DCI_Info) == 0)
  {
    stop("Input 'DCI_Info' is missing.")
  }
  
  #This will be the output list.
  warnings = c();
  #Get all warnings as a concatenated string.
  warningsAsString = .Call('RDCI_Invoke', as.integer(DCI_Info$Handle), 'GetSolverWarnings', "");
  
  #If retrieving warnings failed for some reason, stop with an error
  if (is.integer(warningsAsString) & warningsAsString == 0)
  {
    stop("Could not get solver warnings!")
  }
  
  #Split into sub-strings, each sub-string representing a distinct warning.
  warningsAsString = strsplit(warningsAsString, "¦", fixed = TRUE);
  for (warning in warningsAsString){
    warnings = c(warnings, warning);
  }
  
  return (warnings)
}