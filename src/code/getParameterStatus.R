
getParameterStatus <- function(options = {}, DCI_Info = {})
{
  if (length(DCI_Info) == 0)
  {
    stop("Input 'DCI_Info' is missing.")
  }
  if (length(options) == 0)
  {
    options <- list(Type="variable")
  }
  if (length(grep("Type",names(options), fixed =TRUE)) == 0)
  {
    options[[length(options)+1]] <- "variable"
    names(options)[length(options)] <- "Type"
  }
  if (!(toupper(options$Type) %in% c("VARIABLE", "REFERENCE")))
  {
    stop("Invalid type provided. Use one of the following: variable, reference.")
  }
  
  parameterStatus = list(ParameterType = options$Type)
  suppressWarnings(params <- getParameter(path_id="*", options = list(Type = options$Type), DCI_Info=DCI_Info))
  parameterStatus[[length(parameterStatus)+1]] <- params
  names(parameterStatus)[length(parameterStatus)] <- "Parameters"
  parameterStatus[[length(parameterStatus)+1]] <- getTableParameter(path_id="*", options = list(Type = options$Type), DCI_Info=DCI_Info)
  names(parameterStatus)[length(parameterStatus)] <- "TableParameters"
  parameterStatus[[length(parameterStatus)+1]] <- getSpeciesInitialValue(path_id="*",list(Type =  options$Type), DCI_Info=DCI_Info)
  names(parameterStatus)[length(parameterStatus)] <- "SpeciesInitialValues"
  parameterStatus[[length(parameterStatus)+1]] <- getSpeciesInitialValue(path_id ="*",list(Type = options$Type, Property = "ScaleFactor"), DCI_Info=DCI_Info);
  names(parameterStatus)[length(parameterStatus)] <- "SpeciesScaleFactors"
  parameterStatus[[length(parameterStatus)+1]] <- getSimulationTime(DCI_Info=DCI_Info);
  names(parameterStatus)[length(parameterStatus)] <- "SimulationTime"
  return(parameterStatus)
}
