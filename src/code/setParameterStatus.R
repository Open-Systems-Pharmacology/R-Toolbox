
setParameterStatus <- function(ParameterStatus = {}, DCI_Info = {})
{
	if (length(DCI_Info) == 0)
	{
		stop("Input 'DCI_Info' is missing.")
	}
	if (length(ParameterStatus) == 0)
	{
		stop("Input 'ParameterStatus' is missing.")
	}
  if (length(which(names(ParameterStatus) == "ParameterType")) == 0) 
  {
    stop("Input 'ParameterStatus' does not contain 'ParameterType' information.")  
  }
	if (length(which(names(ParameterStatus) == "Parameters")) == 0) 
	{
	  stop("Input 'ParameterStatus' does not contain 'Parameters' information.")  
	}
	if (length(which(names(ParameterStatus) == "TableParameters")) == 0) 
	{
	  stop("Input 'ParameterStatus' does not contain 'TableParameters' information.")  
	}
	if (length(which(names(ParameterStatus) == "SpeciesInitialValues")) == 0) 
	{
	  stop("Input 'ParameterStatus' does not contain 'SpeciesInitialValues' information.")  
	}
	if (length(which(names(ParameterStatus) == "SpeciesScaleFactors")) == 0) 
	{
	  stop("Input 'ParameterStatus' does not contain 'SpeciesScaleFactors' information.")  
	}
	if (length(which(names(ParameterStatus) == "SimulationTime")) == 0) 
	{
	  stop("Input 'ParameterStatus' does not contain 'SimulationTime' information.")  
	}
	
  # Set parameter values
  if (length(ParameterStatus$Parameters$Value) > 0) {
    options <- list(Type=ParameterStatus$ParameterType, Index=ParameterStatus$Parameters$Index)  
    suppressWarnings(DCI_Info <- setParameter(value=ParameterStatus$Parameters$Value, path_id="*", options=options, DCI_Info=DCI_Info))
  }
  
	# Set table parameter values
	if (length(ParameterStatus$TableParameters$ID) > 0) {
	  options <- list(Type=ParameterStatus$ParameterType)      
	  DCI_Info <- setTableParameter(table=ParameterStatus$TableParameters, options=options, DCI_Info=DCI_Info) 
	}
  
	# Set species initial values
  if (length(ParameterStatus$SpeciesInitialValues$Value) > 0) {
    options <- list(Type=ParameterStatus$ParameterType, Property="InitialValue", Index=ParameterStatus$SpeciesInitialValues$Index)      
    DCI_Info <- setSpeciesInitialValue(value=ParameterStatus$SpeciesInitialValues$Value, path_id="*", options=options, DCI_Info=DCI_Info) 
  }
  
  # Set species scale factors
  if (length(ParameterStatus$SpeciesScaleFactors$Value) > 0) {
    options <- list(Type=ParameterStatus$ParameterType, Property="ScaleFactor", Index=ParameterStatus$SpeciesScaleFactors$Index)      
    DCI_Info <- setSpeciesInitialValue(value=ParameterStatus$SpeciesScaleFactors$Value, path_id="*", options=options, DCI_Info=DCI_Info) 
  }

  # Set simulation time
  DCI_Info <- setSimulationTime(timepoints=ParameterStatus$SimulationTime$Time, DCI_Info=DCI_Info)
  
  return(DCI_Info)
}
