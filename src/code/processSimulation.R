
processSimulation <- function(DCI_Info = {})
{
	if (length(DCI_Info) == 0)
	{
		stop("Input 'DCI_Info' is missing.")
	}
	
    DCI_Info <- updateSimulationInputs(DCI_Info)
	
	# Process Data
	h <- .Call("RDCI_ProcessData", as.integer(DCI_Info$Handle), as.integer(1))
	if (h == 0)
	{
	  stop(.Call("RDCI_GetLastError"))
	}
  
	# Get updated input tables
	TableArray <- .Call("RDCI_GetInputTables", as.integer(DCI_Info$Handle), as.integer(1))
	DCI_Info$InputTab <- TableArray
	
	# Get outputs
	DCI_Info$OutputTabTime <- .Call("RDCI_GetOutputTable", as.integer(DCI_Info$Handle), as.integer(1), as.integer(1))
	DCI_Info$OutputTabValue <- .Call("RDCI_GetOutputTable", as.integer(DCI_Info$Handle), as.integer(2), as.integer(1))
	return(DCI_Info)
}
