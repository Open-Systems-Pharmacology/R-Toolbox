updateSimulationInputs <- function(DCI_Info = {})
{
	# Set Variables
	variableTableIndexes <- grep("VARIABLE", toupper(names(DCI_Info$InputTab)), fixed=TRUE)
	for (i in variableTableIndexes)
	{
		if (length(DCI_Info$InputTab[[i]]$ID))
		{
			h <- .Call("RDCI_SetInputTable", as.integer(DCI_Info$Handle),as.integer(i),DCI_Info$InputTab[[i]])
			if (h == 0)
			{
				stop(.Call("RDCI_GetLastError"))
			}
		}
	}
  
	# Set Time Schema
	i <- which(names(DCI_Info$InputTab) == "TimeSchema")
	h <- .Call("RDCI_SetInputTable", as.integer(DCI_Info$Handle),as.integer(i),DCI_Info$InputTab[[i]])
	if (h == 0)
	{
	  stop(.Call("RDCI_GetLastError"))
	}
	return(DCI_Info)
}