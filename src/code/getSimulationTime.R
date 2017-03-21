
getSimulationTime <- function(DCI_Info = {})
{
	if (length(DCI_Info) == 0)
	{
		stop("Input 'DCI_Info' is missing.")
	}

	if (length(DCI_Info$InputTab) == 0)
	{
		stop("Input 'DCI_Info' has improper structure")
	}
	iTab <- which(names(DCI_Info$InputTab) == "TimeSchema")
	InfoTable <- DCI_Info$InputTab[[iTab]]
	# calculate discretized simulation Time points
	timepattern <- {}
	idx <- which(InfoTable$Distribution == "Equidistant")
	
	if (any(InfoTable$Distribution != "Equidistant"))
	{
		warning("Only equidistant distribution of time points yet implemented. Ignoring other entries.")
	}
	
	for (i in idx)
	{
		if (InfoTable$NoOfTimePoints[i] == 1)
		{
			timepattern <- c(timepattern, InfoTable$StartTime[i], InfoTable$EndTime[i])
		} else {
			timepattern <- c(timepattern, seq(InfoTable$StartTime[i], InfoTable$EndTime[i], length.out = InfoTable$NoOfTimePoints[i]))
		}
	}
	timepattern <- sort(unique(timepattern))
	return(list(Time = timepattern, Pattern = InfoTable))
}
