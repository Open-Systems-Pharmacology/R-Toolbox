setSimulationTime <- function(timepoints={}, DCI_Info = {})
{
  #Check for emtpy time interval.
	if (length(timepoints) ==0)
	{
		stop("Input 'timepoints' is missing.")
	}
  #Check for empty DCI_Info
	if (length(DCI_Info) ==0)
	{
		stop("Input 'DCI_Info' is missing.")
	}
	iTab <-which(names(DCI_Info$InputTab) == "TimeSchema")
	baseunit <- DCI_Info$InputTab[[iTab]]$Unit[1]
	#Erase old time schema
  for (j in 1:length(DCI_Info$InputTab[[iTab]]))
	{
    attr <- attributes(DCI_Info$InputTab[[iTab]][[j]])
		DCI_Info$InputTab[[iTab]][[j]] <- numeric(0)
    attributes(DCI_Info$InputTab[[iTab]][[j]]) <- attr
	}
	timepoints <- sort(timepoints, decreasing = FALSE)
	timepoints <- unique(timepoints)
	
  #split time points into equidistant vectors
  while (length(timepoints) > 1) {
    deltaT <- round(diff(timepoints),12)

    if (length(unique(deltaT)) > 1) {
      idx <- min(which(deltaT == unique(deltaT)[2]))
      equidistantPts <- seq(from=timepoints[1], to=timepoints[idx], length=idx)
      DCI_Info <- addTimePattern(startTime = equidistantPts[1], endTime = equidistantPts[length(equidistantPts)],NoOfTimePoints = length(equidistantPts), Distribution = "Equidistant", DCI_Info = DCI_Info, Unit=baseunit)
      timepoints <- setdiff(timepoints, equidistantPts)
    } else {
      DCI_Info <- addTimePattern(startTime = timepoints[1], endTime = timepoints[length(timepoints)],NoOfTimePoints = length(timepoints), Distribution = "Equidistant", DCI_Info = DCI_Info, Unit=baseunit)
      timepoints <- {}
    }
  }

  if (length(timepoints) == 1) {
	  DCI_Info <- addTimePattern(startTime = timepoints[1], endTime = timepoints[length(timepoints)],NoOfTimePoints = length(timepoints), Distribution = "Equidistant", DCI_Info = DCI_Info, Unit=baseunit)
  }
	return(DCI_Info)
	
}


addTimePattern <- function(startTime, endTime, NoOfTimePoints, Distribution = "Equidistant", DCI_Info, Unit)
{
	iTab <-which(names(DCI_Info$InputTab) == "TimeSchema")

	attr <- attributes(DCI_Info$InputTab[[iTab]]$StartTime)
	DCI_Info$InputTab[[iTab]]$StartTime <- c(DCI_Info$InputTab[[iTab]]$StartTime, startTime)
	attributes(DCI_Info$InputTab[[iTab]]$StartTime) <- attr

	attr <- attributes(DCI_Info$InputTab[[iTab]]$EndTime)
	DCI_Info$InputTab[[iTab]]$EndTime <- c(DCI_Info$InputTab[[iTab]]$EndTime, endTime)
	attributes(DCI_Info$InputTab[[iTab]]$EndTime) <- attr

	attr <- attributes(DCI_Info$InputTab[[iTab]]$NoOfTimePoints)
	DCI_Info$InputTab[[iTab]]$NoOfTimePoints <- c(DCI_Info$InputTab[[iTab]]$NoOfTimePoints, NoOfTimePoints)
	attributes(DCI_Info$InputTab[[iTab]]$NoOfTimePoints) <- attr

	attr <- attributes(DCI_Info$InputTab[[iTab]]$Distribution)
	DCI_Info$InputTab[[iTab]]$Distribution <- c(DCI_Info$InputTab[[iTab]]$Distribution, Distribution)
	attributes(DCI_Info$InputTab[[iTab]]$Distribution) <- attr

	attr <- attributes(DCI_Info$InputTab[[iTab]]$Unit)
	DCI_Info$InputTab[[iTab]]$Unit <- c(DCI_Info$InputTab[[iTab]]$Unit, Unit)
	attributes(DCI_Info$InputTab[[iTab]]$Unit) <- attr

  return(DCI_Info)
}