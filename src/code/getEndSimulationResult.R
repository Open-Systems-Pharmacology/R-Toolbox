#Return just the last value of the simulated species.
getEndSimulationResult <- function(path_id = "*", options = {}, DCI_Info = {})
{
  if (length(DCI_Info) == 0)
  {
    stop("No DCI_Info provided.")
  }
  if (length(options) == 0)
  {
    options <- list(Index = numeric(0))
  }
  if (length(grep("Index",names(options), fixed =TRUE)) == 0)
  {
    options[[length(options)+1]] <- numeric(0)
    names(options)[length(options)] <- "Index"
  }
  if (is.character(path_id) && path_id == "")
  {
    stop("Empty path_id provided.")
  }
  if (!any(names(DCI_Info) == "OutputTabTime") | !any(names(DCI_Info) == "OutputTabValue"))
  {
    stop("Model has not yet been simulated.")
  }
  if (length(DCI_Info$OutputTabValue) == 0)
  {
    stop("Outputs are empty. Probably the last processSimulation was interrupted by an error.")
  }
  
  ordering <- {}
  if (length(options$Index) > 0) 
  {
    ordering <- options$Index
    Path <- {}
    for (i in options$Index) {
      Path <- c(Path, attr(DCI_Info$OutputTabValue[[i]], "Path"))
    }
  }
  else
  {
    # get species
    tmp <-   existsSpeciesInitialValue(path_id=path_id, options = list(Type = "readOnly"), DCI_Info = DCI_Info)
    Index <- tmp$ID
    Path <- tmp$Path
    
    # add observers
    tmp <- existsObserver(path_id=path_id, options = list(Type = "readOnly"), DCI_Info = DCI_Info)
    Index <- c(Index, tmp$ID)
    Path <- c(Path, tmp$Path)
    
    for (i in 1:length(DCI_Info$OutputTabValue))
    {
      
      if (any(Index == as.double(attributes(DCI_Info$OutputTabValue[[i]])$ID)))
      {
        ordering <- c(ordering, i)
      }
    }
    if (length(ordering) == 0)
    {
      stop(paste("Result with path_id", path_id,"does not exist!"))
    }
  }
  
  time <- attributes(DCI_Info$OutputTabValue[[ordering[1]]])$TimeColumnIndex
  for (j in 2:length(ordering))
  {
    time <- c(time, attributes(DCI_Info$OutputTabValue[[ordering[j]]])$TimeColumnIndex)
  }
  time <- unique(time)
  if (length(time) == 1)
  {
    timegrid <- sort(unique(as.vector(DCI_Info$OutputTabTime$Time)))
  } else {
    timegrid <- {}
    for (j in time)
    {
      timegrid <- c(timegrid, sort(unique(as.vector(DCI_Info$OutputTabTime$Time[[j]]))))
    }
  }
  timegrid <- sort(unique(timegrid))
  #browser()
  values <- c();
  for (j in ordering)
  {
    lastIdx = length(DCI_Info$OutputTabValue[[j]]);
    tmp <- DCI_Info$OutputTabValue[[j]][lastIdx];
    values = c(values, tmp);
  }
  return(list(Value = values, Path = Path))
}
