
saveSimulationToXML <- function(XML = "", DCI_Info = {})
{
  warningMessage <- ""
  
  if (length(DCI_Info) == 0)
  {
    stop("No DCI_Info provided.")
  }
  
  if (length(DCI_Info$Handle) == 0)
  {
    stop("Invalid structure in provided DCI_Info object")
  }
  
	if (XML == "")
	{ 
		XML <- file.path(getwd(), "CurrentSimulation.xml")
	} 
  else 
  {
    dirname <- dirname(XML)
    if (!file.exists(dirname)) {
      dir.create(dirname, recursive=TRUE)
      warningMessage <- paste("The path ", dirname, " was not existing and has been created!", sep="")
    }
	}
	if (file.exists(XML)) 
	{
	  filename <- basename(XML)
	  warningMessage <- paste("The file ", filename, " already exists and has been overwritten!", sep="")
	}
	
	DCI_Info <- updateSimulationInputs(DCI_Info)
	retval = .Call('RDCI_Invoke', DCI_Info$Handle, 'SaveSimulationToXml', XML);

  if (is.character(retval) & retval != "") 
  {
    stop(retval)
  }
	if (retval == 0)
	{
		stop("Could not Save Simulation to File.")
	}
  if (warningMessage != "")
  {
    warning(warningMessage)
  }
  return (XML)
}
