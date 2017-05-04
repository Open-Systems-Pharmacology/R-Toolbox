
Report_to_screen <- function(Text, report)
{
	if (report != "none")
	{print(Text)}
}

checkInputList <- function(Parameters, further_options)
{
	for (i in 1:length(names( further_options)))
	{
		tmp <- further_options
		if (length(which("NUMFORMAT"==names(tmp)))>0)
		{
			Parameters$NumFormat <- tmp$NUMFORMAT;
		}
		
		if (length(which("SCALEFACTORS_CSV"==names(tmp)))>0)
		{
			if (length(dir(pattern = tmp$SCALEFACTORS_CSV))>0)
			{
				Parameters$ScaleFactors_csv <- tmp$SCALEFACTORS_CSV
			} else {
				stop(paste("There exists no Scalefactor file ", tmp$SCALEFACTORS_CSV, sep = ""))
			}
		}
		
		if (length(which("GETSCALEFACTORS_CSV"==names(tmp)))>0)
		{
			Parameters$getScaleFactors_csv <- tmp$GETSCALEFACTORS_CSV
		}
		
		if (length(which("ADDFILE"==names(tmp)))>0)
		{
			Parameters$addFile <- tmp$ADDFILE
		}
		
		if (length(which("STOPONWARNINGS"==names(tmp)))>0)
		{
			Parameters$stopOnWarnings <- tmp$STOPONWARNINGS
		}
		
		if (length(which("EXECUTIONTIMELIMIT"==names(tmp)))>0)
		{
			Parameters$executionTimeLimit <- tmp$EXECUTIONTIMELIMIT
		}
		
		if (length(which("REPORT"==names(tmp)))>0)
		{
			Parameters$report <- tmp$REPORT
		}
		
		if (is.na(Parameters$NumFormat) & (Parameters$ScaleFactors_csv | Parameters$getScaleFactors_csv))
		{
			stop("You have to specify the option NumFormat")
		}
	}
	return(Parameters)
}	

CheckXML <- function(XML)
{
	if (file.exists(XML) == FALSE) 
	{
		stop(paste("XML", XML, "does not exist"))
	}
	return(0)
}

initSimulation <- function(XML = "", ParamList = numeric(0), whichInitParam = "", further_options = "", Version = "6_0", SimulationNumber = 1)
{
	DCI_Info <- list()
	if (XML == "")
	{
		stop("Input \"XML\" is missing.")
	}
	
	val <- CheckXML(XML)
	
	DCI_Info$MOBI_SETTINGS <- MobiSettings()
	
	if ((length(ParamList) == 0) & !(toupper(whichInitParam) %in% c("ALL", "NONE", "ALLNONFORMULA")))
	{
		stop ("No parameters can be initialized. Please provide either 'ParamList' or 'whichInitParam' with one the following valid keywords: all, none, allNonFormula.")
	}
	
	# load DCIR6_0-DLL

	if (length(grep("DCIR6_0", names(getLoadedDLLs()), fixed =TRUE)) ==0)
	{
		dyn.load(DCI_Info$MOBI_SETTINGS$RInterface)
	}
	
	# initialize Parameters used for further processing
	Parameters <- list("iniAllParameters" = FALSE,
			"iniAllNonFormulaParameters" = FALSE,
			"iniNoneParameters" = FALSE,
			"iniStructure" = FALSE, 
			"NumFormat" = NA,
			"ScaleFactors_csv" = FALSE,
			"getScaleFactors_csv" = FALSE,
			"addFile" = TRUE,
			"stopOnWarnings" = TRUE,
			"executionTimeLimit" = 0,
			"report" = "short", 
			"prjFile" = XML,
			"SimIndex" = NA)
	
	# Parameter_CSVs can be used in Matlab -- Just to warn that this is not so in R
	if (as.numeric(length(which("Parameter_CSV"== names(ParamList)))>0) == 1)
	{
		stop("R Version does not support Parameter.Callsv");
	}
	
	# Set the correct flags if whichInitParam is provided
	if (length(ParamList) != 0)
	{
		Parameters$iniStructure <- TRUE
	} else { 
		switch(toupper(whichInitParam), 
				"ALL" = Parameters$iniAllParameters <- TRUE, 
				"NONE" = Parameters$iniNoneParameters <- TRUE,
				"ALLNONFORMULA" = Parameters$iniAllNonFormulaParameters <- TRUE
				, stop("Wrong Input for whichInitParam", whichInitParam, "please use none, all or allNonFormula."))
	}
	
	
	# Checking inputs
	if (length(ParamList) > 0 | length(further_options) > 0)
	{
		Parameters <- checkInputList(Parameters, further_options)
	}
	
	# clear memory ... just to make sure
	if (SimulationNumber == 1)
	{	
		v <- .Call("RDCI_DestroyAllComponents", DCI_Info$MOBI_SETTINGS$RInterface);
		if (v==0)
		{
			stop(.Call("RDCI_GetLastError"))
		}
		rm(v)
		Parameters$SimIndex <- 1
	} 
	
	# load SimModelComponent

	DCI_Info$Handle <- .Call("RDCI_LoadComponent", DCI_Info$MOBI_SETTINGS$SimModelComp)
	
	if (DCI_Info$Handle==0)
	{
		stop(.Call("RDCI_GetLastError"));
	}
	
	# Preparing the DCI
	ConfigTab <- .Call("RDCI_GetParameterTable", as.integer(DCI_Info$Handle),as.integer(1), as.integer(1))
	ConfigTab$SimModelSchema <- DCI_Info$MOBI_SETTINGS$SimModelSchema
	ConfigTab$SimulationFile <- Parameters$prjFile
	
	if (Parameters$stopOnWarnings != 0)
	{
		ConfigTab$StopOnWarnings <- as.integer(1);
	}
	
	if (Parameters$executionTimeLimit != 0)
	{
		ConfigTab$ExecutionTimeLimit <- as.integer(Parameters$executionTimeLimit);
	}
	
	retval <- .Call("RDCI_SetParameterTable", as.integer(DCI_Info$Handle), as.integer(1), ConfigTab)
	if (retval == 0)
	{
		stop("Error in SetParameterTable.")
	}
	
	# Configuring the Model

	retval <- .Call("RDCI_Configure", as.integer(DCI_Info$Handle))
	if (retval == 0)
	{
	  stop(.Call("RDCI_GetLastError"));
	}
	
	# for the moment set all output variables/observer as persistable, so that their values are accessible after processSimulation
	# this should be changed in the future to setting only otputs of interest  as persistable 
	retval = .Call('RDCI_Invoke', DCI_Info$Handle, 'SetAllOutputsPersistable', "");
	if (is.character(retval) & retval != "") 
    {
      stop(retval)
    }
	if (retval == 0)
	{
      stop("Error in initSimulation: 'SetAllOutputsPersistable' failed.")
	}
	
	# Get tables
	TableArray <- .Call("RDCI_GetInputTables", as.integer(DCI_Info$Handle), as.integer(1))
	
	DCI_Info$InputTab <- TableArray

	# set variable Parameter 
	
	for (id in which(names(DCI_Info$InputTab) %in% c("VariableParameters", "VariableSpecies")))
	{
		
		#all parameter variable
		if (Parameters$iniAllParameters != 0)
		{
			DCI_Info$InputTab[[id]]$IsVariable = rep(1, length(DCI_Info$InputTab[[id]]$IsVariable)) ;
		} else if ( Parameters$iniAllNonFormulaParameters != 0) 
		{
			DCI_Info$InputTab[[id]]$IsVariable[which(DCI_Info$InputTab[[id]]$ParameterType != "Formula")] <- 1
		} else if (Parameters$iniNoneParameters != 0) 
		{
			DCI_Info$InputTab[[id]]$IsVariable <- rep(0, length(DCI_Info$InputTab[[id]]$IsVariable))		
		} else if (Parameters$iniStructure) 
		{
			DCI_Info<-initParameterStruct(ParamList, TableArray[[id]], id, DCI_Info)
		}
		
		h <- .Call("RDCI_SetInputTable", as.integer(DCI_Info$Handle), as.integer(id), DCI_Info$InputTab[[id]])
		if (h == 0)
		{
			stop(.Call("RDCI_GetLastError"))
		}		
	}
	
	#Process Metadata
	h <- .Call("RDCI_ProcessMetaData", as.integer(DCI_Info$Handle))
	if (h == 0)
	{
		stop("Error in ProcessMetaData")
	}
	
	#save final tables
	for (i in 1:length(TableArray))
	{
		DCI_Info$InputTab[[i]] <- .Call("RDCI_GetInputTable", as.integer(DCI_Info$Handle), as.integer(i),as.integer(1))
		
	}
	
	DCI_Info$ReferenceTab <- DCI_Info$InputTab

	return(DCI_Info)
}


