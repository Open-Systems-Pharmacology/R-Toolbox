
initParameterStruct<-function(initStruct,TableArray, iTab, DCI_Info)
{
	# iCol_isV  corresponds to $Variable
	# iCol_isF corresponds to $IsFormula
	# iCol_F corresponds to $Formula
	if (iTab == 2)
	{	
		tmpStruct <- initStruct$Parameters
		warningtxt <- "The parameter is defined as Formula: "
		errortxt <- paste(warningtxt,"If you want to inititalize this parameter do not use option 'never' for the function initParameter")
	} else if (iTab == 4)
	{ 
		tmpStruct <- initStruct$InitialValues
		warningtxt <- "The SpeciesInitialValue is defined as Formula: "
		errortxt <- paste(warningtxt,"If you want to inititalize this species initial value do not use option 'never' for the function initParameter")
		
	} else {
		stop("Incorrect 'iTab' value provided. Has to be '2' (Parameters) or '4' (Species).")
	}
	
	
	if (tmpStruct$Path_ID[1]!= "")
	{
		for (i in 1:length(tmpStruct$Path_ID))
		{
			id <- findTableIndex(path_id = tmpStruct$Path_ID[i], tableID = iTab,isReference =FALSE, DCI_Info = DCI_Info)
			
			if (toupper(tmpStruct$InitializeIfFormula[i]) == "ALWAYS")
			{
				TableArray$IsVariable[id] <- 1
			} else if(toupper(tmpStruct$InitializeIfFormula[i]) == "WITHWARNING")
			{
				TableArray$IsVariable[id] <- 1
				ix <- which(TableArray$IsFormula[id] == 1)
				for (j in id[ix])
				{
					warning(paste(warningtxt, "however it is initialized", TableArray$Path[j], TableArray$Formula[j]))	
				}
			} else if(toupper(tmpStruct$InitializeIfFormula[i]) == "NEVER")
			{
				ix <- which(TableArray$IsFormula[id] == 1)
				for (j in id[ix])
				{
					stop(paste(errortxt, TableArray$Path[j], TableArray$Formula[j]))	
				}
				
			} else {
				stop(paste("Improper value for InitializeIfFormula",tmpStruct$InitializeIfFormula[i]))
			}
			
		}
	}
	DCI_Info$InputTab[[iTab]] <- TableArray
	return(DCI_Info)	
}
