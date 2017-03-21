
initSpeciesInitialValue <- function(initStruct = list(), path_id ="", initializeIfFormula = "withWarning")
{
	if (length(initStruct) == 0)
	{
		initStruct <- list(Parameters = list(Path_ID = "", InitializeIfFormula = ""), InitialValues = list(Path_ID = "", InitializeIfFormula = ""))
	}
	if (!is.list(initStruct) == 1)
	{
		stop("initStruct has wrong format.")
	}
	if (path_id == "")
	{
		stop("path_id is missing.")
	}
	if (!(toupper(initializeIfFormula) %in% c('ALWAYS','WITHWARNING','NEVER')))
	{
		stop(paste("Wrong input for initializeIfFormula: ", initializeIfFormula," plese use always, withWarning or never"))
	}
	if ((length(initStruct$InitialValues$Path_ID) == 1) & (initStruct$InitialValues$Path_ID[1] == ""))
	{
		initStruct$InitialValues$Path_ID <- path_id
		initStruct$InitialValues$InitializeIfFormula <- initializeIfFormula
		
	} else {
		initStruct$InitialValues$Path_ID <- rbind(initStruct$InitialValues$Path_ID, path_id)
		initStruct$InitialValues$InitializeIfFormula <- rbind(initStruct$InitialValues$InitializeIfFormula, initializeIfFormula)
	}	
	return(initStruct)
}

