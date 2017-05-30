
# install.packages("C:/workspace/sw/MoBiToolBoxForR_7.1.0.zip", repos = NULL, type = "win.binary", lib = "C:/workspace/sw/TBforRVERSIONS/7.1.0/")
# install.packages("C:/workspace/sw/MoBiToolboxForR_6.3.2.zip", repos = NULL, type = "win.binary", lib = "C:/workspace/sw/TBforRVERSIONS/6.3.2/")
# install.packages("C:/workspace/sw/MoBiToolboxForR_7.0.0.zip", repos = NULL, type = "win.binary", lib = "C:/workspace/sw/TBforRVERSIONS/7.0.0/")


# install.packages("C:/workspace/sw/MoBiToolBoxForR_6.3.2.zip", repos = NULL, type = "win.binary")

# .libPaths(c("C:/workspace/sw/TBforRVERSIONS/6.3.2", .libPaths())) 

.libPaths(c("C:/workspace/sw/TBforRVERSIONS/7.1.0.7", .libPaths())) 


library(MoBiToolboxForR)

xmlFile = "../../Models/VoleModelDevelopment/rabbitSensitivityAnalysisModels//50 mg Paracetamol PO admin Typical Tablet_OneCYp.xml"

setwd("C:/workspace/MoBi_Toolbox_for_R/R-Toolbox/tests")
xmlFile = "testSim/exampleSimuationHumCaffeine.xml"

xmlFile = "models/Rabbit Caffein.xml"
print (xmlFile)

path_output_plasma    = "*|Organism|PeripheralVenousBlood|.*|Plasma \\(Peripheral Venous Blood\\)"

path_gfr = "*|Kidney|GFR \\(specific\\)"

initStruct <- list()
initStruct <- initParameter(initStruct = initStruct, path_id = path_gfr)

dci_info <- initSimulation(XML = xmlFile,ParamList = initStruct)

observerList= existsObserver(path_id=path_output_plasma   ,DCI_Info=dci_info, options = list(Type="readonly"))$ID

dci_info <- processSimulation(DCI_Info=dci_info)
print (as.numeric(dci_info$OutputTabValue[[1]]))

.Call("RDCI_GetOutputTable",as.integer(dci_info$Handle), as.integer(2), as.integer(1))[[1]]
