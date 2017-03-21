# detach(package:MoBiToolboxForR)

install.packages("C:/workspace/SB Suite/MoBi_Toolbox_for_R/branches/7.0/Dist/MoBiToolboxForR_7.0.0.zip", 
                 repos = NULL, type = "win.binary")

require(RUnit, quietly=TRUE)
require(MoBiToolboxForR)
simModelXML <- "7.0/Dev/Basis Toolbox/Test//models/Rabbit Caffein.xml"
standard_dci_info <- initSimulation(XML=simModelXML, whichInitParam="none")
standard_dci_info <- processSimulation(DCI_Info = standard_dci_info)
res = getSimulationResult(DCI_Info=standard_dci_info)
getPKParameterForConcentration(res[,1], res[,2])
# if this works, run
# edit paths to test directory:

testDir <- "C:/workspace/SB Suite/MoBi_ToolBox_for_R/branches/7.0/Dev/Basis Toolbox/Test"

source('7.0/Dev/Basis Toolbox/Test/DoRUnitTesting.R')
