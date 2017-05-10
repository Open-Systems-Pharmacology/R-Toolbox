# detach(package:MoBiToolboxForR)
# install.packages("C:/workspace/sw/MoBiToolBoxForR_7.1.0.zip", repos = NULL, type = "win.binary", lib = "C:/workspace/sw/TBforRVERSIONS/7.1.0/")
# install.packages("C:/workspace/sw/MoBiToolboxForR_6.3.2.zip", repos = NULL, type = "win.binary", lib = "C:/workspace/sw/TBforRVERSIONS/6.3.2/")
# install.packages("C:/workspace/sw/MoBiToolboxForR_7.0.0.zip", repos = NULL, type = "win.binary", lib = "C:/workspace/sw/TBforRVERSIONS/7.0.0/")


# install.packages("C:/workspace/sw/MoBiToolBoxForR_6.3.2.zip", repos = NULL, type = "win.binary")

.libPaths(c("C:/workspace/sw/TBforRVERSIONS/7.1.0.7", .libPaths())) 
#.libPaths(c("C:/workspace/sw/TBforRVERSIONS/6.3.2", .libPaths())) 

require(RUnit, quietly=TRUE)
require(MoBiToolboxForR)
simModelXML <- "tests/models/Rabbit Caffein.xml"
standard_dci_info <- initSimulation(XML=simModelXML, whichInitParam="none")
standard_dci_info <- processSimulation(DCI_Info = standard_dci_info)
res = getSimulationResult(DCI_Info=standard_dci_info, path_id = "*Rabbit Caffein|Organism|VenousBlood|Plasma|Caffeine*")
getPKParameterForConcentration(res[,1], res[,2])

plot(res[,1], res[,2], type = "l")


source("tests/runit.setSimulationTime.R")
test.SetSimulationTimePointsAndSimulate()

# if this works, run
# edit paths to test directory:

testDir ="test"
source( "test/DoRUnitTesting.R")
