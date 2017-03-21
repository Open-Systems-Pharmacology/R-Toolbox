require(RUnit, quietly=TRUE)
require(MoBiToolboxForR, quietly=TRUE)
testDir <- "C:/VSS/Projects/MoBi_ToolBox_for_R/Dev/Basis Toolbox/Test"
setwd(testDir)

simModelXML <- "./models/black american girl.xml"

track <- tracker()
track$init()

#code to be inspected
paramList <- {}
paramList <- inspect(initParameter(paramList, path_id = 113), track = track)
paramList <- inspect(initParameter(paramList, path_id = 114), track = track)
dci_info <- inspect(initSimulation(XML = simModelXML, ParamList = paramList), track = track)
# check <- inspect(existsParameter(path_id = 113, DCI_Info = dci_info), track = track)

resTrack <- track$getTrackInfo()
printHTML.trackInfo(resTrack)