#require(RUnit, quietly=TRUE)
#require(MoBiToolboxForR, quietly=TRUE)
simModelXML <- "./tests/models/black american girl.xml"
standard_dci_info <- initSimulation(XML=simModelXML, whichInitParam="none")

test.NoTimepointsInput <- function() {
  dci_info <- standard_dci_info
  checkException(setSimulationTime(timepoints={}, DCI_Info=dci_info))
}

test.NoDCIInfoInput <- function() {
  checkException(setSimulationTime(timepoints=(0:24)*60, DCI_Info={}))
}

test.EndTime_1500_NoOfTimePoints_100 <- function() {
  dci_info <- standard_dci_info
  unit <- dci_info$InputTab$TimeSchema$Unit
  timepoints <- seq(from=0, to=1500, length = 100)
  dci_info <- setSimulationTime(timepoints=timepoints, DCI_Info=dci_info)
  dciTimepoints <- getSimulationTime(DCI_Info=dci_info)
  checkEquals(dciTimepoints$Time, timepoints)
  checkEquals(dci_info$InputTab$TimeSchema$NoOfTimePoints[1], 100)
  checkEquals(dci_info$InputTab$TimeSchema$StartTime[1], 0)
  checkEquals(dci_info$InputTab$TimeSchema$EndTime[1], 1500)
  checkEquals(dci_info$InputTab$TimeSchema$Distribution[1], "Equidistant")
  checkEquals(dci_info$InputTab$TimeSchema$Unit, unit)  
}

test.EndTime_1500_NoOfTimePoints_101 <- function() {
  dci_info <- standard_dci_info
  unit <- dci_info$InputTab$TimeSchema$Unit
  timepoints <- seq(from=0, to=1500, length = 101)
  dci_info <- setSimulationTime(timepoints=timepoints, DCI_Info=dci_info)
  dciTimepoints <- getSimulationTime(DCI_Info=dci_info)
  checkEqualsNumeric(dciTimepoints$Time, timepoints)
  checkEquals(dci_info$InputTab$TimeSchema$NoOfTimePoints[1], 101)
  checkEquals(dci_info$InputTab$TimeSchema$StartTime[1], 0)
  checkEquals(dci_info$InputTab$TimeSchema$EndTime[1], 1500)
  checkEquals(dci_info$InputTab$TimeSchema$Distribution[1], "Equidistant")
  checkEquals(dci_info$InputTab$TimeSchema$Unit, unit)  
}

test.EndTime_2000_NoOfTimePoints_11 <- function() {
  dci_info <- standard_dci_info
  unit <- dci_info$InputTab$TimeSchema$Unit
  timepoints <- seq(from=0, to=2000, length = 11)
  dci_info <- setSimulationTime(timepoints=timepoints, DCI_Info=dci_info)
  dciTimepoints <- getSimulationTime(DCI_Info=dci_info)
  checkEqualsNumeric(timepoints, dciTimepoints$Time)
  checkEquals(dci_info$InputTab$TimeSchema$NoOfTimePoints[1], 11)
  checkEquals(dci_info$InputTab$TimeSchema$StartTime[1], 0)
  checkEquals(dci_info$InputTab$TimeSchema$EndTime[1], 2000)
  checkEquals(dci_info$InputTab$TimeSchema$Distribution[1], "Equidistant")
  checkEquals(dci_info$InputTab$TimeSchema$Unit, unit)  
}

test.SpecialTimepoints <- function() {
  dci_info <- standard_dci_info
  unit <- dci_info$InputTab$TimeSchema$Unit
  #first 8 hours hourly than every 8th hour
  timepoints <- c((0:7)*60, seq(from=8,to=24,length=3)*60)
  dci_info <- setSimulationTime(timepoints=timepoints, DCI_Info=dci_info)
  dciTimepoints <- getSimulationTime(DCI_Info=dci_info)
  checkEqualsNumeric(dciTimepoints$Time, timepoints)
  checkEquals(dci_info$InputTab$TimeSchema$NoOfTimePoints[1], 9)
  checkEquals(dci_info$InputTab$TimeSchema$StartTime[1], 0)
  checkEquals(dci_info$InputTab$TimeSchema$EndTime[1], 480)
  checkEquals(dci_info$InputTab$TimeSchema$Distribution[1], "Equidistant")
  checkEquals(dci_info$InputTab$TimeSchema$Unit[1], unit[1])  
  checkEquals(dci_info$InputTab$TimeSchema$NoOfTimePoints[2], 2)
  checkEquals(dci_info$InputTab$TimeSchema$StartTime[2], 960)
  checkEquals(dci_info$InputTab$TimeSchema$EndTime[2], 1440)
  checkEquals(dci_info$InputTab$TimeSchema$Distribution[2], "Equidistant")
  checkEquals(dci_info$InputTab$TimeSchema$Unit[2], unit[1])
}

test.SpecialTimepointsWithAddTimePattern <- function() {
  dci_info <- standard_dci_info
  unit <- dci_info$InputTab$TimeSchema$Unit
  #first 8 hours hourly than every 8th hour
  timepoints <- c((0:7)*60, seq(from=8,to=24,length=3)*60)
  dci_info <- setSimulationTime(timepoints=(0:7)*60, DCI_Info=dci_info)
  dci_info <- addTimePattern(startTime=480, endTime=1440, NoOfTimePoints=3, DCI_Info=dci_info, Unit=unit)
  dciTimepoints <- getSimulationTime(DCI_Info=dci_info)
  checkEqualsNumeric(dciTimepoints$Time, timepoints)
  checkEquals(dci_info$InputTab$TimeSchema$NoOfTimePoints[1], 8)
  checkEquals(dci_info$InputTab$TimeSchema$StartTime[1], 0)
  checkEquals(dci_info$InputTab$TimeSchema$EndTime[1], 420)
  checkEquals(dci_info$InputTab$TimeSchema$Distribution[1], "Equidistant")
  checkEquals(dci_info$InputTab$TimeSchema$Unit[1], unit[1])
  checkEquals(dci_info$InputTab$TimeSchema$NoOfTimePoints[2], 3)
  checkEquals(dci_info$InputTab$TimeSchema$StartTime[2], 480)
  checkEquals(dci_info$InputTab$TimeSchema$EndTime[2], 1440)
  checkEquals(dci_info$InputTab$TimeSchema$Distribution[2], "Equidistant")
  checkEquals(dci_info$InputTab$TimeSchema$Unit[2], unit[1])
}

test.SetSimulationTimePointsAndSimulate <- function(){
  dci_info <- standard_dci_info
  unit <- dci_info$InputTab$TimeSchema$Unit
  #first 8 hours hourly 
  timepoints <- c((0:7)*60)
  dci_info <- setSimulationTime(c(1:60, timepoints=(0:7)*60), DCI_Info=dci_info)
  dci_info = processSimulation(DCI_Info=dci_info);
  
  dci_info <- setSimulationTime(c(1:60), DCI_Info=dci_info)
  dci_info = processSimulation(DCI_Info=dci_info) # that failes currently with 
  # "Error in processSimulation(DCI_Info = dci_info) : "
  # "Unknown interval distribution type passed: ''"
  
}

