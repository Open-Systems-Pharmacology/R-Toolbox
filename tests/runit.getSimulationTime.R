#require(RUnit, quietly=TRUE)
#require(MoBiToolboxForR, quietly=TRUE)
simModelXML <- "./tests/models/black american girl.xml"
standard_dci_info <- initSimulation(XML=simModelXML, whichInitParam="none")

test.NoDCIInfoInput <- function() {
  checkException(getSimulationTime(DCI_Info={}))
}

test.NoDCIInfoInput <- function() {
  dci_info <- standard_dci_info
  dci_info$InputTab <- {}
  checkException(getSimulationTime(DCI_Info=dci_info))
}

test.CheckTimeSchema <- function() {
  dci_info <- standard_dci_info
  unit <- dci_info$InputTab$TimeSchema$Unit
  dciTimepoints <- getSimulationTime(DCI_Info=dci_info)
  checkEqualsNumeric(dciTimepoints$Time, seq(from=0, to=1500, length = 100))
  checkEquals(names(dciTimepoints), c("Time","Pattern"))
  checkEquals(names(dciTimepoints$Pattern), c("StartTime","EndTime","Unit","NoOfTimePoints","Distribution"))
  checkEquals(names(attributes(dciTimepoints$Pattern$StartTime)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dciTimepoints$Pattern$EndTime)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dciTimepoints$Pattern$Unit)), c("Name","Description"))
  checkEquals(names(attributes(dciTimepoints$Pattern$NoOfTimePoints)), c("MinValue","MaxValue","DefaultValue","Name","Description"))
  checkEquals(names(attributes(dciTimepoints$Pattern$Distribution)), c("Name","Description"))
  checkEquals(dciTimepoints$Pattern$NoOfTimePoints, dci_info$InputTab$TimeSchema$NoOfTimePoints)
  checkEquals(dciTimepoints$Pattern$StartTime, dci_info$InputTab$TimeSchema$StartTime)
  checkEquals(dciTimepoints$Pattern$EndTime, dci_info$InputTab$TimeSchema$EndTime)
  checkEquals(dciTimepoints$Pattern$Distribution, dci_info$InputTab$TimeSchema$Distribution)
  checkEquals(dciTimepoints$Pattern$Unit, dci_info$InputTab$TimeSchema$Unit)
}

test.DistributionNotEquidistant <- function() {
  dci_info <- standard_dci_info
  unit <- dci_info$InputTab$TimeSchema$Unit
  dci_info$InputTab$TimeSchema$Distribution <- "dummy"
  options(warn = 2);
  checkException(dciTimepoints <- getSimulationTime(DCI_Info=dci_info))
  options(warn = 0);
  dciTimepoints <- getSimulationTime(DCI_Info=dci_info)
  checkEquals(dciTimepoints$Time, NULL)
  checkEquals(dciTimepoints$Pattern$NoOfTimePoints, dci_info$InputTab$TimeSchema$NoOfTimePoints)
  checkEquals(dciTimepoints$Pattern$StartTime, dci_info$InputTab$TimeSchema$StartTime)
  checkEquals(dciTimepoints$Pattern$EndTime, dci_info$InputTab$TimeSchema$EndTime)
  # checkEquals(dciTimepoints$Pattern$Distribution, dci_info$InputTab$TimeSchema$Distribution)
  checkEquals(dciTimepoints$Pattern$Unit, unit)
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
  checkEquals(dciTimepoints$Pattern$NoOfTimePoints[1], 8)
  checkEquals(dciTimepoints$Pattern$StartTime[1], 0)
  checkEquals(dciTimepoints$Pattern$EndTime[1], 420)
  checkEquals(dciTimepoints$Pattern$Distribution[1], "Equidistant")
  checkEquals(dciTimepoints$Pattern$Unit[1], unit[1])
  checkEquals(dciTimepoints$Pattern$NoOfTimePoints[2], 3)
  checkEquals(dciTimepoints$Pattern$StartTime[2], 480)
  checkEquals(dciTimepoints$Pattern$EndTime[2], 1440)
  checkEquals(dciTimepoints$Pattern$Distribution[2], "Equidistant")
  checkEquals(dciTimepoints$Pattern$Unit[2], unit[1])
}