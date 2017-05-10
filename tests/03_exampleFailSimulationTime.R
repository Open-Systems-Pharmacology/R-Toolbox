
simModelXML <- "./models/black american girl.xml"
dci_info <- initSimulation(XML=simModelXML, whichInitParam="none")
length (dci_info$InputTab$TimeSchema$StartTime) #2 -> two different time intervals

processSimulation(DCI_Info=dci_info) # works!

dci_info <- setSimulationTime(c(1:60, timepoints=(0:7)*60), DCI_Info=dci_info) # two different time intervals
processSimulation(DCI_Info=dci_info) # works!

dci_info <- setSimulationTime(c(1:60), DCI_Info=dci_info) # one time interval
processSimulation(DCI_Info=dci_info) # not working!

# Error in processSimulation(DCI_Info = dci_info) : 
#   Unknown interval distribution type passed: 
