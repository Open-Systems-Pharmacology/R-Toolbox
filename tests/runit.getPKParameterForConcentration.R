#require(RUnit, quietly=TRUE)
#require(MoBiToolboxForR, quietly=TRUE)
simModelXML <- "./tests/models/black american girl.xml"
tableParamXML <- "./tests/models/TableParameters.xml"
standard_dci_info <- initSimulation(XML=simModelXML, whichInitParam="none")
tableParam_dci_info <- initSimulation(XML=tableParamXML, whichInitParam="all", SimulationNumber=2)


# TODO more tests here.

test.CheckGetPKParameterForConcentration <- function() {
  
  res = getPKParameterForConcentration(1:10, c(1:5, 5*exp(seq(-0.1, -5, length.out = 5))))
  checkEquals(res$C_max, 5)
  
}