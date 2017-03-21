require(RUnit, quietly=TRUE)
require(codetools, quietly=TRUE)
require(MoBiToolboxForR, quietly=TRUE)

stopifnot(exists ("testDir")) #  <- "C:/workspace/SB Suite/MoBi_ToolBox_for_R/branches/7.0/Dev/Basis Toolbox/Test"
setwd(testDir)
options <- getOption("RUnit")
options$silent <- TRUE
options("RUnit"=options)

myTestSuite <- defineTestSuite(name = "MoBiToolBoxForRTests", dirs=testDir,testFileRegexp="^runit.+\\.R")
testResults <- runTestSuite(testSuites=myTestSuite, useOwnErrorHandler=TRUE)
printTextProtocol(testResults)

htmlFileName <- "Protocol.html"
printHTMLProtocol(testResults, fileName=htmlFileName)
# Repair href Links:
protocol.html = scan(file = htmlFileName, what = character(0))
protocol.html = gsub('href=\"', 'href=\"file://', protocol.html)
protocol.html = gsub('Basis', 'Basis%20', protocol.html)
write(protocol.html, htmlFileName)

checkUsagePackage("MoBiToolboxForR")


