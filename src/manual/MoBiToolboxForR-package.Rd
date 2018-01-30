\name{MoBiToolboxForR-package}
\alias{MoBiToolboxForR-package}
\alias{MoBiToolboxForR}
\docType{package}
\title{
MoBi Toolbox for R
}
\description{
This package provides functions to access a MoBi® model from R.
}
\details{
\tabular{ll}{
Package: \tab MoBiToolboxForR\cr
Type: \tab Package\cr
Version: \tab 7.3.0\cr
Date: \tab 2018-30-01\cr
License: \tab GPL2 \cr
}

The MoBi® Toolbox for R is a collection of R functions, which
  allow the processing of models developed in MoBi® from within R.
  For example, the R environment can be used to change parameters in
  a model developed in MoBi®, simulate the model, and analyze the results.
  This allows an efficient operation in the model analysis stage, using the
  programming options as well as the function library available within
  R in combination with the powerful modeling interface and solver
  kernel included in MoBi®.
  
The most important functions are:

\code{\link{initSimulation}}, 
\code{\link{initParameter}}, 
\code{\link{initSpeciesInitalValue}}, 

\code{\link{getParameter}}, 
\code{\link{setParameter}}, 
\code{\link{getSpeciesInitalValue}}, 
\code{\link{setSpeciesInitalValue}}, 
\code{\link{getSimulationTime}}, 
\code{\link{setSimulationTime}}, 

\code{\link{processSimulation}}, 
\code{\link{getSimulationResult}} 
}
\author{
Open Systems Pharmacology Community

Open Systems Pharmacology Community, http://www.open-systems-pharmacology.org/

}
\references{
}
\keyword{ package }
\seealso{
}
\examples{
# this example initializes a simulations, processes it and plots the profile of ID 135.
# adapt working directory and filename of simulation to your specific situation.
dci_info <- initSimulation(XML = file.path(getwd(), "mysimFile.xml"), 
						   whichInitParam = "none")
dci_info <- processSimulation(DCI_Info = dci_info)
results <- getSimulationResult(path_id = 135, 
							   DCI_Info = dci_info)
plot(results, type="l")
}
