\name{initSimulation}
\alias{initSimulation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Initializes a Simulation
}
\description{
Initializes a simulation of provided model xml file and creates a DCI Info object.
}
\usage{
initSimulation(XML = "", ParamList = numeric(0), whichInitParam = "", further_options = "", Version = "6_0", SimulationNumber = 1)
}
\arguments{
  \item{XML}{
The xml file which defines the MoBi® model. Created by PKSim® or MoBi®.
}
  \item{ParamList}{
List of previously initialized parameters and/or species initial values. These parameters or special initial values can be varied.
}
  \item{whichInitParam}{
Which class of parameters should be initialized ("all","none", "allNonFormula").}
  \item{further_options}{
Some options not needed for normal use.
}
  \item{Version}{
SimModel-Version. Must be "6_0", which is the default.
}
  \item{SimulationNumber}{
Default is 1. If set to 1 all previously initialized simulations gets invalid. To add a simulation to the internal DCI object use a number different to 1.
}
}
\details{
}
\value{
Returns a DCI Info structure containing the DCI handle and all settings.
}
\references{
}
\author{
Open Systems Pharmacology Suite Community, https://github.com/Open-Systems-Pharmacology/Suite
}
\note{
If you are using whichInitParam = "allNonFormula" it could happen that some parameters are defined as formula parameters within the simulation model and during the initialization process the internal model structure optimizer recognized that the value would be allways a constant. Therefor those parameters are handled as value parameters with the constant value as value. To initialize those parameters as variable parameters you have to use a ParamList or whichInitParam = "all".
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{initParameter}}, \code{\link{initSpeciesInitialValue}},  

\code{\link{setParameter}}, \code{\link{setSpeciesInitialValue}}, \code{\link{setSimulationTime}},

\code{\link{processSimulation}}
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
