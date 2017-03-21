\name{saveSimulationToXML}
\alias{saveSimulationToXML}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Saves the Simulation
}
\description{
This functions saves the current DCI Info object to an xml file for later use.
}
\usage{
saveSimulationToXML(XML = "", DCI_Info={})
}
\arguments{
  \item{XML}{
The path and file name to the simulation should be saved to. 
}
  \item{DCI_Info}{
The DCI Info structure containing the DCI handle and all settings.
}
}
\details{
}
\value{
Returns the path and file name of the newly created xml file.
}
\references{
}
\author{
Open Systems Pharmacology Suite Community, https://github.com/Open-Systems-Pharmacology/Suite
}
\note{
If the xml parameter is missing the simulation will be saved in the current working directory and is named "currentSimulation.xml". If the specified folder does not exists, the folder is created and a warning is produced.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{initSimulation}}
}
\examples{
# creates a new folder "SavedSimulations" in the current working directory 
# and saves the simulation as "mysim.xml".
savedSimFile <- saveSimulationToXML(XML = file.path(getwd(), 
													"SavedSimulations", 
													"mysim.xml"), 
									DCI_Info = dci_info)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
