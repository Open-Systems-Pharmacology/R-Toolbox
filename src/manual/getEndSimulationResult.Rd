\name{getEndSimulationResult}
\alias{getEndSimulationResult}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gets the last value of Simulation Results
}
\description{
This function retrieves the last simulated value of species and/or observers in a list containing the Values and the Paths.
}
\usage{
getEndSimulationResult(path_id = "*", options = {}, DCI_Info = {})
}
\arguments{
  \item{path_id}{
The path_id can be "*" (Default) or a string or a numerical vector. 
If a string is provided the outputs which pathes do match the string are taken. A wildcard ("*") can be used at any place.
If a numerical vector is provided all outputs which id is in the vector are taken.
If options$Index is set, the path_id is ignored.
}
  \item{options}{
Following options are supported: 

Index (numerical vector of indexes), path_id is ignored if options$Index is set.
}
  \item{DCI_Info}{
The DCI Info structure containing the DCI handle and all settings.
}
}
\details{
}
\value{
Returns a list containing a vector "Value" with last simulation values and a vector "Path" with the corresponding paths of the species/observers.
}
\references{
}
\author{
Open Systems Pharmacology Suite Community, https://github.com/Open-Systems-Pharmacology/Suite
}
\note{
The DCI Info object must have been processed before, so that the outputs have been generated.

The indexes you can specify by options$Index concern to the output tables.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{processSimulation}, \link{getSimulationResult}}
}
\examples{
# processes the simulation and get the end result of ID 134.
dci_info <- processSimulation(DCI_Info = dci_info)
endResult <- getEndSimulationResult(path_id=134, DCI_Info = dci_info)$Value
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
