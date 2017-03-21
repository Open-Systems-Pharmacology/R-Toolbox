\name{getSimulationTime}
\alias{getSimulationTime}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gets Simulation Time Settings
}
\description{
This function retrieves the currently defined simulation time settings.
}
\usage{
getSimulationTime(DCI_Info = {})
}
\arguments{
  \item{DCI_Info}{
The DCI Info structure containing the DCI handle and all settings.
}
}
\details{
}
\value{
Returns a structure with information about the current time settings.
}
\references{
}
\author{
Open Systems Pharmacology Suite Community, https://github.com/Open-Systems-Pharmacology/Suite
}
\note{
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{setSimulationTime}}
}
\examples{
# gets the simulation time information
simTime <- getSimulationTime(DCI_Info = dci_info)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
