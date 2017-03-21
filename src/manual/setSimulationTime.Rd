\name{setSimulationTime}
\alias{setSimulationTime}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sets the Simulation Time
}
\description{
This function defines simulation timepoints.
}
\usage{
setSimulationTime(timepoints = {}, DCI_Info = {})
}
\arguments{
  \item{timepoints}{
Vector of timepoints given in minutes.
}
  \item{DCI_Info}{
The DCI Info structure containing the DCI handle and all settings.
}
}
\details{
}
\value{
The DCI Info structure containing the DCI handle and all settings with the newly setted timepoints.
}
\references{
}
\author{
Open Systems Pharmacology Suite Community, https://github.com/Open-Systems-Pharmacology/Suite
}
\note{
The timepoint vector is internaly splitted into equidistant sections. Each section is described by a pattern.
}


\seealso{
\code{\link{getSimulationTime}}
}
\examples{
# sets the simulation time from 0 to 1500 minutes in 15 minutes steps
dci_info <- setSimulationTime(timepoints = seq(0, 1500, length = 101), 
							  DCI_Info = dci_info)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
