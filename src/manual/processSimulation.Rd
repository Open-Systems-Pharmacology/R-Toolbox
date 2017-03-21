\name{processSimulation}
\alias{processSimulation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Processes a Simulation
}
\description{
This function processes a simulation and adds the calculated outputs to the DCI Info object.
}
\usage{
processSimulation(DCI_Info = {})
}
\arguments{
  \item{DCI_Info}{
The DCI Info structure containing the DCI handle and all settings.
}
}
\details{
}
\value{
Returns the DCI Info structure containing the DCI handle and all settings and the newly added outputs.
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
\code{\link{getSimulationResult}}
}
\examples{
# processes the simulation and plots the result of ID 134.
dci_info <- processSimulation(DCI_Info = dci_info)
results <- getSimulationResult(path_id=134, 
							   DCI_Info = dci_info)
plot(results)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
