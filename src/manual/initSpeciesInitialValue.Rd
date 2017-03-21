\name{initSpeciesInitialValue}
\alias{initSpeciesInitialValue}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Initializes a Species
}
\description{
This functions adds a new species to the initialization structure.
}
\usage{
initSpeciesInitialValue(initStruct = list(), path_id = "", initializeIfFormula = "withWarning")
}
\arguments{
  \item{initStruct}{
Current initialization structure with previously initialized parameters and/or species.
}
  \item{path_id}{
The path_id can be a string (Path) or a number (ID). 
}
  \item{initializeIfFormula}{
Should the parameter be initialized if it contains a formula? ("never", "always", "withWarning"), Default is "withWarning".
}
}
\details{
}
\value{
Structure of all initialized parameters and species.
}
\references{
}
\author{
Open Systems Pharmacology Suite Community, https://github.com/Open-Systems-Pharmacology/Suite
}
\note{
If the path_id is provided by a string all species which Path is matching are initialized.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{initSimulation}}, \code{\link{initParameter}}
}
\examples{
# builds a new initialization structure and inits the simulation
initStruct <- list()
initStruct <- initParameter(path_id = 113, 
							initializeIfFormula = "always", 
							initStruct = initStruct)
initStruct <- initSpeciesInitialValue(path_id = 134, 
									  initializeIfFormula = "withWarning", 
									  initStruct = initStruct)
initStruct <- initSpeciesInitialValue(path_id = 141, 
									  initializeIfFormula = "never", 
									  initStruct = initStruct)
dci_info <- initSimulation(XML = "mySim.xml", 
						   ParamList = initStruct) 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
