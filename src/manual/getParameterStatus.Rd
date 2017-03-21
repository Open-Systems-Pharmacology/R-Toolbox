\name{getParameterStatus}
\alias{getParameterStatus}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gets the current Status of all Variables
}
\description{
This function retrieves all current values of variables (parameters, table parameters, species initial values, species scale factors, simulation timepoints).
}
\usage{
getParameterStatus(options = {}, DCI_Info = {})
}
\arguments{
  \item{options}{
Following options are supported: 

Type ("variable", "reference"). Default is "variable".
}
  \item{DCI_Info}{
The DCI Info structure containing the DCI handle and all settings.
}
}
\details{
}
\value{
Retrieves a structure containing following information: 
\item{ParameterType}{Type used in options$Type. Indication the source of the values.}
\item{Parameters}{Values of variable parameters. Taken from input or reference.}
\item{TableParameters}{Tables of variable table parameters. Taken from input or reference.}
\item{SpeciesInitialValues}{Values of species initial values. Taken from input or reference.}
\item{SpeciesScaleFactors}{Values of species scale factors. Taken from input or reference.}
\item{SimulationTime}{Vector of timepoints.}
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
\code{\link{setParameterStatus}}
}
\examples{
# saves parameter status.
parameterStatus <- getParameterStatus(DCI_Info = dci_info)

# do some changes and work with your simulation
# ...

# sets all back to saved status.
dci_info <- setParameterStatus(ParameterStatus = parameterStatus, 
							   DCI_Info = dci_info)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
