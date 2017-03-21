\name{getSpeciesInitialValue}
\alias{getSpeciesInitialValue}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gets Information about Species
}
\description{
This function retrieves information from the model about species identified by provided information.
}
\usage{
getSpeciesInitialValue(path_id = "*", options = {}, DCI_Info = {})
}
\arguments{
  \item{path_id}{
The path_id can be "*" (Default) or a string or a numerical vector. 
If a string is provided the species which pathes do match the string are taken. A wildcard ("*") can be used at any place.
If a numerical vector is provided all species which id is in the vector are taken.
If options$Index is set, the path_id is ignored.
}
  \item{options}{
Following options are supported: 

Type ("current", "variable", "readonly" or "reference"), Default is "current".

Property ("InitialValue", "ID", "Unit", "Formula", "IsFormula", "ScaleFactor", "Path"), Default is "Initial Value".

Index (numerical vector of indexes), path_id is ignored if options$Index is set.

}
  \item{DCI_Info}{
The DCI Info structure containing the DCI handle and all settings.
}
}
\details{
If options$Type is "current" the function searches first in the variable species initial values and then in the readonly ones.
}
\value{
Returns a list with following information:

\item{Value}{Value of provided property.}

\item{Index}{Vector with indexes.}

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
\code{\link{setSpeciesInitialValue}}
}
\examples{
# gets Path information for species with ID 134.
valueInfo <- getSpeciesInitialValue(path_id = 134, 
									options = list(Type = "readonly", Property = "Path"), 
									DCI_Info = dci_info)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
