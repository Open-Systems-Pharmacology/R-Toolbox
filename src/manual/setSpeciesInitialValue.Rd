\name{setSpeciesInitialValue}
\alias{setSpeciesInitialValue}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sets Value(s) for Species
}
\description{
This function sets one or more species initial value or scale factor specified by provided information to provided value.
}
\usage{
setSpeciesInitialValue(value = numeric(0), path_id = "*", options = {}, DCI_Info = {})
}
\arguments{
  \item{value}{
Vector of new values to be set for all species specified by path_id or options$Index. Must be numeric.
}
  \item{path_id}{
The path_id can be "*" (Default) or a string or a numerical vector. 
If a string is provided the species which pathes do match the string are set. A wildcard ("*") can be used at any place. 
If a numerical vector is provided all species which ids are within the vector are set.
If options$Index is set, the path_id is ignored.
}
  \item{options}{
%%     ~~Describe \code{options} here~~
Following options are supported: 

Type ("variable", "reference"). Default is "variable".

Property ("InitialValue", "ScaleFactor"). Default is "InitialValue".

Index (numerical vector of indexes), path_id is ignored if options$Index is set.
}
  \item{DCI_Info}{
The DCI Info structure containing the DCI handle and all settings.
}
}
\details{
}
\value{
The DCI Info structure containing the DCI handle and all settings with the newly setted species initial values.
}
\references{
}
\author{
Open Systems Pharmacology Suite Community, https://github.com/Open-Systems-Pharmacology/Suite
}
\note{
}


\seealso{
\code{\link{setRelativeSpeciesInitialValue}}
}
\examples{
# sets species initial value of species with ID 134 to value 2.
dci_info <- setSpeciesInitialValue(value = 2, 
								   path_id = 134, 
								   options = list(Type = "variable"), 
								   DCI_Info = dci_info)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
