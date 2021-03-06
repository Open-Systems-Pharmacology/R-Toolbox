\name{setRelativeParameter}
\alias{setRelativeParameter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sets Value(s) for Parameter(s) relatively
}
\description{
This function sets one or more parameters specified by provided information to provided value multiplied by source value.
}
\usage{
setRelativeParameter(value = numeric(0), path_id = "*", options = {}, DCI_Info = {})
}
\arguments{
  \item{value}{
Vector of new values to be set for all parameters specified by path_id or options$Index. Must be numeric.
}
  \item{path_id}{
The path_id can be "*" (Default) or a string or a numerical vector. 
If a string is provided the parameters which pathes do match the string are set. A wildcard ("*") can be used at any place.
If a numerical vector is provided all parameters which id is in the vector are set.
If options$Index is set, the path_id is ignored.
}
  \item{options}{
Following options are supported: 

Type ("variable", "reference"). Default is "variable". Specifies the target of the setting. The source is the other one.

Property ("Value").

Index (numerical vector of indexes), path_id is ignored if options$Index is set.
}
  \item{DCI_Info}{
The DCI Info structure containing the DCI handle and all settings.
}
}
\details{
}
\value{
The DCI Info structure containing the DCI handle and all settings with the newly setted parameters.
}
\references{
}
\author{
Open Systems Pharmacology Suite Community, https://github.com/Open-Systems-Pharmacology/Suite
}
\note{
If there are also table parameters specified by path_id, a warning is generated and those parameters will be ignored.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{setParameter}}
}
\examples{
# sets variable parameters with ID 113 to doubled value of the reference.
dci_info <- setRelativeParameter(value = 2, 
								 path_id = 113, 
								 options = list(Type = "variable"), 
								 DCI_Info = dci_info)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
