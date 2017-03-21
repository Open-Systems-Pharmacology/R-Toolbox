\name{getParameter}
\alias{getParameter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gets Information about Parameter(s)

}
\description{
This function retrieves information from the model about parameters identified by provided information.
}
\usage{
getParameter(path_id = "*", options = {}, DCI_Info = {})
}
\arguments{
  \item{path_id}{
The path_id can be "*" (Default) or a string or a numerical vector. 
If a string is provided the parameters which pathes do match the string are taken. A wildcard ("*") can be used at any place.
If a numerical vector is provided all parameters which id is in the vector are taken.
If options$Index is set, the path_id is ignored.
}
  \item{options}{
Following options are supported: 

Type ("current", "variable", "readonly" or "reference"), Default is "current".

Property ("Value", "ID", "Path", "Unit", "Formula", "ParameterType"", "IsFormula", "IsTable", "IsValue")), Default is "Value".

Index (numerical vector of indexes), path_id is ignored if options$Index is set.

TimeProfile (numerical vector of time points).

}
  \item{DCI_Info}{
The DCI Info structure containing the DCI handle and all settings.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
If options$Type is "current" the function searches first in the variable parameters and then in the readonly ones.

If the parameter is a table you can specify a time profile and the resulting value will be a vector of the values corresponding to the specified time profile. The values get linearly interpolated from the current values in the parameter table.
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
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{setParameter}}
}
\examples{
# gets Path information for parameter with ID 113.
valueInfo <- getParameter(path_id=113, 
						  options=list(Type="readonly", Property="Path"), 
						  DCI_Info=dci_info)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
