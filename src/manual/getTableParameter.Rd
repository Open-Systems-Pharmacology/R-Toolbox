\name{getTableParameter}
\alias{getTableParameter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gets Information about Parameter(s)

}
\description{
This function retrieves the time profile table from the model about table parameters identified by provided information.
}
\usage{
getTableParameter(path_id = "*", options = {}, DCI_Info = {})
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

Index (numerical vector of indexes), path_id is ignored if options$Index is set.

}
  \item{DCI_Info}{
The DCI Info structure containing the DCI handle and all settings.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
If options$Type is "current" the function searches first in the variable table parameters and then in the readonly ones.

}
\value{
Returns a list with following information:

\item{ID}{ID of parameter.}

\item{Time}{Time point.}

\item{Value}{Value at this time point.}

\item{RestartResolver}{If the value differs to 0 the resolver gets restarted at this time point.}
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
# sets the time profiles of parameter with ID 5837.
timeProfiles <- getTableParameter(path_id=5837, 
								  options=list(Type="variable"), 
								  DCI_Info = dci_info)
#...modify timeProfiles
dci_info <- setTableParameter(table = timeProfiles, 
							  options=list(Type="variable"), 
							  DCI_Info = dci_info)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
