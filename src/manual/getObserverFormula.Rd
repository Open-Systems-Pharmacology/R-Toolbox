\name{getObserverFormula}
\alias{getObserverFormula}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gets Information about Observer(s)
}
\description{
This function retrieves information from the model about observers identified by provided information.
}
\usage{
getObserverFormula(path_id = "*", options = {}, DCI_Info = {})
}
\arguments{
  \item{path_id}{
The path_id can be "*" (Default) or a string or a numerical vector. 
If a string is provided the observers which pathes do match the string are taken. A wildcard ("*") can be used at any place.
If a numerical vector is provided all observers which id is in the vector are taken.
If options$Index is set, the path_id is ignored.
}
  \item{options}{
Following options are supported: 

isReference (TRUE or FALSE), Default is FALSE.

Property ("ID", "Unit", "Formula", "Path"), Default is "Formula".

Index (numerical vector of indexes), path_id is ignored if options$Index is set.

}
  \item{DCI_Info}{
The DCI Info structure containing the DCI handle and all settings.
}
}
\details{
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# gets Path information for observer with ID 147.
valueInfo <- getObserverFormula(path_id = 147, 
								options = list(Property = "Path"), 
								DCI_Info = dci_info)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
