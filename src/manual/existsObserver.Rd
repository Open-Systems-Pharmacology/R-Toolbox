\name{existsObserver}
\alias{existsObserver}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Checks Existence of Observer(s)
}
\description{
This function checks whether an observer identified by provided information exists within the model.
}
\usage{
existsObserver(path_id = "*", options = {}, DCI_Info = {})
}
\arguments{
  \item{path_id}{
The path_id can be "*" (Default) or a string or a numerical vector. 
If a string is provided the observers which pathes do match the string are checked. A wildcard ("*") can be used at any place.
If a numerical vector is provided all observers which id is in the vector are checked.

}
  \item{options}{
Following options are supported: 

isReference (TRUE or FALSE), Default is FALSE.
}

  \item{DCI_Info}{
The DCI Info structure containing the DCI handle and all settings.

}
}
\details{
}
\value{
Returns a list with following information:

\item{isExisting}{TRUE or FALSE}

\item{Index}{vector of indexes}

\item{ID}{vector of IDs}

\item{Path}{vector of Pathes}
}
\references{
}
\author{
Open Systems Pharmacology Suite Community, https://github.com/Open-Systems-Pharmacology/Suite
}
\note{
}

\seealso{
}
\examples{
# checks the existence of observer with ID 147.
existenceInfo <- existsObserver(path_id = 147, 
								DCI_Info = dci_info)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
