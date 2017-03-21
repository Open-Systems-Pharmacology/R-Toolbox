\name{existsSpeciesInitialValue}
\alias{existsSpeciesInitialValue}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Checks Existence of Species
}
\description{
This function checks whether a species identified by provided information exists within the model.
}
\usage{
existsSpeciesInitialValue(path_id = "*", options = {}, DCI_Info = {})
}
\arguments{
  \item{path_id}{
The path_id can be "*" (Default) or a string or a numerical vector. 
If a string is provided the species which pathes do match the string are checked. A wildcard ("*") can be used at any place.
If a numerical vector is provided all species which id is in the vector are checked.
}
  \item{options}{
Following options are supported: 

Type ("variable" or "readonly"), Default is "variable".

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
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# checks the existence of a species with ID 134.
existenceInfo <- existsSpeciesInitialValue(path_id = 134, 
										   options = list(Type = "readonly"), 
										   DCI_Info = dci_info)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
