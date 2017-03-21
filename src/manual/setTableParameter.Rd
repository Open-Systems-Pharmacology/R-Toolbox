\name{setTableParameter}
\alias{setTableParameter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sets Time Profiles for Table Parameter(s)
}
\description{
This function sets time profiles for one or more table parameters.
}
\usage{
setTableParameter(table = {}, options = {}, DCI_Info = {})
}
\arguments{
  \item{table}{
List with time profile information. Return value of getTableParameter can be used as input.
}

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
The DCI Info structure containing the DCI handle and all settings with the newly setted table parameters.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Open Systems Pharmacology Suite Community, https://github.com/Open-Systems-Pharmacology/Suite
}
\note{
Only parameters which ID is defined within the given table are set. That means if there are table parameters which are not within the given table, those table parameters are not effected. For the others the data is replaced by the new one.

If IDs of parameters are used in the given table which are no table parameters, an error is thrown.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{getTableParameter}}
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
