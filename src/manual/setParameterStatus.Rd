\name{setParameterStatus}
\alias{setParameterStatus}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sets the Values of all Variables
}
\description{
This function sets all values of variables (parameters, species initial values, species scale factors and simulation timepoints) back to the previously retrieved ones.
}
\usage{
setParameterStatus(ParameterStatus = {}, DCI_Info = {})
}
\arguments{
  \item{ParameterStatus}{
The structure containing value information previously created by \code{\link{getParameterStatus}}.
}
  \item{DCI_Info}{
The DCI Info structure containing the DCI handle and all settings.
}
}
\details{
}
\value{
The DCI Info structure containing the DCI handle and all updated settings.
}
\references{
}
\author{
Open Systems Pharmacology Suite Community, https://github.com/Open-Systems-Pharmacology/Suite
}
\note{
}
\seealso{
\code{\link{getParameterStatus}}
}
\examples{
# saves parameter status.
parameterStatus <- getParameterStatus(DCI_Info = dci_info)

# do some changes and work with your simulation
# ...

# set all back to saved status.
dci_info <- setParameterStatus(ParameterStatus = parameterStatus, 
							   DCI_Info = dci_info)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
