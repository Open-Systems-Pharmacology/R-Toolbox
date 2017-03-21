\name{getPKParameterForConcentration}
\alias{getPKParameterForConcentration}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gets PK parameters of a time profile

}
\description{
This function retrieves PK parameters such as AUC, Cmax etc. from a time - concentration profile.
}
\usage{
getPKParameterForConcentration(time, concentration, infusionTime = 0, timeRange = NA, extrapolationRange = 0.1, methode = "linLog", LLOQ = 0)
}
\arguments{
  \item{time}{
a vector of time points
}
  \item{concentration}{
a vector of concentrations simulated or observed at the particular time points

}
 \item{timeRange}{
specifies the time range, where
            the calculation is done:
                first data point: first point >=value_timeRange(1)
                last data point: last point <=value_timeRange(2)
            default (range of the given time vector).
            Affects all PK parameters
}
  
}
\details{
%%  ~~ If necessary, more details than the description above ~~

}
\value{
Returns a list with PK paramters Cmax, Tmax, AUC_tend, AUC_inf, t_half and MRT
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

\examples{
# gets Path information for parameter with ID 113.
valueInfo <- getPKParameterForConcentration(1:10, exp(-(1:10)))
						  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
