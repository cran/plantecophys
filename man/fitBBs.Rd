% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fitBBs.R
\name{fitBBs}
\alias{fitBBs}
\title{Fit Ball-Berry type models of stomatal conductance to many groups at once}
\usage{
fitBBs(data, group, ...)
}
\arguments{
\item{data}{Input dataframe, containing all variables needed to fit the model.}

\item{group}{Name of the grouping variable in the dataframe (quoted), the model will be fit for each group defined by this variable.}

\item{\dots}{Further parameters passed to \code{\link{fitBB}}, see there for a full description.}
}
\description{
A batch utility for the \code{\link{fitBB}} function, to fit the model for each group in a dataframe.
}
\examples{
\dontrun{
# If you have a factor variable in your dataset called 'species', and you
# want to fit the Ball-Berry model for each of the species:
myfits <- fitBBs(mydata, "species", model="BallBerry")

# A dataframe with coefficients is returned by coef()
coef(myfits)

}
}
