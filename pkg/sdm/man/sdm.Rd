\name{sdm}

\alias{sdm}
\alias{sdm,formula,singleSpecies,character-method}
\alias{sdm,ANY,singleSpecies,sdmSettings-method}

\title{fit sdm models}

\description{
Fits different models using specified methods, and evaluate the outputs if an independent test data is specified and/or partitioning scenarios are specified.
}

\usage{
sdm(formula, data, methods,...)
}

\arguments{
  \item{formula}{optional, specifies the structure of the model, types of features, etc.}
  \item{data}{can be any sdm data object or data.frame or SpatialPointsDataFrames.}
  \item{methods}{specifies the modelling methods, if missing, all available methods are used}
  \item{...}{additional arguments}
  }
\details{
sdm fits multiple models and can be used to for multiple runs of each method when one or several partitioning methods are used.
}

\value{
 an object of class \code{sdmModel}
}

\references{#
}

\author{Babak Naimi \email{naimi.b@gmail.com}

\url{http://biogeoinformatics.org}
}

\seealso{# }

\examples{
#
}

\keyword{spatial,species,sdm, model}
