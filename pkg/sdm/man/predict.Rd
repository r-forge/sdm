\name{predict}


\docType{methods}

\alias{predict}
\alias{predict,sdmModel-method}

\title{sdm model prediction}

\description{
Make a Raster object with predictions from one or several fitted model in sdmModel object. 
}
\usage{
\S4method{predict}{sdmModel}(object, newdata, filename="", models,species=1,scenario,mean=FALSE,overwrite=TRUE, ...)
}


\arguments{
  \item{object}{sdmModel object}
  \item{newdata}{Raster* object, or data.frame}
  \item{filename}{character, output file name, if missing, a name starts with sdm_prediction will be generated}
  \item{models}{character, names of fitted models, e.g., glm, brt, etc.}
  \item{species}{character or numeric, specifies which species should be used if the object contains models for multiple species}
  \item{scenario}{character, specifies the names of partitioning scenarios,if missing, all available scenarios are considered}
  \item{mean}{logical, works if multiple runs for partitioning scenarios are used to fit the models, and specifies whether a mean should be calculated over all predictions}
  \item{overwrite}{logical, whether the filename should be overwriten it it does exist}
  \item{...}{ additional arguments, as for writeRaster}
  }
\details{
predict uses the fitted models in the sdmModel to generate the prediction given newdata. A raster (if newdata is Raster object) or data.frame (if newdata is data.frame) will be created.

}

\value{
 a Raster object or data.frame
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

\keyword{spatial,species,data}
