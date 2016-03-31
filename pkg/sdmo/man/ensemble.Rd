\name{ensemble}


\docType{methods}

\alias{ensemble}
\alias{ensemble,sdmModel-method}

\title{Ensemble Forecasting of SDMs}

\description{
Make a Raster object with a weighted averaging over all predictions from several fitted model in a sdmModel object.
}
\usage{
\S4method{ensemble}{sdmModel}(m, predictors, filename="")
}


\arguments{
  \item{m}{sdmModel object}
  \item{predictors}{Raster* object}
  \item{filename}{character, output file name}
  
  }
\details{
ensemble function uses the fitted models in the sdmModel to generate the prediction given predictor variables. A raster will be created.

}

\value{
 a Raster object
}

\references{
#
}

\author{Babak Naimi \email{naimi.b@gmail.com}

\url{http://biogeoinformatics.org}
}

\seealso{# }

\examples{
# filename <- system.file('external/preds.grd',packages='sdmo')

# r <- brick(filename)
# s <- shapefile('pkg/sdm/inst/external/species.shp')
# d <- sdmData(s,r)
# m <- sdm(Occurrence~.,d,c('glm','brt','rf'))
# e <- ensemble(m,r,'test.grd')
# plot(e)

}

\keyword{spatial,species,data}
