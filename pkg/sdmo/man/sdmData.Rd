\name{sdmData}

\alias{sdmData}
\alias{sdmData,data.frame,missing,formula-method}
\alias{sdmData,data.frame,missing,missing-method}
\alias{sdmData,data.frame,missing,formula-method}
\alias{sdmData,SpatialPoints,Raster,ANY-method}
\alias{sdmData,SpatialPointsDataFrame,missing,ANY-method}
\alias{sdmData,SpatialPointsDataFrame,Raster,ANY-method}

\title{creating sdm Data object}

\description{
Creates sdmData objects including species (single or multiple) and explanatory variates
}

\usage{
sdmData(train, predictors, formula, test, filename, crs)
}

\arguments{
  \item{train}{training data including species observations as a (\code{data.frame} or \code{SpatialPoints} or \code{SpatialPointsDataFrames})}
  \item{predictors}{ explanatory variables (predictors), defined as a raster object (\code{RasterStack} or \code{RasterBrick}).}
  \item{formula}{optional, species and different types of explanatory variables (e.g., factor) as well as coordinates can be specified}
  \item{test}{independent test data}
  \item{filename}{filename of the sdm data object to store in the disk (not implemented) }
  \item{crs}{optional, coordinate reference system}
  }
\details{
sdmData can be used to create data object, for single or multiple species data and returns a singleSpecies or multipleSpecis data object, respectively or a SpeciesDataList when ths multiple species the coordinates of species observations are not the sama.
sdmData can automatically detect the variables contain species data (if train is data.frame), but in formula, the species names can be specifies in the left hand side. In the right hand side of the formula the coordinate columns (if train is data.frame), explanatory variables that should be used as well as the factors can be specified (see examples).

}

\value{
 an object of class \code{singleSpecies} or \code{multipleSpecies} or \code{SpeciesDataList}
}

\references{#
}

\author{Babak Naimi \email{naimi.b@gmail.com}

\url{http://biogeoinformatics.org}
}

\seealso{ #}

\examples{
#
}

\keyword{spatial,species,data}
