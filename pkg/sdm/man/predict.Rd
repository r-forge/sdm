\name{predict}


\docType{methods}

\alias{predict}
\alias{predict,sdmModels-method}

\title{sdm model prediction}

\description{
Make a Raster or matrix object (depending on input dataset) with predictions from one or several fitted model in sdmModels object.
}
\usage{
\S4method{predict}{sdmModels}(object, newdata, filename="", w=NULL, species=NULL,method=NULL,replication=NULL,run=NULL,mean=FALSE,control=NULL,overwrite=TRUE,nc=1,obj.size=1, ...)
}


\arguments{
  \item{object}{sdmModels object}
  \item{newdata}{Raster* object, or data.frame}
  \item{filename}{character, output file name, if missing, a name starts with sdm_prediction will be generated}
  \item{w}{numeric, specifies which model(s) should be used if the object contains several models; with NULL all models are used}
  \item{species}{character, (optional), specifies which species should be used if the object contains models for multiple species; with NULL all species are used}
  \item{method}{character, names of fitted models, e.g., glm, brt, etc.}
  
  \item{replication}{character, specifies the names of replication method,if NULL, all available replications are considered}
  \item{mean}{logical, works if replication with multiple runs are used to fit the models, and specifies whether a mean should be calculated over all predictions of a replication method}
  \item{overwrite}{logical, whether the filename should be overwriten it it does exist}
  \item{...}{ additional arguments, as for writeRaster}
  }
\details{
predict uses the fitted models in the sdmModel to generate the prediction given newdata. A raster (if newdata is Raster object) or data.frame (if newdata is data.frame) will be created.

}

\value{
 a Raster object or data.frame
}

\references{
Naimi, B., Araújo, M.B. (2016) sdm: a reproducible and extensible R platform for species distribution modelling, Ecography, DOI: 10.1111/ecog.01881
}

\author{Babak Naimi \email{naimi.b@gmail.com}

\url{http://biogeoinformatics.org}
}

\seealso{# }

\examples{
#
}

\keyword{spatial,species,data}
