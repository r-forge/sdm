\name{sdm}

\alias{sdm}
\alias{sdm,formula,sdmdata,character-method}
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

\references{
Naimi, B., Araújo, M.B. (2016) sdm: a reproducible and extensible R platform for species distribution modelling, Ecography, DOI: 10.1111/ecog.01881
}

\author{Babak Naimi \email{naimi.b@gmail.com}

\url{http://biogeoinformatics.org}
}

\seealso{# }

\examples{
\dontrun{
file <- system.file("external/pa_df.csv", package="sdm")
df <- read.csv(file)
head(df) 

d <- sdmData(sp~b15+NDVI,train=df)

d
#----
# Example 1: fit using 3 models, and no evaluation (evaluation based on 
# training dataset)

m <- sdm(sp~b15+NDVI,data=d,methods=c('glm','gam','gbm'))

m

# Example 3: fit using 5 models, and 
# evaluate based on subsampling replication method with 
# 30 percent as test dataset and 10 replicated (repeats)

m <- sdm(sp~b15+NDVI,data=d,methods=c('glm','gam','gbm','svm','rf'),replication='sub',test.percent=30,n=10)

m


# Example 3: fit using 5 models, and 
# evaluate based on 5-folds cross-validation and bootsrapping replication 
# methods with 10 runs

m <- sdm(sp~.,data=d,methods=c('gbm','tree','mars','mda','fda'),replication=c('cv','boot'),cv.folds=5,n=10)

m

}

}

\keyword{spatial,species,sdm, model}
