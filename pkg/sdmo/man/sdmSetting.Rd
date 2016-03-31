\name{sdmSetting}

\alias{sdmSetting}
\alias{sdmSetting<-}
\alias{glmSetting}
\alias{sdmSetting,missing,speciesData,character-method}
\alias{sdmSetting,formula,speciesData,character-method}
\alias{sdmSetting<-,ANY-method}
\alias{glmSetting,ANY-method}

\title{creating sdm Data object}

\description{
Creates sdmData objects including species (single or multiple) and explanatory variates
}

\usage{
sdmSetting(formula,data,methods,...)
glmSetting(family,...)
\S4method{sdmSetting}{ANY}(x)<-value
}

\arguments{
  \item{formula}{specify the structure of the model}
  \item{data}{sdm data object or data.frame including species and feature data}
  \item{methods}{character, name of the algorithms}
  \item{family}{family setting for glm method}
   \item{x}{sdmSettings objects}
    \item{value}{setting object for different methods}
  \item{...}{additional arguments}
  }
\details{
using sdmSetting, the feature types, interaction.depth and all settings of the model can be defined.
}

\value{
 an object of class \code{sdmSettings}
}

\references{
Naimi, B., Araújo, M.B. (2016) sdm: a reproducible and extensible R platform for species distribution modelling, Ecography, DOI: 10.1111/ecog.01881
}

\author{Babak Naimi \email{naimi.b@gmail.com}

\url{http://biogeoinformatics.org}
}

\seealso{ #}

\examples{
#
}

\keyword{spatial,species,data}
