\name{sdm-classes}

\docType{class}

\alias{sdmModel-class}
\alias{sdmEvaluate-class}
\alias{singleSpecies-class}
\alias{multipleSpecies-class}
\alias{SpeciesDataList-class}
\alias{sdmEvaluate-class}
\alias{sdmEvaluates-class}
\alias{sdmSettings-class}

\alias{show,sdmModel-method}
\alias{show,singleSpecies-method}
\alias{show,multipleSpecies-method}
\alias{show,SpeciesDataList-method}

\title{sdm classes}

\description{

sdm package defined different classes to handle data, models and their results.

}

\section{Slots}{
Slots for sdmData objects:
	\describe{
    \item{\code{train}:}{object of class \code{.singleSpecies} or \code{.multipleSpecies}} 
    \item{\code{test}:}{same as train}
  }
}


\author{Babak Naimi 

\email{naimi@r-gis.net}

\url{http://biogeoinformatics.org}
}

\examples{
showClass("sdmModel")
}

\keyword{classes}
\keyword{spatial}
