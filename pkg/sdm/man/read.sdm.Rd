\name{read.sdm}
\alias{read.sdm}


\alias{read.sdm,character-method}


\title{Read sdmModel from a file}

\description{
Read an sdm object from a file.
}

\usage{
read.sdm(filename,...)
}

\arguments{
  \item{filename}{ Filename (character)}
  \item{...}{additional arguments}
  }
\details{
This function reads a sdmModel object which has been writen by \code{\link[sdm]{write.sdm}}. Instead of \code{read.sdm}, the \code{sdm} function can be used (usage: \code{sdm(filename)}). 
}

\value{
\code{sdmModel}
}

\author{Babak Naimi

\email{naimi@r-gis.net}

\url{http://biogeoinformatics.org}
}

\seealso{ \code{\link[sdm]{write.sdm}, \link[sdm]{sdm}}}

\examples{
#
}

\keyword{ write, write sdm}
