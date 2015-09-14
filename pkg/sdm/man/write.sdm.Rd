\name{write.sdm }
\alias{write.sdm}

\alias{write.sdm,sdmModel,character-method}
\alias{write.sdm,sdmData,character-method}
\alias{write.sdm,sdmSettings,character-method}



\title{Write an sdm Object to a file}

\description{
Write an entire (\code{sdm*}) object to a file.
}

\usage{
write.sdm(object, filename, overwrite=FALSE, ...) 
}

\arguments{
  \item{object}{an sdm (\code{sdm*}) object}
  \item{filename}{Output filename}
  \item{overwrite}{Logical. If \code{TRUE}, "filename" will be overwritten if it exists }
  \item{...}{Additional arguments as for \code{\link[raster]{writeRaster}} }
 
}
\details{
This function writes an sdm object into a directory which named as is specified in the \code{filename} argument. To write the raster data, \code{\link[raster]{writeRaster}} in the package \pkg{raster} is used.
}

\value{
This function is used for writing values to a series of files.
}

\author{Babak Naimi
\email{naimi.b@gmail.com}
\url{http://biogeoinformatics.org}
}

\seealso{ \code{\link[sdm]{read.sdm}}}

\examples{
#
}

\keyword{ write, write sdm}
