\name{roc}

\alias{roc}
\alias{getRoc}
\alias{roc,sdmModel-method}
\alias{getRoc,vector,vector-method}

\title{plot ROC curves}

\description{
plot the ROC curves
}

\usage{
roc(x,...)
getRoc(o,p,...)
}

\arguments{
  \item{x}{sdmModel object}
    \item{o}{a numeric vector including the observed values}
  \item{p}{a numeric vector including the predicted values}
  \item{...}{additional arguments}
  }
\details{
roc generates the plots of roc curves, and getRoc generate the values of ROC
}

\value{
 an object of class \code{matrix}
}

\references{
#
}

\author{Babak Naimi \email{naimi.b@gmail.com}

\url{http://biogeoinformatics.org}
}

\seealso{# }

\examples{
#
}

\keyword{spatial,species,data}
