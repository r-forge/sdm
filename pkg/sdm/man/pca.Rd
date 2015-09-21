\name{pca}

\alias{pca}
\alias{pca,singleSpecies-method}
\alias{pca,multipleSpecies-method}
\alias{pca,RasterStackBrick-method}

  
\title{Principle Component Analysis}

\description{
pca performs a principal components analysis (using \code{princomp} function from \code{stats} package) on the given numeric data matrix and returns the results as an object of class princomp.
}

\usage{
\S4method{pca}{singleSpecies}(x,...)

\S4method{pca}{multipleSpecies}(x,...)

\S4method{pca}{RasterStackBrick}(x,...)

}

\arguments{
  \item{x}{speciesData (either of singleSpecies or multiple Species) or a Raster object}
  \item{...}{additional arguments pass to \code{princomp} function}
}
\details{
pca analysis can be considered as a way to deal with multicollinearity problem and reduction of the data dimention. It returns two items in a list, data, and pca. The data contains the transoformed data into priciple components (the number of components is the same as the number of variable in the input data). You can check the pca item to see how many components (e.g., first 3) should be selected (e.g., by checking the loading table). For more information on the calculation, see the \code{\link[stats]{princomp}} function. 
}

\value{
a list including data (a data.frame or a RasterStack depending on the type of x), and pca results (output of the princomp function)
}


\author{Babak Naimi \email{naimi.b@gmail.com}

\url{http://r-gis.net}

\url{http://biogeoinformatics.org}

}


\examples{
filename <- system.file('external/preds.grd',packages='sdm')

r <- brick(filename)

p <- pca(r) # p is a list with two items

plot(p$pca)

biplot(p$pca)

plot(p$data)
}

\keyword{spatial}
