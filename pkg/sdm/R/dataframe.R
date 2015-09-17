
# Author: Babak Naimi, naimi.b@gmail.com
# Date :  Sep 2015
# Version 1.0
# Licence GPL v3

setMethod('as.data.frame', signature(x='singleSpecies'), 
          function(x,n=1,...) {
            d <- data.frame(x@train@Occurrence@Occurrence,x@train@Features@features)
            colnames(d) <- c(x@train@Occurrence@species.name,x@train@Features@featureNames)
            rownames(d) <- x@train@Occurrence@index@ID
            d
          }
)

setMethod('as.data.frame', signature(x='multipleSpecies'), 
          function(x,n=1,...) {
            d <- data.frame(x@train@Occurrence@Occurrence,x@train@Features@features)
            colnames(d) <- c(x@train@Occurrence@species.names,x@train@Features@featureNames)
            rownames(d) <- x@train@Occurrence@index@ID
            d
          }
)

setMethod('as.data.frame', signature(x='SpeciesDataList'), 
          function(x,n=1,...) {
            d <- data.frame(x@train@SpeciesDataList[[n]]@Occurrence@Occurrence,x@train@SpeciesDataList[[n]]@Features@features)
            colnames(d) <- c(x@train@SpeciesDataList[[n]]@Occurrence@species.name,x@train@SpeciesDataList[[n]]@Features@featureNames)
            rownames(d) <- x@train@SpeciesDataList[[n]]@Occurrence@index@ID
            d
          }
)
