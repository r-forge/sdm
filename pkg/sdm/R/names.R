# Author: Babak Naimi, naimi.b@gmail.com
# Date :  June 2014
# Version 1.0
# Licence GPL v3


if (!isGeneric("names")) {
  setGeneric("names", function(x)
    standardGeneric("names"))
}

setMethod('names', signature(x='speciesData'), 
          function(x) {
            if (inherits(x,'singleSpecies')) {
              n <- x@train@Occurrence@species.name
            } else if (inherits(x,'multipleSpecies')) {
              n <- x@train@Occurrence@species.names
            } else if (inherits(x,'SpeciesDataList')) {
              n <- rep(NA,length(x@train@SpeciesDataList))
              for (i in seq_along(x@train@SpeciesDataList)) {
                n[i] <- x@train@SpeciesDataList[[i]]@Occurrence@species.name
              }
            }
            n
          }
)

if (!isGeneric("names<-")) {
  setGeneric("names<-", function(x,value)
    standardGeneric("names<-"))
}


setReplaceMethod('names', signature(x='speciesData','character'), 
                 function(x,value) {
                   if (inherits(x,'singleSpecies')) {
                     x@train@Occurrence@species.name <- value[1]
                   } else if (inherits(x,'multipleSpecies')) {
                     if (length(x@train@Occurrence@species.names) != length(value)) stop('the length of names is not equal to the number of species...!')
                     x@train@Occurrence@species.names <- value
                   } else if (inherits(x,'SpeciesDataList')) {
                     if (length(x@train@SpeciesDataList) != length(value)) stop('the length of names is not equal to the number of species...!')
                     names(x@train@SpeciesDataList) <- value
                     for (i in seq_along(x@train@SpeciesDataList)) {
                       x@train@SpeciesDataList[[i]]@Occurrence@species.name <- value[i]
                     }
                   }
                   x
                 }
)