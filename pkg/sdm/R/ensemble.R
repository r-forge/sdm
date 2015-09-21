# Author: Babak Naimi, naimi.b@gmail.com
# Date :  September 2015
# Version 1.0
# Licence GPL v3

if (!isGeneric("ensemble")) {
  setGeneric("ensemble", function(m,predictors,filename)
    standardGeneric("ensemble"))
}	



setMethod('ensemble', signature(m='sdmModel'), 
          function(m, predictors, filename) {
            nm <- names(m@models@multiModelList[[1]]@modelList)
            w <- rep(NA,length(nm))
            names(w) <- nm
            
            for (mo in nm) {
              w[mo] <- mean(unlist(lapply(m@models@multiModelList[[1]]@modelList[[mo]]@evaluation@evaluationList,function(x) x@AUC)),na.rm=TRUE)
            }
            f <- function(x,...) {
              sum(w * x)
            }
            w <- w / sum(w)
            p <- predict(m,predictors,mean=TRUE,filename=filename)
            calc(p,f,filename=filename,overwrite=TRUE)
          }
)

