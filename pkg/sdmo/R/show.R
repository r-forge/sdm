# Author: Babak Naimi, naimi.b@gmail.com
# Date :  June 2014
# Version 1.0
# Licence GPL v3

setMethod ('show' , 'singleSpecies',
           function ( object ) {
             cat('class                                 :' , class(object), '\n')
             cat('===========================================================','\n')
             cat('species name                          : ' , object@train@Occurrence@species.name , '\n')
             cat('number of fearures                    : ' , if (!is.null(object@train@Features)) length(object@train@Features@featureNames) else NA, '\n')
             cat('fearure names                         : ' , if (!is.null(object@train@Features)) {
               if (length(object@train@Features@featureNames) > 3) paste(c(object@train@Features@featureNames[1:3],'...'),collapse=', ') else paste(object@train@Features@featureNames,collapse=', ')
             } else NA, '\n')
             cat('type                                  : ' , if (object@train@Occurrence@presence.only) 'presence-only' else 'presence-absence', '\n')
             cat('number of observations (train)        : ', length(object@train@Occurrence@Occurrence),'\n')
             cat('has independet test data?             : ' , !is.null(object@test), '\n')
             if (!is.null(object@test))
               cat('number of observations (test)         : ', length(object@test@Occurrence@Occurrence),'\n' )
             cat('has Coordinates?                      : ' , !is.null(object@train@coordinates) , '\n')
           }
)

setMethod ('show' , 'multipleSpecies',
           function ( object ) {
             cat('class                                     :' , class(object), '\n')
             cat('===========================================================','\n')
             cat('number of species                         : ' , length(object@train@Occurrence@species.names), '\n')
             cat('species names                             : ' , if (length(object@train@Occurrence@species.names) > 3) paste(c(object@train@Occurrence@species.names[1:3],'...'),collapse=', ') else paste(object@train@Occurrence@species.names,collapse=', ') , '\n')
             cat('number of fearures                        : ' , if (!is.null(object@train@Features)) length(object@train@Features@featureNames) else NA, '\n')
             cat('fearure names                             : ' , if (!is.null(object@train@Features)) {
               if (length(object@train@Features@featureNames) > 3) paste(c(object@train@Features@featureNames[1:3],'...'),collapse=', ') else paste(object@train@Features@featureNames,collapse=', ')
             } else NA, '\n')
             cat('type                                      : ' , 
                 if (all(object@train@Occurrence@presence.only)) 'presence-only' 
                 else if (all(!object@train@Occurrence@presence.only)) 'presence-absence' 
                 else 'presence-absence AND presence-only', '\n')
             cat('number of observations (train)            : ', nrow(object@train@Occurrence@Occurrence),'\n')
             cat('has independet test data?                 : ' , if (is.null(object@test)) FALSE 
                 else if (all(unlist(lapply(object@train@Occurrence@species.names,function(x) {x %in% object@test@Occurrence@species.names})))) TRUE
                 else paste('Only ',length(object@test@Occurrence@species.names),' species have test data!',sep=''),'\n' )
             if (!is.null(object@test))
               cat('number of observations (test)             : ', nrow(object@test@Occurrence@Occurrence),'\n' )
             cat('has Coordinates?                          : ' , !is.null(object@train@coordinates) , '\n')
           }
)

#-------------
setMethod ('show' , 'SpeciesDataList',
           function ( object ) {
             cat('class                                     :' , class(object), '\n')
             cat('===========================================================','\n')
             cat('number of species                         : ' , length(object@train@species.names), '\n')
             cat('species names                             : ' , if (length(object@train@species.names) > 3) paste(c(object@train@species.names[1:3],'...'),collapse=', ') else paste(object@train@species.names,collapse=', ') , '\n')
             cat('number of fearures                        : ' , if (!is.null(object@train@SpeciesDataList[[1]]@Features)) length(object@train@SpeciesDataList[[1]]@Features@featureNames) else NA, '\n')
             cat('fearure names                             : ' , if (!is.null(object@train@SpeciesDataList[[1]]@Features)) {
               if (length(object@train@SpeciesDataList[[1]]@Features@featureNames) > 3) paste(c(object@train@SpeciesDataList[[1]]@Features@featureNames[1:3],'...'),collapse=', ') else paste(object@train@SpeciesDataList[[1]]@Features@featureNames,collapse=', ')
             } else NA, '\n')
             cat('type                                      : ' , 
                 if (all(unlist(lapply(object@train@SpeciesDataList,function(x) {x@Occurrence@presence.only})))) 'presence-only' 
                 else if (all(!unlist(lapply(object@train@SpeciesDataList,function(x) {x@Occurrence@presence.only})))) 'presence-absence' 
                 else 'presence-absence AND presence-only', '\n')
             cat('number of observations (train)            : ', if (length(object@train@species.names) > 6) paste(c(unlist(lapply(object@train@SpeciesDataList,function(x) {length(x@Occurrence@Occurrence)}))[1:6],'...'),collapse=', ')
                 else paste(unlist(lapply(object@train@SpeciesDataList,function(x) {length(x@Occurrence@Occurrence)})),collapse=', ') ,'\n')
             cat('has independet test data?                 : ' , if (is.null(object@test)) FALSE 
                 else if (all(unlist(lapply(object@train@species.names,function(x) {x %in% object@test@species.names})))) TRUE
                 else paste('Only ',length(object@test@species.names),' species have test data!',sep=''),'\n' )
             if (!is.null(object@test))
               cat('number of observations (test)             : ', nrow(object@test@Occurrence@Occurrence),'\n' )
             cat('has Coordinates?                          : ' , !is.null(object@train@SpeciesDataList[[1]]@coordinates) , '\n')
           }
)
#---------
setMethod ('show' , 'sdmSettings',
           function (object) {
             cv.n <- 1
             r.n <- 1
             r <- 1
             
             cat('class                                 :' , class(object), '\n')
             cat('========================================================','\n')
             cat('modelling methods                     : ' , paste(object@methods,collapse=', '), '\n')
             sn <- strsplit(as.character(object@basicSettings@formula[2]),'\\+')[[1]]
             cat('species names                         : ' , if (length(sn) > 3) paste(length(sn),'species including:',paste(c(sn,'...'),collapse=', ')) else paste(sn,collapse=', '), '\n')
             cat('feature names                         : ' , if (length(object@basicSettings@featureNames) > 3) paste(length(object@basicSettings@featureNames),'variables including:',paste(c(object@basicSettings@featureNames[1:3],'...'),collapse=', ')) else paste(object@basicSettings@featureNames,collapse=', '), '\n')
             cat('feature types                         : ' , paste(.ftype_names(object@basicSettings@feature.types),collapse=', '), '\n')
             if (!is.null(object@basicSettings@replicates)) {
               r.n <- length(object@basicSettings@replicates@method)
               r <- object@basicSettings@replicates@n.replicates
               cat('replicate.methods (data partitioning) : ' , paste(object@basicSettings@replicates@method,collapse=','), '\n')
               cat('test percentage                       : ' , object@basicSettings@test.percentage, '\n')
               cat('number of replicates (each method)    : ' , r, '\n')
               if ("cross-validation" %in% object@basicSettings@replicates@method) {
                 cv.n <- length(unique(object@basicSettings@replicates@fold$'cross-validation'))
                 cat('n.folds in cross-validation           : ' ,cv.n , '\n')
               }
             }
             n <- r * (r.n-1+cv.n)
             cat('------------------------------------------\n')
             cat('number of runs                        : ' , paste(n,' for each model, ',n*length(object@methods),' in total... (per species)' ,sep=''), '\n')
             
           }
)
#---------------------

setMethod ('show' , 'sdmModel',
           function (object) {
             cv.n <- 1
             r.n <- 1
             r <- 1
             nn <- names(object@data)
             cat('class                                 :' , class(object), '\n')
             cat('========================================================','\n')
             cat('number of species                     : ' , length(nn), '\n')
             cat('number of modelling methods           : ' , length(object@settings@methods), '\n')
             cat('names of modelling methods            : ' , paste(object@settings@methods,collapse=', '), '\n')
             if (!is.null(object@settings@basicSettings@replicates)) {
               r.n <- length(object@settings@basicSettings@replicates@method)
               r <- object@settings@basicSettings@replicates@n.replicates
               if ("cross-validation" %in% object@settings@basicSettings@replicates@method) {
                 cv.n <- length(unique(object@settings@basicSettings@replicates@fold$'cross-validation'))
                 #cat('n.folds in cross-validation           : ' ,cv.n , '\n')
               }
               n <- r * (r.n-1+cv.n)
               cat('number of simulations per model       : ' , paste(n,'(per species)'), '\n')
               cat('replicate.methods (data partitioning) : ' , paste(object@settings@basicSettings@replicates@method,collapse=','), '\n')
               cat('number of replicates (each method)    : ' , r, '\n')
               cat('test percentage                       : ' , object@settings@basicSettings@test.percentage, '\n')
               cat('---------------------------------------\n')
               cat('run success percentage (all species)  :\n')
               cat('---------------------------------------\n')
               p2 <- function(x) {
                 o <- rep(NA,length(nn))
                 for (i in seq_along(nn)) {
                   o[i] <- (length(which(object@models@multiModelList[[nn[i]]]@modelList[[x]]@run.success))/n)*100
                 }
                 o
               }
               for (i in seq_along(object@settings@methods)) {
                 cat(paste(object@settings@methods[i],paste(rep(' ',8 - length(unlist(strsplit(object@settings@methods[i],'')))),collapse=''),' : ',paste(rep(' ',10),collapse=''),sep='')  , mean(p2(object@settings@methods[i])), '%\n')
               }
               
             }
             if (!is.null(object@models@multiModelList[[1]]@modelList[[1]]@modelObjectList) || !is.null(object@models@multiModelList[[1]]@modelList[[1]]@independent.evaluationList)) {
               cat('---------------------------------------\n')
               cat('average performance (all species)     :\n')
               cat('---------------------------------------\n')
               p <- function(x,n) {
                 apply(.getPerformance(object,x,species = n)[[1]],2,function(x) mean(x,na.rm=TRUE))
               }
               #o <- data.frame(matrix(ncol=4,nrow=length(object@settings@methods)))
               #colnames(o) <- c('method',)
               cat(paste('methods',paste(rep(' ',3),collapse=''),' : ',paste(rep(' ',3),collapse=''),sep='')  , paste(c('AUC','COR','TSS'),collapse='    |  '), '\n')
               cat('------------------------------------------\n')
               for (i in seq_along(object@settings@methods)) {
                 o <- rep(0,3)
                 k <- 0
                 for (j in seq_along(nn)) {
                   w <- try(p(object@settings@methods[i],nn[j]),silent=TRUE)
                   if (!inherits(w,'try-error')) {
                     o <- o + w
                     k <- k + 1
                   }
                 }
                 if (k > 0) {
                   o <- o / k
                   cat(paste(object@settings@methods[i],paste(rep(' ',10 - length(unlist(strsplit(object@settings@methods[i],'')))),collapse=''),' : ',paste(rep(' ',3),collapse=''),sep='')  , paste(round(o,3),collapse='  |  '), '\n') 
                 } else {
                   cat(paste(object@settings@methods[i],paste(rep(' ',10 - length(unlist(strsplit(object@settings@methods[i],'')))),collapse=''),' : ',paste(rep(' ',3),collapse=''),sep='')  , paste(rep('---',3),collapse='  |  '), '\n') 
                 }
                 
               } 
             }
           }
)
