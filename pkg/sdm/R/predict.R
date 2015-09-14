# Author: Babak Naimi, naimi.b@gmail.com
# Date :  July 2014
# Version 1.0
# Licence GPL v3


.getlevels <- function(x) {
  o <- NULL
  if (inherits(x,'singleSpecies') || inherits(x,'multipleSpecies')) {
    if (!is.null(x@train@Features@factors)) {
      o <- vector('list',length(x@train@Features@factors))
      names(o) <- x@train@Features@factors
      for (i in seq_along(o)) o[[i]] <- levels(x@train@Features@features[,x@train@Features@factors[i]])
    }
  } else if (inherits(x,'sdmModel')) {
    f <- c()
    for (i in seq_along(x@settings@basicSettings@feature.types)) {
      if ('factor' %in% x@settings@basicSettings@feature.types[[i]]) f <- c(f,names(x@settings@basicSettings@feature.types)[i])
    }
    
    if (length(f) > 0) {
      o <- vector('list',length(f))
      names(o) <- f
      for (i in seq_along(o)) o[[i]] <- levels(x@data@train@Features@features[,f[i]])
    }
  } else if (inherits(x,'data.frame')) {
    f <- .where(is.factor,x)
    if (any(f)) {
      f <- names(f)[f]
      o <- vector('list',length(f))
      names(o) <- f
      for (i in seq_along(o)) o[[i]] <- levels(x[,f[i]])
    }
  }
  o
}
#-------------


.raster2df <- function(x,level) {
  d <- data.frame(cellnr=1:ncell(x),x[1:ncell(x)])
  d <- d[apply(d,1,function(x) all(!is.na(x))),]
  if (nrow(d) == 0) stop('raster object has no data...!')
  
  if (!missing(level) && !is.null(level)) {
    n <- names(level)
    for (i in seq_along(n)) {
      l <- level[[i]]
      u <- sort(unique(d[,n[i]]))
      if (any(u %in% c(1:length(l)))) {
        u <- u[u %in% c(1:length(l))]
        l <- l[u]
        d[,n[i]] <- factor(d[,n[i]])
        levels(d[,n[i]]) <- l
      } else stop('the grid values in categorical rasters does not match with the factor levels in the model')
    }
  }
  d
}
#----------------


if (!isGeneric("predict")) {
  setGeneric("predict", function(object, ...)
    standardGeneric("predict"))
}	



setMethod('predict', signature(object='sdmModel'), 
          function(object, newdata, filename="",models,species=1,scenario,mean=FALSE,overwrite=TRUE,...) {
            if (missing(newdata)) stop('mewdata is missing...')
            
            if (!missing(models)) {
              models <- unique(.methodFix(models))
              w <- .isMethodAvailable(models)
              if (!any(w)) stop('the specified models are not recognized...')
              if (!all(w)) {
                warning(paste(paste(models[!w], collapse=', '), 'are not recognized...'))
                models <- models[w]
              }
              w <- models %in% object@settings@methods
              if (!any(w)) stop('the specified models do not exist in the sdm object...')
              if (!all(w)) {
                warning(paste('models ',paste(models[!w], collapse=', '), ', are ignored, because they do not exist in the sdm object...',sep=''))
                models <- models[w]
              }
            } else models <- object@settings@methods
            
            names(models) <- models
            
            lib <- unlist(lapply(models,.loadLib))
            #----
            if (is.character(species)) {
              w <- species %in% object@models@species.names
              if (!any(w)) stop('specified species names do not exist in the model object')
              if (!all(w)) {
                species <- species[w]
                warning(paste('species ',paste(species[!w], collapse=', '), ', are ignored, because they do not exist in the sdm object...',sep=''))
              }
            } else if (is.numeric(species)) {
              w <- species %in% seq_along(object@models@multiModelList)
              if (!any(w)) stop('specified species number do not exist in the model object')
              if (!all(w)) {
                species <- species[w]
                warning(paste('species ',paste(species[!w], collapse=', '), ', are ignored, because they do not exist in the sdm object...',sep=''))
              }
              species <- object@models@species.names[w]
            } else stop('species should be character(name of species) or numeric (species number)')
            
            #----
            sc <- object@models@multiModelList[[1]]@modelList[[1]]@scenarios
            nr <- nrow(sc)
            sc <- .showScenarios(sc)
            
            if (missing(scenario)) scenario <- 1:nrow(sc)
            else if (is.character(scenario)) {
              scenario <- sort(.scenarioMatch(scenario,as.character(sc[,1])))
            } else if (is.numeric(scenario)) {
              w <- which(scenario %in% 1:nrow(sc))
              if (length(w) > 0) scenario <- scenario[w]
              else stop('specified scenario(s) is not found in the model')
            } else stop('scenario should be character (name) or numeric')
            
            sn <- as.character(sc[scenario,1])
            sn <- ifelse(sn == 'cross-validation','cv',sn)
            
            nr <- nr * length (models) * length(species)
            #----------
            
            
            b <- nf <- nFact <- NULL
            
            if (inherits(newdata,'data.frame')) {
              n <- colnames(newdata)
              if (!all(object@settings@basicSettings@featureNames %in% n)) stop('the data does not contain some or all of the variables that the model needs...')
              d <- data.frame(.model.frame(newdata,object@settings@basicSettings@feature.types))
              rm(newdata)
              
            } else if (inherits(newdata,'Raster')) {
              if (filename == '') filename <- .generateName('sdm_prediction')
              if (extension(filename) == '') filename <- paste(filename,'.grd',sep='')
              n <- names(newdata)
              if (!all(object@settings@basicSettings@featureNames %in% n)) stop('the data does not contain some or all of the variables that the model needs...')
              d <- .raster2df(newdata,.getlevels(object))
              cellnr <- d$cellnr
              d <- data.frame(.model.frame(d,object@settings@basicSettings@feature.types))
              b <- brick(raster(newdata))
              mr <- rep(NA,ncell(b))
              rm(newdata)
            } else stop('newdata should be Raster* or data.frame...!')
            
            if ('factor' %in% .ftype_names(object@settings@basicSettings@feature.types)) {
              nf <- c()
              for (f in object@settings@basicSettings@featureNames) {
                if ('linear' %in% object@settings@basicSettings@feature.types[[f]]) nf <- c(nf,f)
              }
              nFact <- .where(is.factor,d)
              nFact <- names(nFact[nFact])
              dM <- data.frame(.model.frame(.species2df(object@data),object@settings@basicSettings@feature.types))
            }
            
            #--------------
            options(warn=-1)
            
            funsP <- lapply(models,function(x) get(paste('.',x,'Predict',sep='')))
            
            metadata <- data.frame(id=1:nr,species=rep(NA,nr),scenario=rep(NA,nr),model=rep(NA,nr),success=rep(FALSE,nr))
            rnames <- fullnames <- c()
            if (inherits(b,'Raster')) {
              if (nr > 1) {
                b <- brick(b,nl=nr)
                writeRaster(b,filename=filename,overwrite=overwrite)
                b <- brick(filename)
              }
              else {
                b <- raster(b)
                writeRaster(b,filename=filename,overwrite=overwrite)
                b <- raster(filename)
              }
              
            } else {
              mtx <- matrix(NA,nrow=nrow(d),ncol=0)
            }
            
            
            co <- 1
            
            for (i in seq_along(species)) {
              for (j in seq_along(scenario)) {
                runs <- sc[j,2]:sc[j,3]
                
                options(warn=-1)
                for (r in seq_along(runs)) {
                  mL <- lapply(models,function(x) object@models@multiModelList[[species[i]]]@modelList[[x]]@modelObjectList[[runs[r]]])
                  
                  if (!is.null(nFact)) {
                    if (sn[j] != 'full') {
                      run.ID <- .getRunIndex(object@settings@basicSettings@replicates,object@models@multiModelList[[i]]@modelList[[1]]@scenarios[runs[r],])
                      d2 <- .factorFix(dM[as.character(run.ID),],d,nFact,nf)
                      d2 <- .eqFactLevel(dM[as.character(run.ID),],d2)
                    } else {
                      d2 <- .factorFix(dM,d,nFact,nf)
                      d2 <- .eqFactLevel(dM,d2)
                    }
                    m2 <- lapply(funsP,function(f,...) try(f(d2,...),silent=TRUE),s=object@settings@modelSettings,models=mL)
                    rm(d2)
                  } else m2 <- lapply(funsP,function(f,...) try(f(d,...),silent=TRUE),s=object@settings@modelSettings,models=mL)
                  #-----
                  for (m in seq_along(models)) {
                    metadata[co,2] <- species[i]
                    metadata[co,3] <- sn[j]
                    metadata[co,4] <- models[m]
                    if (!inherits(m2[[m]],'try-error')) {
                      metadata[co,5] <- TRUE
                      if (inherits(b,'RasterBrick')) {
                        mr[cellnr] <- m2[[m]]
                        b <- update(b,mr,cell=1,band=co)
                        rnames <- c(rnames,paste(models[m],'_sp',i,'sc',j,'r',r,sep=''))
                        fullnames <- c(fullnames,paste(models[m],'_',species[i],'_',sn[j],'_run',r,sep=''))
                      } else if (inherits(b,'RasterLayer')) {
                        mr[cellnr] <- m2[[m]]
                        b <- update(b,mr,cell=1)
                        rnames <- models[m]
                        fullnames <- models[m]
                      } else {
                        mtx <- cbind(mtx,m2[[m]])
                        rnames <- c(rnames,paste(models[m],'_sp',i,'sc',j,'r',r,sep=''))
                      }
                    }
                    co <- co + 1
                  }
                  #-----
                }
              }
            }
            options(warn=0)
            if (!all(metadata[,5])) {
              if (!any(metadata[,5])) {
                if (inherits(b,'Raster')) {
                  rm(b)
                  unlink(filename)
                  stop('Error in prediction....!')
                } else {
                  rm(mtx)
                  stop('Error in prediction....!')
                }
              } else {
                if (inherits(b,'Raster')) {
                  i <- (nlayers(b) - length(!metadata[,5])):nlayers(b)
                  b <- dropLayer(b,i)
                }
              }
            }
            
            if (length(runs) > 1 && mean) {
              if (inherits(b,'Raster')) {
                rnames <- c()
                newfullnames <- c()
                newr <- raster(b)
                for (i in seq_along(species)) {
                  for (j in seq_along(sn)) {
                    for (m in seq_along(models)) {
                      rnames <- c(rnames,paste(models[m],'_sp',i,'sc',j,sep=''))
                      newfullnames <- c(newfullnames,paste(models[m],'_',species[i],'_',sn[j],sep=''))
                      w <- which(unlist(lapply(strsplit(fullnames,'_'),function(x) {x[1] == models[m] & x[2] == species[i] & x[3] == sn[j]})))
                      if (length(w) > 1) {
                        temp <- calc(b[[w]],function(x) mean(x,na.rm=TRUE))
                        newr <- addLayer(newr,temp)
                      } else newr <- addLayer(newr,b[[w]])
                    }
                  }
                }
                
                rm(b)
                unlink(filename)
                b <- brick(newr,filename=filename,values=TRUE,overwrite=TRUE) 
                fullnames <- newfullnames
                rm(newr,newfullnames)
              } else {
                newmtx <- matrix(NA,nrow=nrow(d),ncol=0)
                newrnames <- c()
                .splt <- function(x) {
                  s1 <- strsplit(x,'_')[[1]]
                  m <- s1[1]
                  s2 <- strsplit(strsplit(s1[2],'sp')[[1]][2],'sc')[[1]]
                  sp <- s2[1]
                  s3 <- strsplit(s2[2],'r')[[1]]
                  sc <- s3[1]
                  r <- s3[2]
                  c(m,sp,sc,r)
                }
                for (i in seq_along(species)) {
                  for (j in seq_along(sn)) {
                    for (m in seq_along(models)) {
                      newrnames <- c(newrnames,paste(models[m],'_sp',i,'sc',j,sep=''))
                      w <- unlist(lapply(rnames,function(x) {
                        s <- .splt(x)
                        s[1] == models[m] & s[2] == i & s[3] == j
                      }))
                      if (length(w) > 1) {
                        temp <- apply(mtx[,w],1,function(x) mean(x,na.rm=TRUE))
                        newmtx <- cbind(newmtx,temp)
                      } else newmtx <- cbind(newmtx,mtx[,w])
                    }
                  }
                }
                rnames <- newrnames
                mtx <- newmtx
              }
              
            }
            
            
            if (!inherits(b,'Raster')) {
              colnames(mtx) <- rnames
              return(mtx)
            } else {
              names(b) <- rnames
              b <- setZ(b,fullnames,name='fullname')
              return(b)
            }
          }
)

