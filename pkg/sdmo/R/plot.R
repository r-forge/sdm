# Author: Babak Naimi, naimi.b@gmail.com
# Date :  July 2014
# Version 1.0
# Licence GPL v3


.statFix2 <- function(x) {
  # roc is included...
  if (is.numeric(x)) {
    x <- ifelse(x > 16,1,x)
    x <- ifelse(x < 1,1,x)
    x <- unique(x)
    x <- c('roc','sensitivity','specificity','TSS','Kappa','NMI','phi','ppv','npv','ccr','mcr','or','ommission','commission','predicted.prevalence')[x]
  } else {
    x <- tolower(x)
    for (i in seq_along(x)) {
      if (any(!is.na(pmatch(c("se"),x[i])))) x[i] <- 'sensitivity'
      else if (any(!is.na(pmatch(c("sp"),x[i])))) x[i] <- 'specificity'
      else if (any(!is.na(pmatch(c("ts"),x[i])))) x[i] <- 'TSS'
      else if (any(!is.na(pmatch(c("ka"),x[i])))) x[i] <- 'Kappa'
      else if (any(!is.na(pmatch(c("nm"),x[i])))) x[i] <- 'NMI'
      else if (any(!is.na(pmatch(c("pp"),x[i])))) x[i] <- 'ppv'
      else if (any(!is.na(pmatch(c("np"),x[i])))) x[i] <- 'npv'
      else if (any(!is.na(pmatch(c("cc"),x[i])))) x[i] <- 'ccr'
      else if (any(!is.na(pmatch(c("mc"),x[i])))) x[i] <- 'mcr'
      else if (any(!is.na(pmatch(c("or"),x[i])))) x[i] <- 'or'
      else if (any(!is.na(pmatch(c("om"),x[i])))) x[i] <- 'ommission'
      else if (any(!is.na(pmatch(c("com"),x[i])))) x[i] <- 'commission'
      else if (any(!is.na(pmatch(c("pr"),x[i])))) x[i] <- 'prevalence'
      else if (any(!is.na(pmatch(c("ph"),x[i])))) x[i] <- 'phi'
      else if (any(!is.na(pmatch(c("ro"),x[i])))) x[i] <- 'ROC'
    }
    x <- unique(x)
    w <- which(x %in% c('ROC','sensitivity','specificity','TSS','Kappa','NMI','phi','ppv','npv','ccr','mcr','or','ommission','commission','prevalence'))
    x <- x[w]
  }
  x
}
#----
.scenarioMatch <- function(x,y) {
  for (i in seq_along(x)) {
    if (any(!is.na(pmatch(c("cros"),x[i])))) x[i] <- 'cross-validation'
    else if (any(!is.na(pmatch(c("cv"),x[i])))) x[i] <- 'cross-validation'
    else if (any(!is.na(pmatch(c("bo"),x[i])))) x[i] <- 'bootstrap'
    else if (any(!is.na(pmatch(c("sub"),x[i])))) x[i] <- 'subsampling'
    else if (any(!is.na(pmatch(c("fu"),x[i])))) x[i] <- 'full'
  }
  if (!missing(y)) {
    xx <- c()
    for (i in seq_along(x)) {
      w <- which(y == x[i])
      if (length(w) > 0) xx <-c(xx,w) 
    }
    x <- xx
  }
  x
}


if (!isGeneric("plot")) {
  setGeneric("plot", function(x,y,...)
    standardGeneric("plot"))
}  

setMethod("plot", signature(x='sdmEvaluate'),
          function(x,y,col='red',smooth=TRUE,xlab,ylab,main,xlim,ylim,...) {
            if (missing(y)) y <- 'ROC'
            y <- .statFix2(y)[1]
            if (is.na(y)) stop('the evaluation statistic specified in y is not recognized...') 
            
            if (y == 'ROC') {
              r <- .roc(x@observed,x@predicted)
              if (smooth) {
                rr <- try(supsmu(r[,1],r[,2],bass=0),silent=TRUE)
                if (!inherits(rr,'try-error')) {
                  r <- rr
                  rm(rr)
                }
              }
              
              if (missing(xlab)) xlab <- '1-Specificity (false positive rate)'
              if (missing(ylab)) ylab <- 'Sensitivity (true positive rate)'
              if (missing(main)) main <- 'ROC plot'
              
              plot(r,type='l',col=col,xlim=c(0,1),ylim=c(0,1),xlab=xlab, ylab=ylab,main=main,...)
              abline(a=0,b=1,col='gray',lty=2)
              legend(x=0.7,y=0.07,legend=paste('AUC = ',x@AUC,sep=''),lty=1,col=col)
            } else {
              r <- .getEvalThresholds(x@observed,x@predicted,y)
              mx <- r[which.max(r[,2]),1]
              if (smooth) {
                rr <- try(supsmu(r[,1],r[,2],bass=0),silent=TRUE)
                if (!inherits(rr,'try-error')) {
                  r <- rr
                  rm(rr)
                }
              }
              if (missing(xlab)) xlab <-'threshold'
              if (missing(ylab)) ylab <- y
              if (missing(main)) main <- paste('max at:',round(mx,2))
             plot(r,type='l',col=col,xlab=xlab,ylab=ylab,main= main,...)
            }
          }
)




if (!isGeneric("roc")) {
  setGeneric("roc", function(x,...)
    standardGeneric("roc"))
}  

setMethod("roc", signature(x='sdmModel'),
          function(x,species=1,models,smooth=TRUE,scenario,legend=TRUE,...) {
            op <- par(no.readonly = TRUE) 
            if (length(species) > 1) {
              species <- species[1]
              warning('species argument should has one element, so, only the first item is considered...')
            }
            if (is.character(species)) {
              if (!species %in% x@models@species.names) stop('specified species name does not exist in the model object')
            } else if (is.numeric(species)) {
              if (!species %in% seq_along(x@models@multiModelList)) stop('specified species number does not exist in the model object')
            } else stop('species should be character(name of species) or numeric (species number)')
            
            if (missing(models)) models <- names(x@models@multiModelList[[species]]@modelList)
            else {
              models <- models[models %in% names(x@models@multiModelList[[species]]@modelList)]
              if (length(models) == 0) stop('the specified models do not exist in the model object...!')
            }
            
            x <- x@models@multiModelList[[species]]@modelList[models]
            
            if (missing(scenario)) scenario <- 1:nrow(.showScenarios(x[[1]]@scenarios))
            else if (is.character(scenario)) {
              scenario <- sort(.scenarioMatch(scenario,as.character(.showScenarios(x[[1]]@scenarios)[,1])))
            } else if (is.numeric(scenario)) {
              w <- which(scenario %in% 1:nrow(.showScenarios(x[[1]]@scenarios)))
              if (length(w) > 0) scenario <- scenario[w]
              else stop('specified scenario(s) is not found in the model')
            } else stop('scenario should be character (name) or numeric')
            
            sn <- as.character(.showScenarios(x[[1]]@scenarios)[scenario,1])
            
            np <- length(scenario) * length (models)
            if (length(scenario) == 3 & np > 12) {
              warning('Due to larger number of plots, only the first 4 models are used...')
              models <- models[1:3]
              par(mfrow=c(4,3))
            } else if (np > 16) {
              w <- floor(16 / length(scenario))
              models <- models[1:w]
              warning(paste('Due to larger number of plots, only the first', w ,'models are used...'))
              par(mfrow=c(4,4))
            } else {
              w <- floor(sqrt(np))
              h <- ceiling(np/w)
              if (abs(w-h) > 1) {
                w <- w+1
                h <- h-1
              }
              par(mfrow=c(w,h))  
            }
            
            if (np > 10) z <- 0.04
            else if (np > 5 & np <= 10) z <- 0.06
            else z <- 0.13
            
            for (i in seq_along(models)) {
              sc <- .showScenarios(x[[i]]@scenarios)
              for (j in scenario) {
                runs <- sc[j,2]:sc[j,3]
                e <- x[[i]]@evaluation@scenarios[runs,]
                e1 <- e[!is.na(e[,2]),2]
                e2 <- e[!is.na(e[,3]),3]
                if (length(e1) > 0) {
                  ee1 <- lapply(x[[i]]@evaluation@evaluationList[e1],function(x) .roc(x@observed,x@predicted))
                  auc1 <- round(mean(unlist(lapply(x[[i]]@evaluation@evaluationList[e1],function(x) x@AUC)),na.rm=TRUE),3)
                } else {
                  ee1 <- NULL
                  auc1 <- NULL
                }
                if (length(e2) > 0) {
                  ee2 <- lapply(x[[i]]@evaluation@evaluationList[e2],function(x) .roc(x@observed,x@predicted))
                  auc2 <- round(mean(unlist(lapply(x[[i]]@evaluation@evaluationList[e2],function(x) x@AUC)),na.rm=TRUE),3)
                } else {
                  ee2 <- NULL
                  auc2 <- NULL
                }
                if (legend) .plot.roc(ee1,ee2,auc=c(auc1=auc1,auc2=auc2),main=paste('model: ',models[i],'- ',sn[j],sep=''),cex=1-np*z)
                else .plot.roc(ee1,ee2,main=paste('model: ',models[i],'- ',sn[j],sep=''))
              }
            }
            par(op)
          }
)

.plot.roc <- function(x,y,auc,smooth=TRUE,xlab,ylab,main,cex=1,...) {
  if (missing(xlab)) xlab <- '1-Specificity (false positive rate)'
  if (missing(ylab)) ylab <- 'Sensitivity (true positive rate)'
  if (missing(main)) main <- 'ROC plot'
  xm <- ym <-  NULL
  if (inherits(x,'list')) {
    xm <- x[[1]]
    if (length(x) > 1) {
      for (i in 2:length(x)) xm <- xm + x[[i]]
      xm <- xm / length(x)
    }
    
    if (smooth) {
      for (i in seq_along(x)) {
        rr <- try(supsmu(x[[i]][,1],x[[i]][,2],bass=0),silent=TRUE)
        if (!inherits(rr,'try-error')) x[[i]] <- rr
      }
      rr <- try(supsmu(xm[,1],xm[,2],bass=0),silent=TRUE)
      if (!inherits(rr,'try-error')) xm <- rr
      rm(rr)
    }
    plot(x[[1]],type='l',col='#FFC0CB',xlim=c(0,1),ylim=c(0,1),xlab=xlab, ylab=ylab,main=main,lwd=0.7,...)
    if (length(x) > 1) {
      for (i in 2:length(x)) {
        lines(x[[i]],col='#FFC0CB',lwd=0.7)
      }
    }
    abline(a=0,b=1,lty=2,lwd=2)
    
  } else {
    plot(x,type='l',col='red',xlim=c(0,1),ylim=c(0,1),xlab=xlab, ylab=ylab,main=main,lwd=3,...)
   
  }
  
  if (!missing(y) & !is.null(y)) {
    if (inherits(y,'list')) {
      ym <- y[[1]]
      if (length(y) > 1) {
        for (i in 2:length(y)) ym <- ym + y[[i]]
        ym <- ym / length(y)
      }
      
      if (smooth) {
        for (i in seq_along(y)) {
          rr <- try(supsmu(y[[i]][,1],y[[i]][,2],bass=0),silent=TRUE)
          if (!inherits(rr,'try-error')) y[[i]] <- rr
        }
        rr <- try(supsmu(ym[,1],ym[,2],bass=0),silent=TRUE)
        if (!inherits(rr,'try-error')) ym <- rr
        rm(rr)
      }
      for (i in 1:length(y)) {
        lines(y[[i]],col='#ADD8E6',lwd=0.7)
      }
    
      
    } else {
      lines(y,lwd=3,col='blue')
    }
  }
  abline(a=0,b=1,lty=2,lwd=2)
  if (!is.null(xm)) lines(xm,lwd=3,col='red')
  if (!is.null(ym)) lines(ym,lwd=3,col='blue')
  if (!missing(auc)) {
    
    if (length(auc) == 1) {
      if (names(auc) == 'auc1') col='red'
      else col='blue'
      if (length(x) > 1) n <- paste('Mean AUC = ',auc,sep='')
      else n <- paste('AUC = ',auc,sep='')
      legend(x="bottomright",legend=n,lty=1,col=col,cex=cex)
    } else if (length(auc) == 2) {
      if (length(x) > 1) n <- paste(paste('Mean AUC (',c('partitionings) = ','independent) ='),sep=''),auc)
      else n <- paste(paste('AUC (',c('partitionings) = ','independent) ='),sep=''),auc)
      legend(x="bottomright",legend=n,lty=c(1,1),col=c('red','blue'),cex=cex)
    }
  }
}
#---------

setMethod("plot", signature(x='singleSpecies'),
          function(x,y,which=1,col=c('blue','red'),xlab,ylab,main,xlim,ylim,pch,...) {
            if (missing(y)) y <- 'map'
            else y <- tolower(y)
            
            if (is.numeric(which) && !which %in% c(0,1)) stop('"which" argument specifies wheather train (1) or test (2) should be plotted')
            else if (is.character(which)) {
              if (!which %in% c('test','train')) stop('"which" argument specifies wheather train (1) or test (2) should be plotted')
              else {
                if (which == 'train') which <- 1
                else which <- 2
              }
            } 
            
            if (y == 'map') {
              if (missing(xlab)) xlab <- 'X'
              if (missing(ylab)) ylab <- 'Y'
              if (missing(pch)) pch <- c(16,16)
              if (length(col) == 1) col <- rep(col,2)
              if (length(pch) == 1) pch <- rep(pch,2)
              if (which == 1) {
                if (is.null(x@train@coordinates)) stop('the species train data does not contain coordinates, chnage y for the other plots')
                else {
                  xy <- x@train@coordinates@coords
                  rx <- range(xy[,1])
                  xadd <- (rx[2] - rx[1]) * 0.01
                  ry <- range(xy[,2])
                  yadd <- (ry[2] - ry[1])*0.01
                  if (missing(xlim)) xlim <- rx + (xadd*c(-1,1))
                  if (missing(ylim)) ylim <- ry + (yadd*c(-1,1))
                  if (missing(main)) main <- 'presence/absence map'
                  if (!x@train@Occurrence@presence.only) {
                    w1 <- which(x@train@Occurrence@Occurrence == 1)
                    plot(xy[w1,],xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,col=col[1],main=main,pch=pch[1],...)
                    points(xy[-w1,],col=col[2],pch=pch[2],...)
                  } else {
                    plot(xy,xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,col=col[1],main=main,pch=pch[1],...)
                  }
                }
              } else {
                if (is.null(x@test)) stop('there is no test data, change which to 1 to plot train data')
                if (is.null(x@test@coordinates)) stop('the species test data does not contain coordinates, chnage y for the other plots')
                else {
                  xy <- x@test@coordinates@coords
                  rx <- range(xy[,1])
                  xadd <- (rx[2] - rx[1]) * 0.01
                  ry <- range(xy[,2])
                  yadd <- (ry[2] - ry[1])*0.01
                  if (missing(xlim)) xlim <- rx + (xadd*c(-1,1))
                  if (missing(ylim)) ylim <- ry + (yadd*c(-1,1))
                  if (missing(main)) main <- 'presence/absence map'
                  if (!x@test@Occurrence@presence.only) {
                    w1 <- which(x@train@Occurrence@Occurrence == 1)
                    plot(xy[w1,],xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,col=col[1],main=main,pch=pch[1],...)
                    points(xy[-w1,],col=col[2],pch=pch[2],...)
                  } else {
                    plot(xy,xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,col=col[1],main=main,pch=pch[1],...)
                  }
                }
              }
            }
            
          }
)
