# Author: Babak Naimi, naimi.b@gmail.com
# Date :  July 2014
# Version 1.0
# Licence GPL v3

.isMethodAvailable <- function(x) {
  m <- c('glm','brt','rf')
  x %in% m
}


.getParams <- function(x,name) {
  params <- list()
  g <- x[[name]]
  n <- slotNames(g)[-1]
  n <- n[-length(n)]
  for (i in 1:(length(n) - 1)) params[[n[i]]] <- slot(g,n[i])
  params <- c(params,slot(g,n[length(n)]))
  params
}
#------------


.getPredictParams <- function(x,name) {
  g <- x[[name]]
  slot(g,'predictSettings')
}



.paramFix <- function(x) {
  for (i in seq_along(x)) {
    if (any(!is.na(pmatch(c("interact"),x[i])))) x[i] <- 'interaction.depth'
    else if (any(!is.na(pmatch(c("replicates.m"),x[i])))) x[i] <- 'replicate.method'
    else if (any(!is.na(pmatch(c("replicate.m"),x[i])))) x[i] <- 'replicate.method'
    else if (any(!is.na(pmatch(c("cv"),x[i])))) x[i] <- 'cv.fold'
    else if (any(!is.na(pmatch(c("form"),x[i])))) x[i] <- 'formula'
    else if (any(!is.na(pmatch(c("meth"),x[i])))) x[i] <- 'methods'
    else if (any(!is.na(pmatch(c("test.p"),x[i])))) x[i] <- 'test.percent'
    else if (any(!is.na(pmatch(c("pseudo.m"),x[i])))) x[i] <- 'pseudo.method'
    else if (any(!is.na(pmatch(c("pseudo.n"),x[i])))) x[i] <- 'pseudo.n'
    else if (any(!is.na(pmatch(c("var.i"),x[i])))) x[i] <- 'var.importance'
    else if (any(!is.na(pmatch(c("var.s"),x[i])))) x[i] <- 'var.selection'
    else if (any(!is.na(pmatch(c("varI"),x[i])))) x[i] <- 'var.importance'
    else if (any(!is.na(pmatch(c("variable.i"),x[i])))) x[i] <- 'var.importance'
    else if (any(!is.na(pmatch(c("variable.s"),x[i])))) x[i] <- 'var.selection'
    else if (any(!is.na(pmatch(c("varS"),x[i])))) x[i] <- 'var.selection'
    else if (any(!is.na(pmatch(c("sdm"),x[i])))) x[i] <- 'sdmSetting'
    else if (any(!is.na(pmatch(c("predict"),x[i])))) x[i] <- 'predict'
  }
  x
}

#---------


##################################
#------- FIT functions ------
.glmFit <- function(data,...) {
  s <- list(...)[['s']]
  p <- .getParams(s,'glm')
  p <- c(formula=.getFormula(colnames(data)),data=quote(data),p)
  m <- do.call(glm,p)
  m
}
#----------
.brtFit <- function(data,...) {
  s <- list(...)[['s']]
  p <- .getParams(s,'brt')
  p <- c(formula=.getFormula(colnames(data)),data=quote(data),p)
  m <- do.call(gbm,p)
  m
}
#-----
.rfFit <- function(data,...) {
  s <- list(...)[['s']]
  p <- .getParams(s,'rf')
  p <- c(formula=.getFormula(colnames(data)),data=quote(data),p)
  m <- do.call(randomForest,p)
  m
}
#-----

.svmFit <- function(data,...) {
  s <- list(...)[['s']]
  p <- .getParams(s,'svm')
  p <- c(x=.getFormula(colnames(data)),data=quote(data),p)
  m <- do.call(ksvm,p)
  m
}


.gamFit <- function(data,...) {
  s <- list(...)[['s']]
  p <- .getParams(s,'gam')
  f <- .where(is.factor,data)
  p <- c(list(formula=.getGamFormula(colnames(data),f),data=quote(data)),p)
  m <- do.call(gam,p)
  m
}




#######################
#------------ PREDICT FUNCTIONS
.glmPredict <- function(newdata,...) {
  s <- list(...)[['s']]
  m <- list(...)[['models']]$glm
  if (!inherits(m,'try-error')) {
    p <- .getPredictParams(s,'glm')
    p[['object']] <- quote(m)
    p <- c(newdata=quote(newdata),p)
    m <- do.call(predict.glm,p)
  } else m <- NA
  m
}
#----------
.brtPredict <- function(newdata,...) {
  s <- list(...)[['s']]
  m <- list(...)[['models']]$brt
  if (!inherits(m,'try-error')) {
    p <- .getPredictParams(s,'brt')
    p[['object']] <- quote(m)
    p <- c(newdata=quote(newdata),p)
    m <- do.call(predict.gbm,p)
  } else m <- NA
  m
}
#---------

.rfPredict <- function(newdata,...) {
  s <- list(...)[['s']]
  m <- list(...)[['models']]$rf
  if (!inherits(m,'try-error')) {
    p <- .getPredictParams(s,'rf')
    p[['object']] <- quote(m)
    p <- c(newdata=quote(newdata),p)
    m <- do.call(predict,p)
  } else m <- NA
  m
}
#--------------

.gamPredict <- function(newdata,...) {
  s <- list(...)[['s']]
  m <- list(...)[['models']]$gam
  if (!inherits(m,'try-error')) {
    p <- .getPredictParams(s,'gam')
    p[['object']] <- quote(m)
    p <- c(newdata=quote(newdata),p)
    m <- do.call(predict,p)
  } else m <- NA
  m
}

#------

.svmPredict <- function(newdata,...) {
  s <- list(...)[['s']]
  m <- list(...)[['models']]$svm
  if (!inherits(m,'try-error')) {
    p <- .getPredictParams(s,'svm')
    p[['object']] <- quote(m)
    p <- c(newdata=quote(newdata),p)
    m <- do.call(predict,p)
  } else m <- NA
  m
}

#######################
#-----
.loadLib <- function(method) {
  switch(method,
         brt=require(gbm, quietly = TRUE),
         rf=require(randomForest, quietly = TRUE),
         glm=TRUE,
         gam=require(mgcv,quietly = TRUE),
         svm=require(kernlab,quietly = TRUE)
  )
}
#-----

.genFormula <- function(x,y) {
  n <- names(x)
  fo <- c()
  for (i in seq_along(n)) {
    f <- x[[i]]
    for (j in seq_along(f)) {
      if (f[j] == 'linear') {
        fo <- c(fo,n[i])
      } else if (f[j] == 'quadratic') {
        fo <- c(fo,paste('quadratic(',n[i],')',sep=''))
      } else if (f[j] == 'cubic') {
        fo <- c(fo,paste('cubic(',n[i],')',sep=''))
      } else if (any(!is.na(pmatch(c("power@"),f[j])))) {
        s <- strsplit(f[j],'@')[[1]][2:3]
        fo <- c(fo,paste('I(',n[i],'^',s[2],')',sep=''))
      } else if (f[j] == 'factor') {
        fo <- c(fo,paste('factor(',n[i],')',sep=''))
      } else if (any(!is.na(pmatch(c("interaction@"),f[j])))) {
        s <- strsplit(f[j],'@')[[1]][-1]
        fo <- c(fo,paste(s,collapse=':'))
      }
    }
  }
  as.formula(paste(y,'~',paste(fo,collapse='+'),sep=''))
}

#----
.getFormula <- function(n) {
  as.formula(paste(n[1],'~',paste(n[-1],collapse='+'),sep=''))
}
#----

.getGamFormula <- function(n,f) {
  if (any(f)) {
    nf <- names(which(f))
    n <- n[-which(n %in% nf)]
    as.formula(paste(n[1],'~',paste(c(paste(paste('s(',n[-1],sep=''),')',sep=''),nf),collapse='+'),sep=''))
  } else as.formula(paste(n[1],'~',paste(paste(paste('s(',n[-1],sep=''),')',sep=''),collapse='+'),sep=''))
}
#----
.where <- function(f, x) {
  vapply(x, f, logical(1))
}
#------

.dummy <- function(x,n) {
  out <- data.frame(matrix(nrow=length(x),ncol=0)) 
  for (level in unique(x)) out[paste(n,'_',level,sep='')] <- ifelse(x == level, 1, 0)
  out
}
#-----
.model.frame <- function(data,setting) {
  d <- data.frame(matrix(nrow=nrow(data),ncol=0))
  n <- names(setting)
  fac <- .where(is.factor,head(data[,n]))
  cn <- c()
  for (i in seq_along(n)) {
    f <- setting[[i]]
    for (j in seq_along(f)) {
      if (f[j] == 'linear') {
        d <- cbind(d,data[,n[i]])
        cn <- c(cn,n[i])
      } else if (f[j] == 'quadratic') {
        d <- cbind(d,data[,n[i]]^2)
        cn <- c(cn,paste('quadratic_',n[i],sep=''))
      } else if (any(!is.na(pmatch(c("interaction@"),f[j])))) {
        s <- strsplit(f[j],'@')[[1]][-1]
        wF <- .where(is.factor,head(data[,s]))
        if (!any(wF)) {
          w <- 1
          for (ii in seq_along(s)) w <- w * data[,s[ii]]
          d <- cbind(d,w)
          cn <- c(cn,f[j])
          rm(w)
        } else {
          dd <- data[,s]
          nn <- list()
          for (ii in seq_along(s)) {
            if (wF[ii]) {
              dF <- .dummy(dd[,s[ii]],s[ii])
              dd <- data.frame(dd,dF)
              nn[[ii]] <- colnames(dF)
            } else nn[[ii]] <- s[ii]
          }
          
          dF <- nn[[1]]
          for (ii in 2:length(nn)) dF <- outer(dF, nn[[ii]], paste) 
          dF <- lapply(as.vector(dF),function(x) {strsplit(x,' ')[[1]]})
          for (ii in seq_along(dF)) {
            w <- 1
            for (jj in seq_along(dF[[ii]])) {
              w <- w * dd[,dF[[ii]][jj]]
            }
            fJ <- paste('interaction@',paste(dF[[ii]],collapse='@'),sep='')
            d <- cbind(d,w)
            cn <- c(cn,fJ)
          }
        }
        
      } else if (any(!is.na(pmatch(c("power@"),f[j])))) {
        s <- strsplit(f[j],'@')[[1]][2:3]
        d <- cbind(d,data[,s[1]]^as.numeric(s[2]))
        cn <- c(cn,f[j])
      } else if (f[j] == 'factor') {
        d <- cbind(d,factor(data[,n[i]]))
        cn <- c(cn,paste('factor_',n[i],sep=''))
      }
    }
  }
  colnames(d) <- cn
  d
}


#---------
.simpleFormula <- function(x) {
  formula(paste('~',paste(x,collapse='+'),sep=''))
}
#---------
.methodFix <- function(x) {
  x <- tolower(x)
  for (i in seq_along(x)) {
    if (any(!is.na(pmatch(c("brt"),x[i])))) x[i] <- 'brt'
    else if (any(!is.na(pmatch(c("boo"),x[i])))) x[i] <- 'brt'
    else if (any(!is.na(pmatch(c("gbm"),x[i])))) x[i] <- 'brt'
    else if (any(!is.na(pmatch(c("ran"),x[i])))) x[i] <- 'rf'
    else if (any(!is.na(pmatch(c("rf"),x[i])))) x[i] <- 'rf'
    else if (any(!is.na(pmatch(c("forest"),x[i])))) x[i] <- 'rf'
    else if (any(!is.na(pmatch(c("sv"),x[i])))) x[i] <- 'svm'
    else if (any(!is.na(pmatch(c("glm"),x[i])))) x[i] <- 'glm'
    else if (any(!is.na(pmatch(c("lin"),x[i])))) x[i] <- 'glm'
    else if (any(!is.na(pmatch(c("lm"),x[i])))) x[i] <- 'glm'
    else if (any(!is.na(pmatch(c("gam"),x[i])))) x[i] <- 'gam'
    else if (any(!is.na(pmatch(c("maxl"),x[i])))) x[i] <- 'maxlike'
    else if (any(!is.na(pmatch(c("maxe"),x[i])))) x[i] <- 'maxent'
    else if (any(!is.na(pmatch(c("bio"),x[i])))) x[i] <- 'bioclim'
    else if (any(!is.na(pmatch(c("do"),x[i])))) x[i] <- 'domain'
    else if (any(!is.na(pmatch(c("mah"),x[i])))) x[i] <- 'mahalanobis'
    else if (any(!is.na(pmatch(c("nn"),x[i])))) x[i] <- 'nnet'
    else if (any(!is.na(pmatch(c("mar"),x[i])))) x[i] <- 'mars'
    else if (any(!is.na(pmatch(c("fd"),x[i])))) x[i] <- 'fda'
  }
  x
}
#------

#---------
.createModelSetting <- function(models) {
  m <- c('glm','lm','brt','gbm','rf','randomforests','svm','ksvm','gam')
  models <- tolower(models)
  #-------
  w <- which(models == 'gbm')
  if (length(w) > 0) models[w] <- 'brt'
  
  w <- which(models == 'lm')
  if (length(w) > 0) models[w] <- 'glm'
  
  w <- which(models == 'randomforests')
  if (length(w) > 0) models[w] <- 'rf'
  
  w <- which(models == 'ksvm')
  if (length(w) > 0) models[w] <- 'svm'
  
  models <- unique(models)
  
  #-------
  w <- models %in% m
  if (!any(w)) stop('the specifed models are unknown!')
  models <- models[which(w)]
  s <- new("sdmSettings",basicSettings=new('.basicSettings'))
  
  for (i in seq_along(models)) {
    if (models[i] == 'glm') {
      s@methods <- c(s@methods,'glm')
      s@modelSettings <- c(s@modelSettings,glm=new('.glmSettings'))
    } else if (models[i] == 'gam') {
      s@methods <- c(s@methods,'gam')
      s@modelSettings <- c(s@modelSettings,gam=new('.gamSettings'))
    } else if (models[i] == 'brt') {
      s@methods <- c(s@methods,'brt')
      s@modelSettings <- c(s@modelSettings,brt=new('.brtSettings'))
    } else if (models[i] == 'rf') {
      s@methods <- c(s@methods,'rf')
      s@modelSettings <- c(s@modelSettings,rf=new('.rfSettings'))
    } else if (models[i] == 'svm') {
      s@methods <- c(s@methods,'svm')
      s@modelSettings <- c(s@modelSettings,svm=new('.svmSettings'))
    }
  }
  s
}
#----------


#----------

.species2df <- function(x,n=1) {
  if (inherits(x,'singleSpecies')) {
    d <- data.frame(x@train@Occurrence@Occurrence,x@train@Features@features)
    colnames(d) <- c(x@train@Occurrence@species.name,x@train@Features@featureNames)
    rownames(d) <- x@train@Occurrence@index@ID
  } else if (inherits(x,'multipleSpecies')) {
    d <- data.frame(x@train@Occurrence@Occurrence,x@train@Features@features)
    colnames(d) <- c(x@train@Occurrence@species.names,x@train@Features@featureNames)
    rownames(d) <- x@train@Occurrence@index@ID
  } else if (inherits(x,'.singleSpeciesData')) {
    d <- data.frame(x@Occurrence@Occurrence,x@Features@features)
    colnames(d) <- c(x@Occurrence@species.name,x@Features@featureNames)
    rownames(d) <- x@Occurrence@index@ID
  } else if (inherits(x,'.multipleSpeciesData')) {
    d <- data.frame(x@Occurrence@Occurrence,x@Features@features)
    colnames(d) <- c(x@Occurrence@species.names,x@Features@featureNames)
    rownames(d) <- x@Occurrence@index@ID
  } else if (inherits(x,'SpeciesDataList')) {
    d <- data.frame(x@train@SpeciesDataList[[n]]@Occurrence@Occurrence,x@train@SpeciesDataList[[n]]@Features@features)
    colnames(d) <- c(x@train@SpeciesDataList[[n]]@Occurrence@species.name,x@train@SpeciesDataList[[n]]@Features@featureNames)
    rownames(d) <- x@train@SpeciesDataList[[n]]@Occurrence@index@ID
  } else if (inherits(x,'.SpeciesDataList')) {
    d <- data.frame(x@SpeciesDataList[[n]]@Occurrence@Occurrence,x@SpeciesDataList[[n]]@Features@features)
    colnames(d) <- c(x@SpeciesDataList[[n]]@Occurrence@species.name,x@SpeciesDataList[[n]]@Features@featureNames)
    rownames(d) <- x@SpeciesDataList[[n]]@Occurrence@index@ID
  } else if (inherits(x,'data.frame')) {
    d <- x
  }
  d
}
#---------



#----------

.getSpeciesIndex <- function(x,n=1) {
  d <- NA
  if (inherits(x,'singleSpecies') || inherits(x,'multipleSpecies')) {
    d <- x@train@Occurrence@index@ID
  } else if (inherits(x,'.singleSpeciesData') || inherits(x,'.multipleSpeciesData')) {
    d <- x@Occurrence@index@ID
  } else if (inherits(x,'SpeciesDataList')) {
    d <- x@train@SpeciesDataList[[n]]@Occurrence@index@ID
  } else if (inherits(x,'.SpeciesDataList')) {
    d <- x@SpeciesDataList[[n]]@Occurrence@index@ID
  } else if (inherits(x,'data.frame')) {
    d <- 1:nrow(x)
  } else if (inherits(x,'sdmSettings')) {
    if (!is.null(x@basicSettings@replicates)) {
      w <- which(c('cross-balidation','subsampling') %in% names(x@basicSettings@replicates@row.index))
      if (any(w)) {
        w <- c('cross-balidation','subsampling')[w][1]
        d <- x@basicSettings@replicates@row.index[[w]][,1]
      }
    }
  }
  d
}

#----------

.getBasicSetting <- function(p) {
  n <- names(p)
  w <- n %in% c('formula','interaction.depth','test.percent','replicate.method','pseudo.method','pseudo.n','var.importance','var.selection')
  if (any(w)) {
    bs <- new('.basicSettings')
    w <- n[which(w)]
    for (i in seq_along(w)) {
      if (w[i] == 'formula') {
        if ('data' %in% n) {
          if ('interaction.depth' %in% n) int.d <- p[['interaction.depth']]
          else int.d <- 1
          ex <- .featExt(p[['data']],p[['formula']],int.d)
          bs@featureNames <- c(ex$nf,ex$nFact)
          bs@feature.types <- ex$features
          bs@interaction.depth <- int.d
          bs@formula <- .genFormula(ex$features,paste(ex$nsp,collapse='+'))
        } else stop('formula is used but data is not specified...!')
      }
      else if (w[i] == 'replicate.method') {
        if ('sdmSetting' %in% n && inherits(p[['sdmSetting']],'sdmSettings') && any(is.na(.getSpeciesIndex(p[['sdmSetting']])))) {
          if (!'test.percent' %in% n) test.percent <- 30
          else test.percent <- p[['test.percent']]
          if (!'cv.folds' %in% n) cv.folds <- 5
          else cv.folds <- p[['cv.folds']]
          if (!'replicates' %in% n) replicates <- 1
          else replicates <- p[['replicates']]
          rpl <- .replicate(.getSpeciesIndex(p[['sdmSetting']]),p[['replicate.method']],replicates,cv.folds,test.percent)
          bs@basicSettings@replicates <- rpl
          bs@basicSettings@test.percentage <- test.percent
        } else if ('data' %in% n) {
          if (!'test.percent' %in% n) test.percent <- 30
          else test.percent <- p[['test.percent']]
          if (!'cv.folds' %in% n) cv.folds <- 5
          else cv.folds <- p[['cv.folds']]
          if (!'replicates' %in% n) replicates <- 1
          else replicates <- p[['replicates']]
          rpl <- .replicate(.getSpeciesIndex(data),p[['replicate.method']],replicates,cv.folds,test.percent)
          bs@replicates <- rpl
          bs@test.percentage <- test.percent
        } else {
          stop('to set the replications, data (or a sdmSettings object) is needed...')
        }
      }
      else if (w[i] == 'test.percent' && !'replicate.method' %in% n) {
        bs@basicSettings@test.percentage <- p[['test.percent']]
      } else if (w[i] == 'pseudo.method') bs@pseudo.absence.method <- p[['pseudo.method']]
      else if (w[i] == 'pseudo.n') bs@pseudo.absence.number <- p[['pseudo.n']]
      else if (w[i] == 'var.importance') bs@variable.importance <- p[['var.importance']]
      else if (w[i] == 'var.selection') bs@variable.selection <- p[['var.selection']]
    } 
  } else bs <- NULL
  bs
}
#---------

.featExt <- function(d,formula,interaction.depth) {
  d <- .species2df(d)
  ex <- .exformula(formula,d)
  
  if (!all(c(ex$nf,ex$nFact) %in% colnames(d))) stop('data does not contain one or more variable(s) specified in the formula!')
  
  if (length(ex$nsp) > 0) {
    w <- which(ex$nsp %in% colnames(d))
    if (length(w) > 0)  {
      ex$nsp <- ex$nsp[w]
    } else stop('data object does not contain the species data specified in the formula!')
  }
  
  #s@basicSettings@featureNames <- c(ex$nf,ex$nFact)
  
  #--
  if (interaction.depth > 1 & length(c(ex$nf,ex$nFact)) > 1) {
    w <- .interaction.generate(c(ex$nf,ex$nFact),interaction.depth)
    for (i in seq_along(ex$features)) {
      for (j in seq_along(ex$features[[i]])) {
        f <- ex$features[[i]][j]
        if (any(!is.na(pmatch(c("interaction@"),f)))) {
          ww <- strsplit(f,'@')[[1]][-1]
          ww <- which(unlist(lapply(lapply(w,function(x) sort(strsplit(x,'@')[[1]][-1])),function(x) all(x %in% ww))))
          if (length(ww) > 0) w <- w[-ww]
        }
      }
    }
    n <- names(ex$features)
    for (i in seq_along(w)) {
      ww <- which(n == strsplit(w[i],'@')[[1]][2])
      ex$features[[ww]] <- c(ex$features[[ww]],w[i])
    }
  }
  ex
}

#---------
.interaction.generate <- function(n,depth) {
  if (depth > length(n)) depth <- length(n)
  out <- c()
  for (i in 2:depth) {
    co <- combn(n,i)
    for (j in 1:ncol(co)) out <- c(out,paste('interaction@',paste(co[,j],collapse='@'),sep=''))
  }
  out
}
#-----------


.pseudo <- function(p,predictors,n=1000,method='random',r=3) {
  method <- tolower(method)
  w <- which(unlist(lapply(method,function(x) {  any(!is.na(pmatch(c("ran"),x))) | 
                                                   any(!is.na(pmatch(c("geo"),x))) | 
                                                   any(!is.na(pmatch(c("bio"),x))) | 
                                                   any(!is.na(pmatch(c("ens"),x))) |
                                                   any(!is.na(pmatch(c("dis"),x)))
  })))
  if (length(w) == 0) stop('method is not recognized!')
  else method <- method[w]
  
  out <- list()
  rm <- rep(method,each=r)
  for (i in seq_along(rm)) {
    if (any(!is.na(pmatch(c("ran"),rm[i])))) rm[i] <- 'random'
    else if (any(!is.na(pmatch(c("bio"),rm[i])))) rm[i] <- 'bioclim'
    else if (any(!is.na(pmatch(c("env"),rm[i])))) rm[i] <- 'envdist'
    else if (any(!is.na(pmatch(c("geo"),rm[i])))) rm[i] <- 'geodist'
    else if (any(!is.na(pmatch(c("ens"),rm[i])))) rm[i] <- 'ensemble'
    else if (any(!is.na(pmatch(c("dis"),rm[i])))) rm[i] <- 'envdist'
  }
  
  ps <- new('.pseudo.absence')
  ps@method <- rm
  ps@n.replicates <- r
  ps@featureNames <- names(predictors)
  
  for (i in seq_along(rm)) {
    if (rm[i] == 'random') {
      s <- sampleRandom(predictors,n,cells=TRUE,xy=TRUE)
      if(!missing(p)) {
        p.cells <- cellFromXY(predictors,p)
        s <- s[which(!s[,'cell'] %in% p.cells),]
      }
      
      ps@coord.names <- colnames(xyFromCell(predictors,s[1:2,'cell']))
      ps@pseudo.absence[[i]] <- s[,-which(colnames(s) == 'cell')]
      
    } else if (rm[i] == 'envdist') {
      
    }
  }
  ps
}



#-----------

cv.folds <- function(x,k=5,r=1) {
  # based on the function of cvFolds in the package cvTools (Author: Andreas Alfons)
  if (length(x) == 1) x <- n <- round(x,0)
  else n <- length(x)
  if (k > n) {
    k <- n
    warning('k cannot be greater than the number of observation! It is changed to the number of observation, which is equivalent to leave one out...')
  }
  r <- round(r,0)
  if (r <=0) r <- 1
  list(fold=rep(seq_len(k), length.out =n),row.index=replicate(r, sample(x)))
}
#-----------

.replicate <- function(x,method='cv',r=1,k=5,test.percent=NULL) {
  method <- tolower(method)
  w <- which(unlist(lapply(method,function(x) {  any(!is.na(pmatch(c("cros"),x))) | 
                                                   any(!is.na(pmatch(c("cv"),x))) | 
                                                   any(!is.na(pmatch(c("bo"),x))) | 
                                                   any(!is.na(pmatch(c("sub"),x)))
  })))
  if (length(w) == 0) stop('method is not recognized!')
  else method <- method[w]
  
  out <- list()
  rm <- method
  for (i in seq_along(rm)) {
    if (any(!is.na(pmatch(c("cros"),rm[i])))) rm[i] <- 'cross-validation'
    else if (any(!is.na(pmatch(c("cv"),rm[i])))) rm[i] <- 'cross-validation'
    else if (any(!is.na(pmatch(c("bo"),rm[i])))) rm[i] <- 'bootstrap'
    else if (any(!is.na(pmatch(c("sub"),rm[i])))) rm[i] <- 'subsampling'
  }
  
  if (r <= 0) r <- 1
  r <- round(r,0)
  
  ps <- new('.replicates')
  ps@method <- rm
  ps@n.replicates <- r
  
  rm <- unique(rm)
  for (i in seq_along(rm)) {
    if (rm[i] == 'cross-validation') {
      if (k > length(x)) {
        k <- length(x)
        warning('k cannot be greater than the number of observations! It is changed to the number of observations (i.e., leave one out)')
      }
      ps@fold[[rm[i]]] <- rep(seq_len(k), length.out=length(x))
      ps@row.index[[rm[i]]] <- replicate(r, sample(x))
    } else if (rm[i] == 'bootstrap') {
      ps@row.index[[rm[i]]] <- replicate(r, sample(x,replace=TRUE))
      ps@fold[[rm[i]]] <- list()
      for (j in 1:ncol(ps@row.index[[rm[i]]])) {
        w <- x %in% ps@row.index[[rm[i]]][,j]
        ps@fold[[rm[i]]][[j]] <- ifelse(w,1,2)
      }
    } else if (rm[i] == 'subsampling') {
      if (is.null(test.percent)) test.percent <- 0.3
      else if (test.percent > 100) {
        warning('test percentage should be less than 100, it is changed to the default, 30!')
        test.percent <- 0.3
      }
      
      if (test.percent > 1) test.percent <- test.percent / 100
      test.nr <- ceiling(length(x) * test.percent)
      f <- rep(1:2,times=c((length(x)-test.nr),test.nr))
      ps@fold[[rm[i]]] <- f
      ps@row.index[[rm[i]]] <- replicate(r, sample(x))
    }
  }
  ps
}
#------
.ftype_names <- function(x) {
  xo <- c()
  for (i in seq_along(x)) {
    for (j in seq_along(x[[i]])) {
      xo <- c(xo,strsplit(x[[i]][j],'@')[[1]][1])
    }
  }
  unique(xo)
}
#------

if (!isGeneric('.updateModel<-')) {
  setGeneric('.updateModel<-', function(x,value)
    standardGeneric('.updateModel<-'))
}
#---


setReplaceMethod('.updateModel','list', 
                 function(x,value) {
                   n <- value[[1]]
                   k <- value[[3]]
                   i <- value[[4]]
                   success <- TRUE
                   if (inherits(value[[2]][[n]], "try-error")) {
                     value[[2]][[n]] <- NA
                     success <- FALSE
                   }
                   x[[k]]@n.run <- as.integer(x[[k]]@n.run + 1)
                   x[[n]]@run.success[i] <- success
                   x[[n]]@modelObjectList[[i]] <- value[[2]][[n]]
                   if(!is.null(value[[5]])) x[[n]]@evaluationList[[i]] <- value[[5]][[n]]
                   if(!is.null(value[[6]])) x[[n]]@independent.evaluationList[[i]] <- value[[6]][[n]]
                   x
                 }
)


#-----------

#------------
if (!isGeneric("sdmSetting")) {
  setGeneric("sdmSetting", function(formula,data,methods,...)
    standardGeneric("sdmSetting"))
}


setMethod('sdmSetting', signature(formula='missing','speciesData','character'), 
          function(formula,data,methods,interaction.depth=1,replicates=1,replicate.method=NULL,
                   cv.folds=NULL,test.percent=NULL,pseudo.method=NULL,pseudo.n=NULL,...) {
            s <- .createModelSetting(models)
            s@basicSettings@interaction.depth <- interaction.depth
            s@basicSettings@replicates <- replicates
            s@basicSettings@replication.type <- replication.type
            s@basicSettings@test.percentage <- test.percent
            s@basicSettings@pseudo.absence.method <- pseudo.method
            s@basicSettings@pseudo.absence.number <- pseudo.n
            s
          }
)
#---------

setMethod('sdmSetting', signature(formula='formula','speciesData','character'), 
          function(formula,data,methods,interaction.depth=1,replicates=1,replicate.method=NULL,
                   cv.folds=NULL,test.percent=NULL,pseudo.method=NULL,pseudo.n=NULL,var.importance=TRUE,
                   var.selection=FALSE,...) {
            s <- .createModelSetting(methods)
            ex <- .featExt(data,formula,interaction.depth)
            s@basicSettings@featureNames <- c(ex$nf,ex$nFact)
            s@basicSettings@feature.types <- ex$features
            s@basicSettings@formula <- .genFormula(ex$features,ex$nsp)
            
            s@basicSettings@interaction.depth <- interaction.depth
            
            if (!is.null(replicate.method)) {
              replicate.method <- tolower(replicate.method)
              if (is.null(test.percent)) test.percent <- 30
              if (is.null(cv.folds)) cv.folds <- 5
              #if (any(!is.na(pmatch(c("cros"),replicate.method))) | any(!is.na(pmatch(c("cv"),replicate.method))))
              rpl <- .replicate(data@train@Occurrence@index@ID,replicate.method,replicates,cv.folds,test.percent)
              rpl@index.name <- data@train@Occurrence@index@index.name
              s@basicSettings@replicates <- rpl
              s@basicSettings@test.percentage <- test.percent
            } else if (!is.null(test.percent)) {
              rpl <- .replicate(data@train@Occurrence@index@ID,'sub',replicates,test.percent = test.percent)
              s@basicSettings@replicates <- rpl
              s@basicSettings@test.percentage <- test.percent
            }
            
            s@basicSettings@pseudo.absence.method <- pseudo.method
            s@basicSettings@pseudo.absence.number <- pseudo.n
            p <- list(...)
            n <- tolower(names(p))
            names(p) <- n
            
            if ('glm' %in% n) {
              if (!inherits(p[['glm']],'list')) warning('setting for glm model should be a list. 
                                                        glm argument is ignored, you can use glmSetting to update this sdmSetting...')
              else {
                p[['glm']]$data <- data
                w <- try(do.call('glmSetting',p[['glm']]),silent=TRUE)
                if (!inherits(w, "try-error")) sdmSetting(s) <- w
                else warning('specific setting for glm is ignored due to error in parameter matching, you can use glmSetting to update this sdmSetting...')
              }
            }
            #---
            if ('brt' %in% n) {
              if (!inherits(p[['brt']],'list')) warning('setting for brt model should be a list.
                                                        brt argument is ignored, you can use brtSetting to update this sdmSetting...')
              else {
                p[['brt']]$data <- data
                w <- try(do.call('brtSetting',p[['brt']]),silent=TRUE)
                if (!inherits(w, "try-error")) sdmSetting(s) <- w
                else warning('specific setting for brt is ignored due to error in parameter matching, you can use brtSetting to update this sdmSetting...')
              }
            }
            #---
            if ('rf' %in% n) {
              if (!inherits(p[['rf']],'list')) warning('setting for rf model should be a list.
                                                       rf argument is ignored, you can use rfSetting to update this sdmSetting...')
              else {
                p[['rf']]$data <- data
                w <- try(do.call('rfSetting',p[['rf']]),silent=TRUE)
                if (!inherits(w, "try-error")) sdmSetting(s) <- w
                else warning('specific setting for rf is ignored due to error in parameter matching, you can use rfSetting to update this sdmSetting...')
              }
            }
            #----
            s
            }
)
#--------------

if (!isGeneric('sdmSetting<-')) {
  setGeneric('sdmSetting<-', function(x,value)
    standardGeneric('sdmSetting<-'))
}

#---

setReplaceMethod('sdmSetting','ANY', 
                 function(x,value) {
                   if (inherits(value,'.glmSettings')) {
                     if (!'glm' %in% x@methods) x@methods <- c(x@methods,'glm')
                     x@modelSettings$glm <- value
                   } else if (inherits(value,'.brtSettings')) {
                     if (!'brt' %in% x@methods) x@methods <- c(x@methods,'brt')
                     x@modelSettings$brt <- value
                   } else if (inherits(value,'.rfSettings')) {
                     if (!'rf' %in% x@methods) x@methods <- c(x@methods,'rf')
                     x@modelSettings$rf <- value
                   } else if (inherits(value,'.svmSettings')) {
                     if (!'svm' %in% x@methods) x@methods <- c(x@methods,'svm')
                     x@modelSettings$svm <- value
                   } else if (inherits(value,'.gamSettings')) {
                     if (!'gam' %in% x@methods) x@methods <- c(x@methods,'gam')
                     x@modelSettings$gam <- value
                   }
                   x
                 }
)

#--------------

if (!isGeneric("glmSetting")) {
  setGeneric("glmSetting", function(family,...)
    standardGeneric("glmSetting"))
}

setMethod('glmSetting', signature(family='ANY'), 
          function(family,...) {
            s <- new('.glmSettings')
            if (!missing(family)) s@family <- family
            p <- list(...)
            n <- names(p)
            n <- .paramFix(n)
            names(p) <- n
            
            if ('replicates' %in% n && !'replicate.method' %in% n) {
              if (p[['replicates']] > 1) {
                n <- c(n,'replicate.method')
                p[['replicate.method']] <- 'subsampling'
              } 
            }
            if ('test.percent' %in% n && !'replicate.method' %in% n) {
              if (p[['test.percent']] > 0) {
                n <- c(n,'replicate.method')
                p[['replicate.method']] <- 'subsampling'
                if (!'replicates' %in% n) {
                  n <- c(n,'replicates')
                  p[['replicates']] <- 1
                }
              }
            }
            bs <- .getBasicSetting(p)
            if (!is.null(bs)) s@basicSettings <- bs
            
            w <- which(n %in% c('weights','subset','na.action','start','etastart','mustart','offset','control','model','method','x','y','contrasts'))
            if (any(w)) s@otherSettings <- p[w]
            w <- which(n %in% c('predict'))
            if (any(w)) {
              if(is.list(p[w])) {
                if ('type' %in% names(p[w])) s@predictSettings <- p[w]
                else s@predictSettings <- c(s@predictSettings,p[w])
              }
            } 
            s
          }
)
######################

.mahal <- function(d1,d2) {
  co <- solve(cov(d1))
  mahalanobis(d2,colMeans(d1),co,inverted=TRUE)
}
#----------
.checkFactor <- function(f1,f2) {
  l <- levels(f2)[!levels(f2) %in% levels(f1)]
  ww <- NULL
  if (length(l) > 0) {
    w1 <- w2 <- ww <- list()
    for (i in seq_along(l)) w1[[l[i]]] <- which(f2 == l[i])
    l2 <- levels(f2)[levels(f2) %in% levels(f1)]
    for (i in seq_along(l2)) w2[[l2[i]]] <- which(f2 == l2[i])
    ww[['p']] <- w1
    ww[['np']] <- w2
  }
  ww
}


#----------------
.factorFix <- function(data1,data2,nFact,nf) {
  # assign the problematic factors to the more similar factors according to continuous variables
  # if no continus variable does exist, then it is assigned to a dominant class.
  if (missing(nFact) || is.null(nFact)) nFact <- colnames(data2)[.where(is.factor,data2)]
  if (missing(nf) || is.null(nf)) {
    nf <- colnames(data2)[!colnames(data2) %in% nFact]
    if (length(nf) == 0) nf <- NULL
  }
  
  dd <- data2
  
  for (i in seq_along(nFact)) {
    data1[,nFact[i]] <- factor(data1[,nFact[i]])
    data2[,nFact[i]] <- factor(data2[,nFact[i]])
    dd[,nFact[i]] <- as.character(dd[,nFact[i]])
    
    fc <- .checkFactor(data1[,nFact[i]],data2[,nFact[i]])
    if (!is.null(fc)) {
      p <- names(fc$p)
      np <- names(fc$np)
      for (j in seq_along(p)) {
        w <- fc[['p']][[p[[j]]]]
        if (!is.null(nf)) {
          d2 <- data2[w,which(colnames(data2) %in% nf)]
          m <- rep(NA,length(np))
          for (k in seq_along(np)) {
            ww <- fc[['np']][[np[[k]]]]
            d1 <- data2[ww,which(colnames(data2) %in% nf)]
            m[k] <- mean(try(.mahal(d1,d2),silent=TRUE),na.rm=TRUE)
          }
          ww <- which.min(m)
          if (length(ww) > 0) dd[w,nFact[i]] <- np[ww]
          else {
            dom.class <- summary(data1[,nFact[i]])
            dom.class <- attr(sort(dom.class, decreasing = TRUE)[1], "names")
            dd[w,nFact[i]] <- dom.class
          }
        } else {
          dom.class <- summary(data1[,nFact[i]])
          dom.class <- attr(sort(dom.class, decreasing = TRUE)[1], "names")
          dd[w,nFact[i]] <- dom.class
        }
        
      }
      
    }
  }
  for (i in seq_along(nFact)) dd[,nFact[i]] <- factor(dd[,nFact[i]])
  dd
}
#---------------

.eqFactLevel <- function(data1,data2) {
  nFact <- colnames(data2)[.where(is.factor,data2)]
  for (i in seq_along(nFact)) data2[,nFact[i]] <- factor(data2[,nFact[i]],levels = levels(data1[,nFact[i]]))
  data2
}
############################


#-------------


.paramCheck <- function(x) {
  n <- names(x)
  n <- .paramFix(n)
  names(x) <- n
  if (!'interaction.depth' %in% n) x$'interaction.depth' <-1
  if (!'replicates' %in% n) x$'replicates' <-1
  if (!'replicate.method' %in% n) x$'replicate.method' <-NULL
  if (!'cv.folds' %in% n) x$'cv.folds' <-NULL
  if (!'test.percent' %in% n) x$'test.percent' <-NULL
  if (!'pseudo.method' %in% n) x$'pseudo.method' <-NULL
  if (!'pseudo.n' %in% n) x$'pseudo.n' <-NULL
  if (!'var.importance' %in% n) x$'var.importance' <-TRUE
  if (!'var.selection' %in% n) x$'var.selection' <-FALSE
  x
}
#-----

.createSModels <- function (model) {
  switch(model,
         glm=new('.sdmModels',model='glm'),
         brt=new('.sdmModels',model='brt'),
         rf=new('.sdmModels',model='rf'),
         svm=new('.sdmModels',model='svm'),
         gam=new('.sdmModels',model='gam')
  )
}
#----------

.getRunIndex <- function(x,r) {
  if (as.vector(r[1,1]) == 'cross-validation') {
    w <- which(x@fold[[ as.vector(r[1,1])]] !=  as.vector(r[1,2]))
  } else w <- which(x@fold[[ as.vector(r[1,1])]] ==  as.vector(r[1,2]))
  x@row.index[[as.vector(r[1,1])]][w,as.vector(r[1,3])]
}
.getTestIndex <- function(x,r) {
  if (as.vector(r[1,1]) == 'cross-validation') {
    w <- which(x@fold[[ as.vector(r[1,1])]] ==  as.vector(r[1,2]))
  } else w <- which(x@fold[[ as.vector(r[1,1])]] !=  as.vector(r[1,2]))
  x@row.index[[as.vector(r[1,1])]][w,as.vector(r[1,3])]
}
#-----
.runScenarios <- function(s) {
  runs <- data.frame(matrix(nrow=0,ncol=3))
  colnames(runs) <- c('run_scenarios','fold','replicate')
  m <- s@method
  r <- s@n.replicates
  for (i in seq_along(m)) {
    if (m[i] == 'cross-validation') {
      f <- unique(s@fold[[m[i]]])
      runs <- rbind(runs,data.frame(run_scenarios=rep(m[i],r*length(f)),fold=rep(f,r),replicate=rep(1:r,each=length(f))))
    } else {
      runs <- rbind(runs,data.frame(run_scenarios=rep(m[i],r),fold=rep(1,r),replicate=1:r))
    }
  }
  runs
}

#-----
.showScenarios <- function(x) {
  s <- unique(x$run_scenarios)
  sc <- data.frame(matrix(nrow=length(s),ncol=3))
  colnames(sc) <- c('scenarios','start','end')
  sc[,1] <- s
  for (i in seq_along(s)) {
    sc[i,2:3] <- range(which(x[,1] == s[i]))
  }
  sc
}
#################

.statFixname <- function(x) {
  x <- tolower(x)
  for (i in seq_along(x)) {
    if (any(!is.na(pmatch(c("se"),x[i])))) x[i] <- 'sensitivity'
    else if (any(!is.na(pmatch(c("sp"),x[i])))) x[i] <- 'specificity'
    else if (any(!is.na(pmatch(c("ts"),x[i])))) x[i] <- 'TSS'
    else if (any(!is.na(pmatch(c("ka"),x[i])))) x[i] <- 'kappa'
    else if (any(!is.na(pmatch(c("nm"),x[i])))) x[i] <- 'NMI'
    else if (any(!is.na(pmatch(c("pp"),x[i])))) x[i] <- 'ppv'
    else if (any(!is.na(pmatch(c("np"),x[i])))) x[i] <- 'npv'
    else if (any(!is.na(pmatch(c("cc"),x[i])))) x[i] <- 'ccr'
    else if (any(!is.na(pmatch(c("mc"),x[i])))) x[i] <- 'mcr'
    else if (any(!is.na(pmatch(c("or"),x[i])))) x[i] <- 'or'
    else if (any(!is.na(pmatch(c("om"),x[i])))) x[i] <- 'ommission'
    else if (any(!is.na(pmatch(c("com"),x[i])))) x[i] <- 'commission'
    else if (any(!is.na(pmatch(c("pr"),x[i])))) x[i] <- 'predicted.prevalence'
    else if (any(!is.na(pmatch(c("cor"),x[i])))) x[i] <- 'cor'
    else if (any(!is.na(pmatch(c("auc"),x[i])))) x[i] <- 'auc'
  }
  x <- unique(x)
  w <- which(x %in% c('sensitivity','specificity','TSS','Kappa','NMI','ppv','npv','ccr','mcr','or','ommission','commission','predicted.prevalence','cor','auc'))
  x <- x[w]
  
  x
}



.getPerformanceStat <- function(e,stat,opt=2) {
  stat <- .statFixname(stat)
  stat <- tolower(stat)
  switch(stat,
         auc=e@AUC,
         cor=e@COR,
         tss=e@threshod_based[opt[1],5],
         nmi=e@threshod_based[opt[1],7],
         ppv=e@threshod_based[opt[1],8],
         npv=e@threshod_based[opt[1],9],
         ccr=e@threshod_based[opt[1],10],
         predicted.prevalence=e@threshod_based[opt[1],11],
         sensitivity=e@threshod_based[opt[1],3],
         specificity=e@threshod_based[opt[1],4],
         threshold=e@threshod_based[opt[1],2]
  )
}

.is.empty <- function(x) {
  if(length(x) == 0) return(TRUE)
  else return(FALSE)
}

.getPerformance <- function(model,methods,species=1,stat,opt=2,independent=FALSE) {
  model <- model@models@multiModelList[[species]]
  if (missing(methods)) methods <- names(model@modelList)
  if (missing(stat)) stat <- c('AUC','COR','TSS')
  stat <- .statFixname(stat)
  out <- vector('list',length(methods))
  names(out) <- methods
  #if (!independent && .is.empty(model@modelList[[methods[1]]]@evaluationList)) independent <- TRUE
  #if (independent && .is.empty(model@modelList[[methods[1]]]@independent.evaluationList)) independent <- FALSE
  
  
  for (m in methods) {
    if (!.is.empty(model@modelList[[m]]@evaluation@evaluationList)) {
      w <- length(model@modelList[[m]]@evaluation@evaluationList)
      o <- matrix(nrow=w,ncol=length(stat))
      colnames(o) <- stat
      for (i in 1:w) {
        o[i,] <- unlist(lapply(stat,function (x) .getPerformanceStat(model@modelList[[m]]@evaluation@evaluationList[[i]],x,opt)))
      }
      out[[m]] <- o
    } else out[[m]] <- NULL
    
  }
  out
}


######################

.sdmFit <- function(data,setting) {
  nFact <- nf <- NULL
  s <- new('sdmModel',data=data,settings=setting)
  rm(data,setting)
  s@models@species.names <- names(s@data)
  d <- .species2df(s@data)
  d <- data.frame(d[,1],.model.frame(d,s@settings@basicSettings@feature.types))
  colnames(d)[1] <- s@models@species.names
  
  if ('factor' %in% .ftype_names(s@settings@basicSettings@feature.types)) {
    nf <- c()
    for (f in s@settings@basicSettings@featureNames) {
      if ('linear' %in% s@settings@basicSettings@feature.types[[f]]) nf <- c(nf,f)
    }
    nFact <- .where(is.factor,d)
    nFact <- names(nFact[nFact])
  }
  
  
  if (!is.null(s@data@test)) {
    dT <- .species2df(s@data@test)
    dT <- data.frame(dT[,1],.model.frame(dT,s@settings@basicSettings@feature.types))
    colnames(dT)[1] <- s@models@species.names
    if (!is.null(nFact)) {
      dT <- .factorFix(d,dT,nFact,nf)
      dT <- .eqFactLevel(d,dT)
    }
  }
  
  names(s@settings@methods) <- s@settings@methods
  
  sMo <- lapply(s@settings@methods,.createSModels)
  
  funs <- lapply(s@settings@methods,function(x) get(paste('.',x,'Fit',sep='')))
  funsP <- lapply(s@settings@methods,function(x) get(paste('.',x,'Predict',sep='')))
  
  options(warn=-1)
  
  if (!is.null(s@settings@basicSettings@replicates)) {
    runs <- .runScenarios(s@settings@basicSettings@replicates)
    e_sc <- data.frame(runIndex=as.integer(1:nrow(runs)),evalDependent=integer(nrow(runs)),evalIndependent=integer(nrow(runs)) )
    
    for (i in seq_along(sMo)) {
      sMo[[i]]@modelObjectList <- vector('list',nrow(runs))
      sMo[[i]]@run.index <- as.integer(1:nrow(runs))
      sMo[[i]]@run.success <- logical(nrow(runs))
      sMo[[i]]@evaluation@model <- sMo[[i]]@model
      sMo[[i]]@evaluation@n.run <- nrow(runs)
      sMo[[i]]@evaluation@scenarios <- e_sc
    }
    rm(e_sc)
    for (i in 1:nrow(runs)) {
      run.ID <- .getRunIndex(s@settings@basicSettings@replicates,runs[i,])
      test.ID <- .getTestIndex(s@settings@basicSettings@replicates,runs[i,])
      
      if (!is.null(nFact)) {
        d2 <- .factorFix(d[as.character(run.ID),],d[as.character(test.ID),],nFact,nf)
        d2 <- .eqFactLevel(d[as.character(run.ID),],d2)
      } else  d2 <- d[as.character(test.ID),]
      
      m <- lapply(funs,function(f,...) try(f(d[as.character(run.ID),],...),silent=TRUE),s=s@settings@modelSettings)
      m2 <- lapply(funsP,function(f,...) try(f(d2,...),silent=TRUE),s=s@settings@modelSettings,models=m)
      e <- lapply(s@settings@methods,function(x) {try(.evaluate(d2[,1],as.vector(m2[[x]])),silent=TRUE)})
      e <- lapply(e,function(x) {if (inherits(x,'try-error')) NULL else x} )
      if (!is.null(s@data@test)) {
        mT <- lapply(funsP,function(f,...) try(f(dT,...),silent=TRUE),s=s@settings@modelSettings,models=m)
        e2 <- lapply(s@settings@methods,function(x) {try(.evaluate(dT[,1],mT[[x]]),silent=TRUE)})
        e2 <- lapply(e2,function(x) {if (inherits(x,'try-error')) NULL else x} )
      } else e2 <- NULL
      
      for (j in seq_along(s@settings@methods)) {
        success <- TRUE
        if (inherits(m[[j]], "try-error")) {
          m[[j]] <- NA
          success <- FALSE
        }
        sMo[[j]]@n.run <- as.integer(sMo[[j]]@n.run + 1)
        sMo[[j]]@run.success[i] <- success
        sMo[[j]]@modelObjectList[[i]] <- m[[j]]
        if (is.null(e[[j]])) {
          sMo[[j]]@evaluation@scenarios[i,2] <- NA
        } else {
          sMo[[j]]@evaluation@scenarios[i,2] <- length(sMo[[j]]@evaluation@evaluationList) + 1
          sMo[[j]]@evaluation@evaluationList[[sMo[[j]]@evaluation@scenarios[i,2]]] <- e[[j]]
        }
        
        if (!is.null(e2) && !is.null(e2[[j]])) {
          sMo[[j]]@evaluation@scenarios[i,3] <- length(sMo[[j]]@evaluation@evaluationList) + 1
          sMo[[j]]@evaluation@evaluationList[[sMo[[j]]@evaluation@scenarios[i,3]]] <- e2[[j]]
        } else {
          sMo[[j]]@evaluation@scenarios[i,3] <- NA
        }
      }
    }
    #for (i in seq_along(sMo)) sMo[[i]]@scenarios <- .showScenarios(runs) 
    for (i in seq_along(sMo)) sMo[[i]]@scenarios <- runs 
    rm(m,m2,run.ID,i,runs,e,e2)
  } else {
    e_sc <- data.frame(runIndex=as.integer(1),evalDependent=integer(1),evalIndependent=integer(1) )
    for (i in seq_along(sMo)) {
      sMo[[i]]@evaluation@model <- sMo[[i]]@model
      sMo[[i]]@evaluation@n.run <- 1
      sMo[[i]]@evaluation@scenarios <- e_sc
    }
    rm(e_sc,i)
    m <- lapply(funs,function(f,...) try(f(d,...),silent=TRUE),s=s@settings@modelSettings)
    if (!is.null(s@data@test)) {
      m2 <- lapply(funsP,function(f,...) try(f(dT,...),silent=TRUE),s=s@settings@modelSettings,models=m)
      e <- lapply(s@settings@methods,function(x) {try(.evaluate(dT[,1],m2[[x]]),silent=TRUE)})
      e <- lapply(e,function(x) {if (inherits(x,'try-error')) NULL else x} )
      rm(dT)
    } else e <- NULL
    
    for (j in seq_along(s@settings@methods)) {
      success <- TRUE
      if (inherits(m[[j]], "try-error")) {
        m[[j]] <- NA
        success <- FALSE
      }
      sMo[[j]]@n.run <- as.integer(sMo[[j]]@n.run + 1)
      sMo[[j]]@run.success[1] <- success
      sMo[[j]]@modelObjectList[[1]] <- m[[j]]
      sMo[[j]]@evaluation@scenarios[1,2] <- NA
      
      if (!is.null(e) && !is.null(e[[j]])) {
        sMo[[j]]@evaluation@scenarios[1,3] <- length(sMo[[j]]@evaluation@evaluationList) + 1
        sMo[[j]]@evaluation@evaluationList[[sMo[[j]]@evaluation@scenarios[1,3]]] <- e[[j]]
      } else {
        sMo[[j]]@evaluation@scenarios[1,3] <- NA
      }
    }
    
    #for (n in seq_along(s@settings@methods)) .updateModel(sMo) <- list(s@settings@methods[n],m,n,1,NULL,e)
    runs <- data.frame(matrix(nrow=1,ncol=3))
    colnames(runs) <- c('run_scenarios','fold','replicate')
    runs[1,1] <- 'full'
    runs[1,2:3] <- 1
    
    for (i in seq_along(sMo)) sMo[[i]]@scenarios <- runs 
    rm(m,runs,j,e)
    
  }
  options(warn=0)
  mMo <- new('.sdmMultiModel',species.name=s@models@species.names,modelList=sMo)
  s@models@multiModelList[[mMo@species.name]] <- mMo
  rm(sMo,mMo,d)
  s
}
#----------------
if (!isGeneric("sdm")) {
  setGeneric("sdm", function(formula,data,methods,...)
    standardGeneric("sdm"))
}

setMethod('sdm', signature(formula='formula',data='singleSpecies',methods='character'), 
          function(formula,data,methods,...) {
            methods <- unique(.methodFix(methods))
            lib <- unlist(lapply(methods,.loadLib))
            if (!any(lib)) stop('the required packages for the specified methods are not installed...!')
            w <- which(!lib)
            if (length(w) > 0) {
              warning(paste('the package for methods',paste(methods[!w],collapse=', '),'are not found. These methods are ignored! After installing the required packages, you can use update function to update the sdm model with specific methods'))
              methods <- methods[w]
            }
            p <- list(...)
            n <- names(p)
            n <- .paramFix(n)
            names(p) <- n
            p <- .paramCheck(p)
            p$data <- data
            p$formula <- formula
            p$methods <- methods
            s <- do.call('sdmSetting',p)
            s <- .sdmFit(data,s)
            rm(data)
            s
          }
)

#################

setMethod('sdm', signature(formula='ANY',data='singleSpecies',methods='sdmSettings'), 
          function(formula,data,methods,...) {
            s <- methods
            methods <- s@methods
            lib <- unlist(lapply(methods,.loadLib))
            if (!any(lib)) stop('the required packages for the specified methods are not installed...!')
            w <- which(!lib)
            if (length(w) > 0) {
              warning(paste('the package for methods',paste(methods[!w],collapse=', '),'are not found. These methods are ignored! After installing the required packages, you can use update function to update the sdm model with specific methods'))
              methods <- methods[w]
              s@methods <- methods
              s@modelSettings <- s@modelSettings[w]
            }
            s <- .sdmFit(data,s)
            rm(data)
            s
          }
)


#---------------------------
