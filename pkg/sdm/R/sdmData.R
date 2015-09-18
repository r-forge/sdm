# Author: Babak Naimi, naimi.b@gmail.com
# Date :  July 2014
# Version 1.1
# Licence GPL v3


#------
.isBinomial <- function(x) {
  if (is.numeric(x)) {
    u <- unique(x)
    if (length(u) > 2) return(FALSE)
    else if (length(u) == 2) return(all(sort(u) == c(0,1)))
    else return(u == 1)
  } else if (is.logical(x)) return(TRUE)
  else {
    x <- as.character(x)
    u <- unique(x)
    if (length(u) > 2) return(FALSE)
    else if (length(u) == 2) return(all(sort(u) == c('0','1')))
    else return(u == '1')
  }
}

#-------
.pseudo_df <- function(df,predictors,n=1000,method='random',spn,nxy=NULL) {
  nn <- colnames(df)
  np <- names(predictors)
  nf <- nn[nn %in% np]
  if (missing(spn)) {
    spn <- nn[!nn %in% np]
    spn <- .excludeVector(spn,nxy)
    l <- length(spn)
    spn <- spn[unlist(lapply(spn,function(x) .isBinomial(df[,x])))]
    if (length(spn) != l) stop('there are some variables in data that are neither predictors nor species!')
  }
  if (method %in% c('random','rand','Random','r','R')) {
    if (!is.null(nxy)) {
      s <- data.frame(sampleRandom(predictors,n,xy=TRUE))
      colnames(s)[1:2] <- nxy
      s <- s[,c(nxy,nf)]
    } else s <- data.frame(sampleRandom(predictors,n))[,nf]
    for (sp in spn) s[[sp]] <- 0
  }
  
  rbind(df,s)
}

#----------
.occurrence <- function(x) {
  if (is.logical(x)) {
    xx <- rep(0,length(x))
    xx[which(x)] <- 1
    return(xx)
  } else if (is.character(x) | is.factor(x)) {
    return(as.numeric(as.character(x)))
  } else return(x)
}
#-------
.singleORmulti <- function(x) {
  # return a list including two items:
  # ...the first item- weather single (1) or multi (2) or not identified (0)
  # ...the second item- fields in the data.frame include species data
  w <- which(unlist(lapply(x,.isBinomial)))
  if (length(w) == 0) return(list(0,0))
  else if (length(w) == 1) return(list(1,w))
  else return(list(2,w))
}

#----
.dataClean <- function(x) {
  rm.na <-0; rm.duplicate <- 0
  w <- nrow(x)
  x <- x[which(!apply(x,1,function(x){any(is.na(x))})),]
  if (nrow(x) < w) rm.na <- w - nrow(x)
  w <- nrow(x)
  x <- unique(x)
  if (nrow(x) < w) rm.duplicate <- w - nrow(x)
  list(x,c(na=rm.na,duplicate=rm.duplicate))
}

#----------
.strIdentical <- function(x,y,n) {
  if (missing(n)) n <- colnames(x)
  t3 <- TRUE
  t1 <- all(unlist(lapply(n,function(z) {z %in% colnames(x)})))
  t2 <- all(unlist(lapply(n,function(z) {z %in% colnames(y)})))
  if (t2) t3 <- all(unlist(lapply(x,class))[n] == unlist(lapply(y,class))[n])
  return(t1&t2&t3)
}
#----------

.int.to.numeric <- function(data) {
  w <- which(unlist(lapply(data,is.integer)))
  if (length(w) > 0) {
    for (i in w) data[,i] <- as.numeric(data[,i])
  }
  data
}
#----------

.fixFormula <- function(formula) {
  if (length(formula) == 3) {
    nsp <- trim(unlist(strsplit(as.character(formula[2]),'[+|]')))
    nsp <- nsp[nsp != '']
    if ('.' %in% nsp) {
      nsp <- nsp[nsp != '.']
      if (length(nsp) == 0) formula <- as.formula(paste(formula[1],formula[3]),env = parent.frame())
      else formula <- as.formula(paste(paste(nsp,collapse='+'),formula[1],formula[3]),env = parent.frame())
    }
  }
  formula
}
#-------
.excludeVector <- function(x,y) {
  w <- unlist(lapply(y,function(z){which(x == z)}))
  x <- x[-w]
  if (length(x) == 0) x <- NULL
  x
}
#-------
.splitFormula <- function(x) {
  trim(unlist(strsplit(strsplit(x,'[()]')[[1]][2],'[+]')))
}
#------
.exformula <- function(formula,data) {
  nxy <- nFact <- NULL
  formula <- .fixFormula(formula)
  varNames <- colnames(data)
  n <- all.vars(formula)
  if (length(formula) == 3) {
    
    nsp <- trim(unlist(strsplit(as.character(formula[2]),'[+]')))
    
    if ('.' %in% n) {
      n <- n[-which(n == '.')]
      n <- unique(c(varNames,n))
    }
    n <- .excludeVector(n,nsp)
    
  } else {
    w <- which(unlist(lapply(data,.isBinomial)))
    if (length(w) > 0) {
      nsp <- varNames[w]
      varNames <- .excludeVector(varNames,nsp)
    } else nsp <- NULL
    
    if ('.' %in% n) {
      n <- n[-which(n == '.')]
      n <- unique(c(varNames,n))
    }
  }
  n2 <- attributes(terms(formula,data=data))$term.labels
  w <- which(unlist(lapply(n2,function(x) {
    any(!is.na(pmatch(c("coords(","coord(","coor(","cor(","cords(","c("),tolower(x))))
  })))
  if (length(w) > 0) {
    nc <- n2[w]
    nxy <- .splitFormula(nc)
    n <- .excludeVector(n,nxy)
  }
  
  w <- which(unlist(lapply(n2,function(x) {
    any(!is.na(pmatch(c("f(","factor(","factors(","fact("),tolower(x))))
  })))
  if (length(w) > 0) {
    for(i in 1:length(w)) {
      nc <- n2[w[i]]
      nFact <- c(nFact,.splitFormula(nc))
    }
    n <- .excludeVector(n,nFact)
  }
  w <- c()
  for (nn in n) {
    if (nn %in% varNames) {
      if (is.factor(data[,nn]) | is.character(data[,nn])) {
        w <- c(w,nn)
        nFact <- c(nFact,nn)
      }
    }
  }
  if (length(w) > 0) n <- .excludeVector(n,w)
  
  features <- list()
  
  if (!is.null(n)) {
    for (nnf in n) {
      if (nnf %in% n2) features[[nnf]] <- c('linear')
    }
  }
  
  if (!is.null(nFact)) {
    for (nnf in nFact) features[[nnf]] <- c('factor')
  }
  
  w <- which(unlist(lapply(n2,function(x) {
    any(!is.na(pmatch(c("I("),x)))
  })))
  if (length(w) > 0) {
    for(i in 1:length(w)) {
      nc <- n2[w[i]]
      I.sec <- trim(unlist(strsplit(strsplit(nc,'[()]')[[1]][2],'[\\^|,|+|*|:]')))
      ww <- which(c('^',',','+','*',':') %in% unlist(strsplit(nc,'')))
      if (length(ww) > 0) {
        if (ww <= 2) {
          if(length(I.sec) == 2) {
            if (!is.na(strtoi(I.sec[2]))) {
              i2 <- strtoi(I.sec[2])
              if (i2 == 2) temp <- 'quadratic'
              else if (i2 == 3) temp <- 'cubic'
              else if (i2 == 1) temp <- 'linear'
              else temp <- paste('power@',i2,sep='')
            } else {
              if (ww == 1) temp <- paste('product@',I.sec[1],'@^@',I.sec[2],sep='')
              else temp <- paste('interaction@',I.sec[1],'@',I.sec[2],sep='')
            }
          }
        } else if (ww == 3) {
          temp <- paste('product@',I.sec[1],'@+@',I.sec[2],sep='')
        } else {
          if (!is.na(strtoi(I.sec[2]))) {
            i2 <- strtoi(I.sec[2])
            temp <- paste('product@',I.sec[1],'@*@',I.sec[2],sep='')
          } else temp <- paste('interaction@',I.sec[1],'@',I.sec[2],sep='')
        }
        features[[I.sec[1]]] <- unique(c(features[[I.sec[1]]],temp)) 
      } else features[[I.sec[1]]] <- unique(c(features[[I.sec[1]]],'linear')) 
      
    }
  }
  
  w <- which(unlist(lapply(n2,function(x) {
    any(!is.na(pmatch(c("poly("),tolower(x))))
  })))
  if (length(w) > 0) {
    for(i in 1:length(w)) {
      nc <- n2[w[i]]
      I.sec <- trim(unlist(strsplit(strsplit(nc,'[()]')[[1]][2],'[\\^|,]')))
      if (!is.na(strtoi(I.sec[2]))) {
        i2 <- strtoi(I.sec[2])
        temp <- paste('poly@',i2,sep='')
      } else temp <- paste('poly@',3,sep='')
      features[[I.sec[1]]] <- unique(c(features[[I.sec[1]]],temp))
    }
  }
  
  w <- which(unlist(lapply(n2,function(x) {
    any(!is.na(pmatch(c("quad(",'quadratic(','q('),tolower(x))))
  })))
  if (length(w) > 0) {
    for(i in 1:length(w)) {
      nc <- n2[w[i]]
      nq <- trim(unlist(strsplit(strsplit(nc,'[()]')[[1]][2],'[+|,]')))
      for (nnq in nq) {
        if (is.na(strtoi(nnq))) {
          features[[nnq]] <- unique(c(features[[nnq]],'quadratic'))
        }
      }
    }
  }
  
  w <- which(unlist(lapply(n2,function(x) {
    any(!is.na(pmatch(c("cubic(",'cub('),tolower(x))))
  })))
  if (length(w) > 0) {
    for(i in 1:length(w)) {
      nc <- n2[w[i]]
      nq <- trim(unlist(strsplit(strsplit(nc,'[()]')[[1]][2],'[+|,]')))
      for (nnq in nq) {
        if (is.na(strtoi(nnq))) {
          features[[nnq]] <- unique(c(features[[nnq]],'cubic'))
        }
      }
    }
  }
  
  w <- which(unlist(lapply(n2,function(x) {
    any(!is.na(pmatch(c("auto(",'a('),tolower(x))))
  })))
  if (length(w) > 0) {
    for(i in 1:length(w)) {
      nc <- n2[w[i]]
      nq <- trim(unlist(strsplit(strsplit(nc,'[()]')[[1]][2],'[+|,]')))
      for (nnq in nq) {
        if (is.na(strtoi(nnq))) {
          features[[nnq]] <- unique(c(features[[nnq]],'auto'))
        }
      }
    }
  }
  
  
  w <- which(unlist(lapply(n2,function(x) {
    any(!is.na(pmatch(c("hinge(",'h('),tolower(x))))
  })))
  if (length(w) > 0) {
    for(i in 1:length(w)) {
      nc <- n2[w[i]]
      nq <- trim(unlist(strsplit(strsplit(nc,'[()]')[[1]][2],'[+|,]')))
      for (nnq in nq) {
        if (is.na(strtoi(nnq))) {
          features[[nnq]] <- unique(c(features[[nnq]],'hinge'))
        }
      }
    }
  }
  
  list(nsp=nsp,nf=n,nFact=nFact,nxy=nxy,features=features)
}



#------



.Extract <- function(x,cells,factors) {
  n <- names(x)
  
  if (length(factors) == 1) {
    x2 <- values(x[[factors]])[cells]
  } else x2 <- values(x[[factors]])[cells,]
  
  if (length(n) > length(factors)) {
    x1 <- x[[-factors]][cells]
    d <- data.frame(x1,x2)
    colnames(d) <- c(n[-factors],n[factors])
  } else {
    d <- data.frame(x2)
    colnames(d) <- n[factors]
  }
  
  for (i in 1:length(factors)) d[,i] <- factor(d[,i])
  return(d )
}

#----------
.catReport <- function(rm.train=c(0,0),rm.test=c(0,0),rm.outside=c(0,0)){
  if (any(c(rm.train,rm.test,rm.outside) > 0)) {
    cat(  '\n')  
    cat('data cleaning report                  : ' ,  '\n')  
    cat('--------------------------------------' ,  '\n')
    if (rm.train[1] > 0)
      cat('--- No. of observations with NA removed from train data    : ' , rm.train[1], '\n')  
    if (rm.test[1] > 0)
      cat('--- No. of observations with NA removed from test data     : ' , rm.test[1], '\n')  
    if (rm.train[2] > 0)
      cat('--- No. of duplicated observations removed from train data : ' , rm.train[2], '\n')  
    if (rm.test[2] > 0)
      cat('--- No. of duplicated observations removed from test data  : ' , rm.test[2], '\n')  
    if (rm.outside[1] > 0)
      cat('--- No. of "outside" observations removed from train data  : ' , rm.outside[1], '\n')  
    if (rm.outside[2] > 0)
      cat('--- No. of "outside" observations removed from test data   : ' , rm.outside[2], '\n')  
    
  }
}

#----------

.df2list <- function(data,nsp,n) {
  # n : c(nf,nFact,nxy)
  spn <- unique(data[,nsp])
  o <- vector("list", length(spn))
  names(o) <- spn
  for (s in spn) {
    df <- data[which(data[,nsp] == s),n]
    df <- data.frame(df,rep(1,nrow(df)))
    colnames(df) <- c(n,s)
    o[[s]] <- df
  }  
  o
}
#--------------

.generateName <- function(x) {
  paste(c(x,'_',sample(c(letters,1:9),6,replace=T)),collapse='')
}
#-----
.create_singleSpeciesData <- function(data,nsp,nf,nFact,nxy,crs) {
  o.ind <- new('.Index')
  o.ind@index.name <- .generateName(nsp)
  o.ind@ID <- 1:nrow(data)
  
  o.oc <- new('.Occurrence')
  o.oc@species.name <- nsp
  o.oc@Occurrence <- .occurrence(data[,nsp])
  names(o.oc@Occurrence) <- o.ind@ID
  o.oc@index <- o.ind
  o.oc@presence.only <- all(o.oc@Occurrence == 1)
  sp.d <- new('.singleSpeciesData')
  
  sp.d@Occurrence <- o.oc
  rm(o.oc)
  
  if (!is.null(nf) | !is.null(nFact)) {
    o.fe <- new('.Features')
    o.fe@featureNames <- c(nf,nFact)
    if (!is.null(nFact)) o.fe@factors <- nFact
    o.fe@features <- data.frame(data[,c(nf,nFact)])
    colnames(o.fe@features) <- c(nf,nFact)
    rownames(o.fe@features) <- o.ind@ID
    o.fe@index <- o.ind
    sp.d@Features <- o.fe
    rm(o.fe)
  }
  
  if (!is.null(nxy)) {
    o.cor <- new('.coords')
    o.cor@coords <- as.matrix(data[,nxy])
    rownames(o.cor@coords) <- o.ind@ID
    o.cor@index <- o.ind
    o.cor@crs <- crs
    sp.d@coordinates <- o.cor
    rm(o.cor)
  }
  sp.d 
}
#------
.createSingleSpecies <- function(train,test,nsp,nf,nFact,nxy,crs) {
  d <- new('singleSpecies')
  d@train <- .create_singleSpeciesData(train,nsp,nf,nFact,nxy,crs)
  if (!is.null(test)) d@test <- .create_singleSpeciesData(test,nsp,nf,nFact,nxy,crs)
  d
}
#-------

.create_multipleSpeciesData <- function(data,nsp,nf,nFact,nxy,crs) {
  
  for (spn in nsp) data[,spn] <- .occurrence(data[,spn])
  
  o.ind <- new('.Index')
  o.ind@index.name <- .generateName('multiple')
  o.ind@ID <- 1:nrow(data)
  
  o.oc <- new('.multipleOccurrence')
  o.oc@species.names <- nsp
  o.oc@Occurrence <- as.matrix(data[,nsp])
  rownames(o.oc@Occurrence) <- o.ind@ID
  o.oc@index <- o.ind
  o.oc@presence.only <- unlist(lapply(data[,nsp],function(x) all(x == 1)))
  sp.d <- new('.multipleSpeciesData')
  
  sp.d@Occurrence <- o.oc
  rm(o.oc)
  
  if (!is.null(nf) | !is.null(nFact)) {
    o.fe <- new('.Features')
    o.fe@featureNames <- c(nf,nFact)
    if (!is.null(nFact)) o.fe@factors <- nFact
    o.fe@features <- data.frame(data[,c(nf,nFact)])
    colnames(o.fe@features) <- c(nf,nFact)
    rownames(o.fe@features) <- o.ind@ID
    o.fe@index <- o.ind
    sp.d@Features <- o.fe
    rm(o.fe)
  }
  
  if (!is.null(nxy)) {
    o.cor <- new('.coords')
    o.cor@coords <- as.matrix(data[,nxy])
    rownames(o.cor@coords) <- o.ind@ID
    o.cor@index <- o.ind
    o.cor@crs <- crs
    sp.d@coordinates <- o.cor
    rm(o.cor)
  }
  sp.d 
}

#-------------
.createMultipleSpecies <- function(train,test,nsp,nf,nFact,nxy,crs) {
  d <- new('multipleSpecies')
  d@train <- .create_multipleSpeciesData(train,nsp,nf,nFact,nxy,crs)
  if (!is.null(test)) {
    w <- unlist(lapply(d@train@Occurrence@species.names,function(x){x %in% colnames(test) }))
    nsp <- d@train@Occurrence@species.names[which(w)]
    d@test <- .create_multipleSpeciesData(test,nsp,nf,nFact,nxy,crs)
  } 
  d
}
#--------------

.create_speciesDatalist <- function(data,nsp,nf,nFact,nxy,crs) {
  d <- new('.SpeciesDataList')
  for (spn in nsp) d@SpeciesDataList[[spn]] <- .create_singleSpeciesData(data[[spn]],spn,nf,nFact,nxy,crs)
  d@species.names <- nsp
  d
}
#-----
.createSpeciesDataList <- function(train,test,nsp,nf,nFact,nxy,crs) {
  d <- new('SpeciesDataList')
  d@train <- .create_speciesDatalist(train,nsp,nf,nFact,nxy,crs)
  if (!is.null(test)) {
    nsp <- names(test)
    d@test <- .create_speciesDatalist(test,nsp,nf,nFact,nxy,crs)
  } 
  d
}
#-----


################

if (!isGeneric("sdmData")) {
  setGeneric("sdmData", function(train, predictors, formula, test, filename,crs,pseudo,size)
    standardGeneric("sdmData"))
}


setMethod('sdmData', signature(train='data.frame',predictors='missing',formula='formula'), 
          function(train, predictors,formula, test=NULL, filename=NULL,crs=NULL,pseudo=FALSE) {
            
            if (missing(pseudo)) stop('to be able to generate pseudo absence, you need to introduce predictors as a raster object')
            if(missing(test)) test <- NULL
            if(missing(filename)) filename <- NULL
            if(missing(crs)) crs <- NULL
            
            nxy <- NULL; rm.train <- c(0,0); rm.test <- c(0,0); nf <- NULL; nFact <- NULL
            
            w <- .dataClean(train)
            train <- w[[1]]
            rm.train <- w[[2]]
            rm(w)
            
            w <- .exformula(formula,train)
            nsp <- w$nsp; nf <- w$nf; nxy <- w$nxy; nFact <- w$nFact
            
            n <- c(nsp,nf,nxy,nFact)
            if (!all(unlist(lapply(n,function(x){x %in% colnames(train) })))) stop('One or more variable specified in the formula does not exist in train!')
            
            if (!is.null(nFact)) {
              for (nn in nFact) train[,nn] <- factor(train[,nn])
            }
            
            train <- .int.to.numeric(train)
            
            if (!is.null(test)) {
              if (!is.null(nFact)) {
                if (!all(unlist(lapply(nFact,function(x){x %in% colnames(test) }))))
                  for (nn in nFact) test[,nn] <- factor(test[,nn])
              }
              test <- .int.to.numeric(test)
              # to check common species in train and test:
              nsp.t <- nsp
              w <- unlist(lapply(nsp,function(x){x %in% colnames(test) }))
              if (!all(w)) {
                if (length(which(w)) > 0) {
                  nsp.t <- nsp[which(w)]
                  nn <- c(nsp.t,nf,nxy,nFact)
                }
                else nn <- n
              } else nn <- n
              #-
              
              if (!.strIdentical(head(train),head(test),nn)) stop('test data has not the same stucture as the train data OR some variables are missed...!')
              test <- data.frame(test)
              w <- .dataClean(test)
              test <- w[[1]]
              rm.test <- w[[2]]
              rm(w)
            }
            
            if (length(nsp) == 1) {
              if (.isBinomial(train[,nsp])) {
                d <- .createSingleSpecies(train,test,nsp,nf,nFact,nxy,crs)
              } else if (is.factor(train[,nsp]) | is.character(train[,nsp])) {
                train[,nsp] <- as.character(train[,nsp])
                if (length(unique(train[,nsp])) == 1) {
                  spn <- unique(train[,nsp])
                  train[,nsp] <- rep(1,nrow(train))
                  if (!is.null(test)) {
                    wt <- which(test[,nsp] == spn)
                    if (length(wt) > 0) {
                      test <- test[wt,]
                      test[,nsp] <- rep(1,nrow(test))
                    } else {
                      test <- NULL
                      warning(paste('No records for species "',spn,'" are available in the test data, therefore test data has not been used!',sep=''))
                    }
                  }
                  d <- .createSingleSpecies(train,test,nsp,nf,nFact,nxy,crs)
                } else {
                  train <- .df2list(train,nsp,c(nf,nFact,nxy))
                  if (!is.null(test)) test <- .df2list(test,nsp.t,c(nf,nFact,nxy))
                  d <- .createSpeciesDataList(train,test,nsp=names(train),nf,nFact,nxy,crs)
                }
                
              } else stop('the species occurrence data is not recognised!')
            } else {
              d <- .createMultipleSpecies(train,test,nsp,nf,nFact,nxy,crs)
            }
            show(d)
            .catReport(rm.train,rm.test)
            return(d)
            
          }
)

#----------

setMethod('sdmData', signature(train='data.frame',predictors='missing',formula='missing'), 
          function(train, predictors,formula, test=NULL, filename=NULL,crs=NULL,pseudo=FALSE) {
            if (missing(pseudo)) stop('to be able to generate pseudo absence, you need to introduce predictors as a raster object')
            if(missing(test)) test <- NULL
            if(missing(filename)) filename <- NULL
            if(missing(crs)) crs <- NULL
            
            nxy <- NULL; rm.train <- c(0,0); rm.test <- c(0,0); nFact <- NULL; nf <- NULL
            
            ww <- .dataClean(train)
            train <- ww[[1]]
            rm.train <- ww[[2]]
            rm(ww)
            
            w <- which(unlist(lapply(train,.isBinomial)))
            if (length(w) == 0) {
              stop ('No species records are identified, spcify the variable in formula or use a right date structure...')
            } else {
              varNames <- colnames(train)
              nsp <- varNames[w]
              if (length(varNames) > length(nsp)) {
                nf <- varNames[-w]
                w <- which(unlist(lapply(train[,nf],is.factor)))
                ww <- which(unlist(lapply(train[,nf],is.character)))
                if (length(ww) > 0) w <- c(w,ww)
                if (length(w) > 0) {
                  nFact <- nf[w]
                  nf <- .excludeVector(nf,nFact)
                  for (nn in nFact) train[,nn] <- factor(train[,nn])
                }
              }
              
              
              train <- .int.to.numeric(train)
              
              if (!is.null(test)) {
                if (!is.null(nFact)) {
                  if (!all(unlist(lapply(nFact,function(x){x %in% colnames(test) }))))
                    for (nn in nFact) test[,nn] <- factor(test[,nn])
                }
                test <- .int.to.numeric(test)
                # to check common species in train and test:
                nsp.t <- nsp
                w <- unlist(lapply(nsp,function(x){x %in% colnames(test) }))
                if (!all(w)) {
                  if (length(which(w)) > 0) {
                    nsp.t <- nsp[which(w)]
                    nn <- c(nsp.t,nf,nFact,nxy)
                  }
                  else nn <- c(nsp,nf,nFact,nxy)
                } else nn <- c(nsp.t,nf,nFact,nxy)
                #-
                
                if (!.strIdentical(train,test,nn)) stop('test data has not the same stucture as the train data OR some variables are missed...!')
                test <- data.frame(test)
                w <- .dataClean(test)
                test <- w[[1]]
                rm.test <- w[[2]]
                rm(w)
                for (spn in nsp.t) test[,spn] <- .occurrence(test[,spn])
              }
              
              for (spn in nsp) train[,spn] <- .occurrence(train[,spn])
              
              if (length(nsp) == 1) d <- .createSingleSpecies(train,test,nsp,nf,nFact,nxy,crs)
              else d <- .createMultipleSpecies(train,test,nsp,nf,nFact,nxy,crs)
              
            }
            show(d)
            .catReport(rm.train,rm.test)
            return(d) 
            
            
          }
)
##----------------
setMethod('sdmData', signature(train='SpatialPointsDataFrame',predictors='missing'), 
          function(train, predictors,formula,test,filename,crs,pseudo=FALSE) {
            if (missing(pseudo)) stop('to be able to generate pseudo absence, you need to introduce predictors as a raster object')
            if(missing(test)) test <- NULL
            if(missing(filename)) filename <- NULL
            if(missing(crs)) crs <- NULL
            
            rm.train <- c(0,0); rm.test <- c(0,0); nFact <- NULL; nf <- NULL
            nxy <- coordnames(train)
            if (!is.null(test)) {
              if (class(train) == class(test)) {
                nxyt <- coordnames(test)
                test <- as(test,'data.frame')
                if (all(nxy != nxyt)) {
                  colnames(test)[unlist(lapply(nxyt,function(x) which(colnames(test) ==x)))] <- nxy
                } 
              }
            }
            
            if (!is.na(proj4string(train))) crs <- CRS(proj4string(train))
            train <- as(train,'data.frame')
            
            ww <- .dataClean(train)
            train <- ww[[1]]
            rm.train <- ww[[2]]
            rm(ww)
            
            if (missing(formula)) {
              w <- which(unlist(lapply(train,.isBinomial)))
              if (length(w) == 0) {
                stop ('No species records are identified, spcify the variable in formula or use a right date structure...')
              } else {
                varNames <- colnames(train)
                nsp <- varNames[w]
                nf <- varNames[-w]
                nf <- .excludeVector(nf,nxy)
                if (!is.null(nf)) {
                  w <- which(unlist(lapply(train[,nf],is.factor)))
                  ww <- which(unlist(lapply(train[,nf],is.character)))
                  if (length(ww) > 0) w <- c(w,ww)
                  if (length(w) > 0) {
                    nFact <- nf[w]
                    nf <- .excludeVector(nf,nFact)
                    for (nn in nFact) train[,nn] <- factor(train[,nn])
                  }
                }
                
                train <- .int.to.numeric(train)
                
                if (!is.null(test)) {
                  if (!is.null(nFact)) {
                    if (!all(unlist(lapply(nFact,function(x){x %in% colnames(test) }))))
                      for (nn in nFact) test[,nn] <- factor(test[,nn])
                  }
                  test <- .int.to.numeric(test)
                  # to check common species in train and test:
                  nsp.t <- nsp
                  w <- unlist(lapply(nsp,function(x){x %in% colnames(test) }))
                  if (!all(w)) {
                    if (length(which(w)) > 0) {
                      nsp.t <- nsp[which(w)]
                      nn <- c(nsp.t,nf,nFact,nxy)
                    }
                    else nn <- c(nsp,nf,nFact,nxy)
                  } else nn <- c(nsp.t,nf,nFact,nxy)
                  #-
                  
                  if (!.strIdentical(train,test,nn)) stop('test data has not the same stucture as the train data OR some variables are missed...!')
                  test <- data.frame(test)
                  w <- .dataClean(test)
                  test <- w[[1]]
                  rm.test <- w[[2]]
                  rm(w)
                  #for (spn in nsp.t) test[,spn] <- .occurrence(test[,spn])
                }
                #for (spn in nsp) train[,spn] <- .occurrence(train[,spn])
                if (length(nsp) == 1) d <- .createSingleSpecies(train,test,nsp,nf,nFact,nxy,crs)
                else d <- .createMultipleSpecies(train,test,nsp,nf,nFact,nxy,crs)
              }
            } else {
              w <- .exformula(formula,train)
              nsp <- w$nsp; nf <- w$nf; nFact <- w$nFact
              if (!is.null(nf)) {
                w <- which(unlist(lapply(nf,function(x){x %in% nxy})))
                if (length(w) > 0) nf <- nf[-w]  
              }
              
              n <- c(nsp,nf,nxy,nFact)
              if (!all(unlist(lapply(n,function(x){x %in% colnames(train) })))) stop('One or more variable specified in the formula does not exist in train!')
              
              if (!is.null(nFact)) {
                for (nn in nFact) train[,nn] <- factor(train[,nn])
              }
              
              train <- .int.to.numeric(train)
              
              if (!is.null(test)) {
                if (!is.null(nFact)) {
                  if (!all(unlist(lapply(nFact,function(x){x %in% colnames(test) }))))
                    for (nn in nFact) test[,nn] <- factor(test[,nn])
                }
                test <- .int.to.numeric(test)
                # to check common species in train and test:
                nsp.t <- nsp
                w <- unlist(lapply(nsp,function(x){x %in% colnames(test) }))
                if (!all(w)) {
                  if (length(which(w)) > 0) {
                    nsp.t <- nsp[which(w)]
                    nn <- c(nsp.t,nf,nxy,nFact)
                  }
                  else nn <- n
                } else nn <- n
                #-
                
                if (!.strIdentical(head(train),head(test),nn)) stop('test data has not the same stucture as the train data OR some variables are missed...!')
                
                w <- .dataClean(test)
                test <- w[[1]]
                rm.test <- w[[2]]
                rm(w)
              }
              
              if (length(nsp) == 1) {
                if (.isBinomial(train[,nsp])) {
                  d <- .createSingleSpecies(train,test,nsp,nf,nFact,nxy,crs)
                } else if (is.factor(train[,nsp]) | is.character(train[,nsp])) {
                  train[,nsp] <- as.character(train[,nsp])
                  if (length(unique(train[,nsp])) == 1) {
                    spn <- unique(train[,nsp])
                    train[,nsp] <- rep(1,nrow(train))
                    if (!is.null(test)) {
                      wt <- which(test[,nsp] == spn)
                      if (length(wt) > 0) {
                        test <- test[wt,]
                        test[,nsp] <- rep(1,nrow(test))
                      } else {
                        test <- NULL
                        warning(paste('No records for species "',spn,'" are available in the test data, therefore test data has not been used!',sep=''))
                      }
                    }
                    d <- .createSingleSpecies(train,test,nsp,nf,nFact,nxy,crs)
                  } else {
                    train <- .df2list(train,nsp,c(nf,nFact,nxy))
                    if (!is.null(test)) test <- .df2list(test,nsp.t,c(nf,nFact,nxy))
                    d <- .createSpeciesDataList(train,test,nsp=names(train),nf,nFact,nxy,crs)
                  }
                  
                } else stop('the species occurrence data is not recognised!')
              } else {
                d <- .createMultipleSpecies(train,test,nsp,nf,nFact,nxy,crs)
              }
            }
            
            show(d)
            .catReport(rm.train,rm.test)
            return(d) 
          }
)


#----------
setMethod('sdmData', signature(train='SpatialPointsDataFrame',predictors='Raster'), 
          function(train, predictors,formula, test, filename,crs,pseudo=FALSE,size=1000) {
            if (missing(pseudo)) pseudo <- FALSE
            if (missing(size)) size <- 1000
            if(missing(test)) test <- NULL
            if(missing(filename)) filename <- NULL
            if(missing(crs)) crs <- NULL
            
            rm.train <- c(0,0); rm.test <- c(0,0); nFact <- NULL; rm.outside <- c(0,0)
            nxy <- coordnames(train)
            wF <- is.factor(predictors)
            
            if (!is.null(test)) {
              if (class(train) == class(test)) {
                nxyt <- coordnames(test)
                test <- as(test,'data.frame')
                if (all(nxy != nxyt)) {
                  colnames(test)[unlist(lapply(nxyt,function(x) which(colnames(test) ==x)))] <- nxy
                } 
              }
              if (nxy[1] %in% colnames(test) & nxy[2] %in% colnames(test)) {
                cells <- cellFromXY(predictors,test[,nxy])
                cNA <- is.na(cells)
                if (any(cNA)) {
                  if (all(cNA)) stop('Test data has no overlap with the predictors...!')
                  wNA <- which(cNA)
                  test <- test[-wNA,]
                  cells <- cells[-wNA]
                  rm.outside[2] <- length(wNA)
                  rm(cNA,wNA)
                } else rm(cNA)
                if (!any(wF)) test.p <- data.frame(predictors[cells])
                else test.p <- .Extract(predictors,cells,which(wF))
                rm(cells)
                colnames(test.p) <- names(predictors)
              } else stop('test data should have the same class as train data...')
              
            }
            
            if (!is.na(proj4string(train))) crs <- CRS(proj4string(train))
            train <- as(train,'data.frame')
            
            ww <- .dataClean(train)
            train <- ww[[1]]
            rm.train <- ww[[2]]
            rm(ww)
            cells <- cellFromXY(predictors,train[,nxy])
            cNA <- is.na(cells)
            if (any(cNA)) {
              if (all(cNA)) stop('Train data has no overlap with the predictors...!')
              wNA <- which(cNA)
              train <- train[-wNA,]
              cells <- cells[-wNA]
              rm.outside[1] <- length(wNA)
              rm(cNA,wNA)
            } else rm(cNA)
            
            if (!any(wF)) train.p <- data.frame(predictors[cells])
            else train.p <- .Extract(predictors,cells,which(wF))
            rm(cells)
            colnames(train.p) <- names(predictors)
            
            if (missing(formula)) {
              w <- which(unlist(lapply(train,.isBinomial)))
              
              if (length(w) == 0) {
                stop ('No species records are identified, spcify the variable in formula or use a right date structure...')
              } else {
                nsp <- colnames(train)[w]
                nf <- names(predictors)
                train <- data.frame(train,train.p)
                rm(train.p)
                
                ww <- .dataClean(train)
                train <- ww[[1]]
                rm.train <- rm.train + ww[[2]]
                rm(ww)
                
                if (any(wF)) {
                  nFact <- nf[wF]
                  nf <- .excludeVector(nf,nFact)
                }
                train <- .int.to.numeric(train)
                
                if (!is.null(test)) {
                  test <- data.frame(test,test.p)
                  rm(test.p)
                  test <- .int.to.numeric(test)
                  
                  # to check common species in train and test:
                  nsp.t <- nsp
                  w <- unlist(lapply(nsp,function(x){x %in% colnames(test) }))
                  if (!all(w)) {
                    if (length(which(w)) > 0) {
                      nsp.t <- nsp[which(w)]
                      nn <- c(nsp.t,nf,nFact,nxy)
                    }
                    else nn <- c(nsp,nf,nFact,nxy)
                  } else nn <- c(nsp.t,nf,nFact,nxy)
                  #-
                  
                  if (!.strIdentical(train,test,nn)) stop('test data has not the same stucture as the train data OR some variables are missed...!')
                  w <- .dataClean(test)
                  test <- w[[1]]
                  rm.test <- w[[2]]
                  rm(w)
                }
                if (length(nsp) == 1) {
                  if (pseudo) {
                    train <- .pseudo_df(train,predictors,n=size,spn = nsp,nxy=nxy)
                  }
                  
                  d <- .createSingleSpecies(train,test,nsp,nf,nFact,nxy,crs)
                } else d <- .createMultipleSpecies(train,test,nsp,nf,nFact,nxy,crs)
                
              }
            } else {
              train <- data.frame(train,train.p)
              rm(train.p)
              ww <- .dataClean(train)
              train <- ww[[1]]
              rm.train <- rm.train + ww[[2]]
              rm(ww)
              
              w <- .exformula(formula,train)
              nsp <- w$nsp
              nf <- w$nf
              nFact <- w$nFact
              
              w <- which(unlist(lapply(nf,function(x){x %in% nxy})))
              if (length(w) > 0) nf <- nf[-w]
              n <- c(nsp,nf,nxy,nFact)
              if (!all(unlist(lapply(n,function(x){x %in% colnames(train) })))) stop('One or more variable specified in the formula does not exist in train!')
              
              if (!is.null(nFact)) {
                for (nn in nFact) train[,nn] <- factor(train[,nn])
              }
              
              train <- .int.to.numeric(train)
              
              if (!is.null(test)) {
                test <- data.frame(test,test.p)
                rm(test.p)
                if (!is.null(nFact)) {
                  if (!all(unlist(lapply(nFact,function(x){x %in% colnames(test) }))))
                    for (nn in nFact) test[,nn] <- factor(test[,nn])
                }
                test <- .int.to.numeric(test)
                # to check common species in train and test:
                nsp.t <- nsp
                w <- unlist(lapply(nsp,function(x){x %in% colnames(test) }))
                if (!all(w)) {
                  if (length(which(w)) > 0) {
                    nsp.t <- nsp[which(w)]
                    nn <- c(nsp.t,nf,nxy,nFact)
                  }
                  else nn <- n
                } else nn <- n
                #-
                
                if (!.strIdentical(head(train),head(test),nn)) stop('test data has not the same stucture as the train data OR some variables are missed...!')
                
                w <- .dataClean(test)
                test <- w[[1]]
                rm.test <- w[[2]]
                rm(w)
              }
              if (length(nsp) == 1) {
                if (.isBinomial(train[,nsp])) {
                  d <- .createSingleSpecies(train,test,nsp,nf,nFact,nxy,crs)
                } else if (is.factor(train[,nsp]) | is.character(train[,nsp])) {
                  train[,nsp] <- as.character(train[,nsp])
                  if (length(unique(train[,nsp])) == 1) {
                    spn <- unique(train[,nsp])
                    train[,nsp] <- rep(1,nrow(train))
                    if (!is.null(test)) {
                      wt <- which(test[,nsp] == spn)
                      if (length(wt) > 0) {
                        test <- test[wt,]
                        test[,nsp] <- rep(1,nrow(test))
                      } else {
                        test <- NULL
                        warning(paste('No records for species "',spn,'" are available in the test data, therefore test data has not been used!',sep=''))
                      }
                      if (length(unique(test[,nsp])) == 1) test <-.pseudo_df(test,predictors,n,nsp)
                    }
                    
                    if (pseudo) {
                      train <- .pseudo_df(df = train,predictors = predictors,n = size,spn = nsp,nxy = nxy )
                    }
                    
                    d <- .createSingleSpecies(train,test,nsp,nf,nFact,nxy,crs)
                  } else {
                    train <- .df2list(train,nsp,c(nf,nFact,nxy))
                    if (!is.null(test)) {
                      test <- .df2list(test,nsp.t,c(nf,nFact,nxy))
                      if (pseudo) {
                        for (i in seq_along(test)) {
                          if (length(unique(test[[i]][,nsp])) == 1) test[[i]] <-.pseudo_df(test[[i]],predictors,n=size,spn = nsp,nxy=nxy)
                        }
                      }
                    }
                    if (pseudo) {
                      for (i in seq_along(train)) train[[i]] <- .pseudo_df(train[[i]],predictors,n=size,spn=nsp,nxy=nxy)
                    }
                    d <- .createSpeciesDataList(train,test,nsp=names(train),nf,nFact,nxy,crs)
                  }
                  
                } else stop('the species occurrence data is not recognised!')
              } else {
                d <- .createMultipleSpecies(train,test,nsp,nf,nFact,nxy,crs)
              }
            }
            show(d)
            .catReport(rm.train,rm.test)
            return(d)
          }
)

#----------------
setMethod('sdmData', signature(train='SpatialPoints',predictors='Raster'), 
          function(train, predictors,formula, test, filename,crs,pseudo=FALSE,size=1000) {
            if (missing(pseudo)) pseudo <- FALSE
            if (missing(size)) size <- 1000
            if(missing(test)) test <- NULL
            if(missing(filename)) filename <- NULL
            if(missing(crs)) crs <- NULL
            
            rm.train <- c(0,0); rm.test <- c(0,0); nFact <- NULL; rm.outside <- c(0,0)
            nxy <- coordnames(train)
            wF <- is.factor(predictors)
            
            if (!is.null(test)) {
              if (class(train) == class(test)) {
                nxyt <- coordnames(test)
                test <- data.frame(coordinates(test))
                if (all(nxy != nxyt)) {
                  colnames(test)[unlist(lapply(nxyt,function(x) which(colnames(test) ==x)))] <- nxy
                } 
              } else if (inherits(test,'SpatialPointsDataFrame')) {
                nxyt <- coordnames(test)
                test <- as(test,'data.frame')
                if (all(nxy != nxyt)) {
                  colnames(test)[unlist(lapply(nxyt,function(x) which(colnames(test) ==x)))] <- nxy
                } 
              }
              
              if (nxy[1] %in% colnames(test) & nxy[2] %in% colnames(test)) {
                cells <- cellFromXY(predictors,test[,nxy])
                cNA <- is.na(cells)
                if (any(cNA)) {
                  if (all(cNA)) stop('Test data has no overlap with the predictors...!')
                  wNA <- which(cNA)
                  test <- test[-wNA,]
                  cells <- cells[-wNA]
                  rm.outside[2] <- length(wNA)
                  rm(cNA,wNA)
                } else rm(cNA)
                if (!any(wF)) test.p <- data.frame(predictors[cells])
                else test.p <- .Extract(predictors,cells,which(wF))
                rm(cells)
                colnames(test.p) <- names(predictors)
              } else stop('test data should have the same class as train data...')
              
            }
            
            if (!is.na(proj4string(train))) crs <- CRS(proj4string(train))
            train <- coordinates(train)
            
            #ww <- .dataClean(train)
            #train <- ww[[1]]
            #rm.train <- ww[[2]]
            #rm(ww)
            cells <- cellFromXY(predictors,train[,nxy])
            cNA <- is.na(cells)
            if (any(cNA)) {
              if (all(cNA)) stop('Train data has no overlap with the predictors...!')
              wNA <- which(cNA)
              train <- train[-wNA,]
              cells <- cells[-wNA]
              rm.outside[1] <- length(wNA)
              rm(cNA,wNA)
            } else rm(cNA)
            
            if (!any(wF)) train.p <- data.frame(predictors[cells])
            else train.p <- .Extract(predictors,cells,which(wF))
            rm(cells)
            colnames(train.p) <- names(predictors)
            
            if (missing(formula)) {
              nsp <- 'SPECIES'
              nf <- names(predictors)
              train <- data.frame(SPECIES=rep(1,nrow(train)),train)
              train <- data.frame(train,train.p)
              rm(train.p)
              
              ww <- .dataClean(train)
              train <- ww[[1]]
              rm.train <- rm.train + ww[[2]]
              rm(ww)
              
              if (any(wF)) {
                nFact <- nf[wF]
                nf <- .excludeVector(nf,nFact)
              }
              train <- .int.to.numeric(train)
              if (pseudo) {
                train <- .pseudo_df(train,predictors,n=size,spn=nsp,nxy=nxy)
              }
              
              if (!is.null(test)) {
                test <- data.frame(SPECIES=rep(1,nrow(test)),test,test.p)
                rm(test.p)
                test <- .int.to.numeric(test)
                
                w <- .dataClean(test)
                test <- w[[1]]
                rm.test <- w[[2]]
                rm(w)
                if (pseudo) {
                  if (length(unique(test[,nsp])) == 1) test <-.pseudo_df(test,predictors,n=size,spn=nsp,nxy)
                }
                
              }
              d <- .createSingleSpecies(train,test,nsp,nf,nFact,nxy,crs)
              
            } else {
              train <- data.frame(SPECIES=rep(1,nrow(train)),train,train.p)
              rm(train.p)
              ww <- .dataClean(train)
              train <- ww[[1]]
              rm.train <- rm.train + ww[[2]]
              rm(ww)
              
              w <- .exformula(formula,train)
              nsp <- w$nsp[1]
              nf <- w$nf
              nFact <- w$nFact
              colnames(train)[1] <- nsp
              
              w <- which(unlist(lapply(nf,function(x){x %in% nxy})))
              if (length(w) > 0) nf <- nf[-w]
              n <- c(nsp,nf,nxy,nFact)
              if (!all(unlist(lapply(n,function(x){x %in% colnames(train) })))) stop('One or more variable specified in the formula does not exist in train!')
              
              if (!is.null(nFact)) {
                for (nn in nFact) train[,nn] <- factor(train[,nn])
              }
              
              train <- .int.to.numeric(train)
              if (pseudo) {
                train <- .pseudo_df(train,predictors,n=size,spn=nsp,nxy=nxy)
              }
              if (!is.null(test)) {
                test <- data.frame(test,test.p)
                rm(test.p)
                if (!nsp %in% colnames(test)) {
                  test <- data.frame(SPECIES=rep(1,nrow(test)),test)
                  colnames(test)[1] <- nsp[1]
                }
                if (!is.null(nFact)) {
                  if (!all(unlist(lapply(nFact,function(x){x %in% colnames(test) }))))
                    for (nn in nFact) test[,nn] <- factor(test[,nn])
                }
                test <- .int.to.numeric(test)
                if (pseudo) {
                  if (length(unique(test[,nsp])) == 1) test <-.pseudo_df(test,predictors,n=size,spn=nsp,nxy)
                }
                w <- .dataClean(test)
                test <- w[[1]]
                rm.test <- w[[2]]
                rm(w)
              }
              d <- .createSingleSpecies(train,test,nsp,nf,nFact,nxy,crs)
              
            }
            show(d)
            .catReport(rm.train,rm.test)
            return(d)
            
          }
          
)
