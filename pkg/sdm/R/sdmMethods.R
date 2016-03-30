# Author: Babak Naimi, naimi.b@gmail.com
# Date :  August 2015
# Version 1.0
# Licence GPL v3


.create.sdmCorrelativeMethod <- function(name,packages=NULL,modelTypes=NULL,fitParams,fitSettings=NULL,settingRules=NULL,fitFunction,predictParams=NULL,predictSettings=NULL,predictFunction=NULL,tuneParams=NULL,metadata=NULL,...) {
  m <- new('sdmCorrelativeMethod',name=name[1])
  if (length(name) > 1) m@aliases <- name[2:length(name)]
  if (!is.null(packages) && is.character(packages)) {
    m@packages <- packages
    w <- rep(FALSE,length(m@packages))
    for (i in seq_along(m@packages)) w[i] <- require(m@packages[i],character.only=TRUE)
    if (!all(w)) print(paste('warning: packages (',paste(m@packages[!w],collapse=', '),') need to be installed to get this method working!',sep=''))
  } else m@packages <- NULL
  
  if (!is.null(modelTypes)) {
    modelTypes <- tolower(modelTypes)
    for (i in 1:length(modelTypes)) {
      if (modelTypes[i] %in% c('po','presenceonly','presence-only','presence')) {
        modelTypes[i] <- 'po'
      } else if (modelTypes[i] %in% c('pa','presenceabsence','presence-absence')) {
        modelTypes[i] <- 'pa'
      } else if (modelTypes[i] %in% c('pb','presenceb','presence-background','presence-pseudo','presence-pseudoabsence','ppa','psa')) {
        modelTypes[i] <- 'pb'
      } else if (modelTypes[i] %in% c('ab','abundance')) {
        modelTypes[i] <- 'ab'
      } else if (modelTypes[i] %in% c('n','nominal','multinominal')) {
        modelTypes[i] <- 'n'
      } else {
        warning(paste('modelType',modelTypes[i],'is unknown, it is ignored!'))
        modelTypes[i] <- NA
      }
    }
    m@modelTypes <- modelTypes
  }
    
  #-------
  if (is.list(fitParams)) {
    n <- names(fitParams)
    if (is.null(n)) stop('fitParams is not appropriately defined; example: list(formula="standard.formula",data="sdmDataFrame")')
    m@fitParams <- fitParams
  } else stop('fitParams should be a list')
  #------
  if (!is.null(fitSettings)) {
    if (!is.list(fitSettings)) stop('fitSettings should be a list!')
    n <- names(fitSettings)
    if (is.null(n)) stop('fitSettings is not appropriately defined; example: list(family=link(binomial),ntrees=1000)')
    if ('' %in% n) {
      w <- which(n == '')
      for (ww in w) {
        if (is.character(n[ww])) names(fitSettings)[ww] <- n[w]
        else stop('fitSettings is not appropriately defined; example: list(family=link(binomial),ntrees=1000)')
      }
    }
    m@fitSettings <- fitSettings
  }
  #------
  if (!is.null(settingRules)) {
    if (!is.function(settingRules)) stop('settingRules should be a function!')
    m@settingRules <- settingRules
  }
  #-----
  if (exists(as.character(substitute(fitFunction)),mode='function') || exists(as.character(substitute(fitFunction)), envir = asNamespace('sdm'), inherits = FALSE)) {
    if (class(substitute(fitFunction)) == 'name') {
      fn <- as.character(substitute(fitFunction))
      if (environmentName(environment(fitFunction)) == "R_GlobalEnv") {
        # assign to the environment in the container of methods!
        if (is.null(m@.temp.env)) m@.temp.env <- new.env()
        assign(fn,fitFunction,envir = m@.temp.env) ####
        m@packages <- unique(c(m@packages,'.temp'))
        # when environment if attached, the conflict with the existing object in .GlobalEnv should be resolved!
      } 
      m@fitFunction <- eval(parse(text=paste("function(params) {do.call(",fn,",params)}")))
      
    } else if (class(substitute(fitFunction)) == 'call') {
      if (environmentName(environment(fitFunction)) == "R_GlobalEnv" || environmentName(environment(fitFunction)) == "sdm") {
        if (is.null(m@.temp.env)) m@.temp.env <- new.env()
        if (!paste(m@name,'.fit',sep='') %in% ls(envir=m@.temp.env)) fn <-paste(m@name,'.fit',sep='') ####
        else stop('the user defined function in fitFunction cannot be registered because an object with a similar name exists in the container!')
        
        assign(fn,fitFunction,envir = m@.temp.env) ####
        m@packages <- unique(c(m@packages,'.temp'))
      } else {
        if (as.character(substitute(fitFunction))[1] == "::") fn <- as.character(substitute(fitFunction))[3]
        else stop('fitFunction cannot be identified!')
      } 
      m@fitFunction <- eval(parse(text=paste("function(params) {do.call(",fn,",params)}")))
    } else stop('fitFunction cannot be identified!')
    
  } else if (exists(as.character(substitute(fitFunction)), envir = asNamespace('sdm'), inherits = FALSE)) {
    fn <- as.character(substitute(fitFunction))
    m@fitFunction <- eval(parse(text=paste("function(params) {do.call(",fn,",params)}")))
  } else stop(paste("Function",as.character(substitute(fitFunction)),"is not found!"))
  #---------
  
  if (!is.null(predictFunction)) {
    if (exists(as.character(substitute(predictFunction)),mode='function') || exists(as.character(substitute(predictFunction)), envir = asNamespace('sdm'), inherits = FALSE)) {
      
      if (class(substitute(predictFunction)) == 'name') {
        fn <- as.character(substitute(predictFunction))
        if (environmentName(environment(predictFunction)) == "R_GlobalEnv" ) {
          # assign to the environment in the container of methods!
          if (is.null(m@.temp.env)) m@.temp.env <- new.env()
          assign(fn,predictFunction,envir = m@.temp.env) ####
          m@packages <- unique(c(m@packages,'.temp'))
          # when environment if attached, the conflict with the existing object in .GlobalEnv should be resolved!
        } 
        m@predictFunction <- eval(parse(text=paste("function(params) {do.call(",fn,",params)}")))
        
      } else if (class(substitute(predictFunction)) == 'call') {
        if (environmentName(environment(predictFunction)) == "R_GlobalEnv" || environmentName(environment(predictFunction)) == "sdm") {
          if (is.null(m@.temp.env)) m@.temp.env <- new.env()
          if (!paste(m@name,'.predict',sep='') %in% ls(envir=m@.temp.env)) fn <-paste(m@name,'.predict',sep='') ####
          else stop('the user defined function in predictFunction cannot be registered because an object with a similar name exists in the container!')
          assign(fn,predictFunction,envir = m@.temp.env) ####
          m@packages <- unique(c(m@packages,'.temp'))
        } else {
          if (as.character(substitute(predictFunction))[1] == "::") fn <- as.character(substitute(predictFunction))[3]
          else stop('predictFunction cannot be identified!')
        } 
        
        m@predictFunction <- eval(parse(text=paste("function(params) {do.call(",fn,",params)}")))
      } else if (exists(as.character(substitute(predictFunction)), envir = asNamespace('sdm'), inherits = FALSE)) {
        fn <- as.character(substitute(predictFunction))
        m@predictFunction <- eval(parse(text=paste("function(params) {do.call(",fn,",params)}")))
      } else stop('predictFunction cannot be identified!')
      
    } else stop(paste("Function",as.character(substitute(predictFunction)),"is not found!"))
  
    if (!is.null(predictParams)) {
      if (is.list(predictParams)) {
        n <- names(predictParams)
        if (is.null(n)) stop('predictParams is not appropriately defined; example: list(newdata="sdmDataFrame")')
        m@predictParams <- predictParams
      } else stop('predictParams should be a list')
    }
    if (!is.null(predictSettings)) {
      if (is.list(predictSettings)) m@predictSettings <- predictSettings
      else stop('predictSettings should be a list!')
    }
  }
 #-------- 
  if (!is.null(tuneParams)) {
    if (!is.list(tuneParams)) stop('tuneParams should be a list; example: list(ntrees=seq(500,3000,by=200))')
    n <- names(tuneParams)
    if (is.null(n)) stop('tuneParams is not appropriately defined; example: list(ntrees=seq(500,3000,by=200))')
    m@tuneParams <- tuneParams
  } 
  #------------  
  if (inherits(metadata,'.Metadata')) m@metadata <- metadata
  else m@metadata <- .newMetadata(...)
  
  m
}

#-----
.update.sdmCorrelativeMethod <- function(m,...) {
  name=NULL;packages=NULL;modelTypes=NULL;fitParams=NULL;fitSettings=NULL;settingRules=NULL;fitFunction=NULL;predictParams=NULL;predictSettings=NULL;predictFunction=NULL;tuneParams=NULL;metadata=NULL
  
  dot <- list(...)
  n <- tolower(names(dot))
  for (i in seq_along(n)) {
    if (any(!is.na(pmatch(c("nam"),n[i])))) name <- dot[[i]]
    else if (any(!is.na(pmatch(c("pac"),n[i])))) packages <- dot[[i]]
    else if (any(!is.na(pmatch(c("mod"),n[i])))) modelTypes <- dot[[i]]
    else if (any(!is.na(pmatch(c("fits"),n[i])))) fitSettings <- dot[[i]]
    else if (any(!is.na(pmatch(c("fitp"),n[i])))) fitParams <- dot[[i]]
    else if (any(!is.na(pmatch(c("set"),n[i])))) settingRules <- dot[[i]]
    else if (any(!is.na(pmatch(c("fitf"),n[i])))) fitFunction <- dot[[i]]
    else if (any(!is.na(pmatch(c("predicts"),n[i])))) predictSettings <- dot[[i]]
    else if (any(!is.na(pmatch(c("predictp"),n[i])))) predictParams <- dot[[i]]
    else if (any(!is.na(pmatch(c("predictf"),n[i])))) predictFunction <- dot[[i]]
    else if (any(!is.na(pmatch(c("tun"),n[i])))) tuneParams <- dot[[i]]
    else if (any(!is.na(pmatch(c("met"),n[i])))) metadata <- dot[[i]]
  }
  #--------
  if (!is.null(name)) {
    if (length(name) > 1) m@aliases <- name[2:length(name)]
  }
  
  if (!is.null(packages) && is.character(packages)) {
    m@packages <- packages
    w <- rep(FALSE,length(m@packages))
    for (i in seq_along(m@packages)) w[i] <- require(m@packages[i],character.only=TRUE)
    if (!all(w)) print(paste('warning: packages (',paste(m@packages[!w],collapse=', '),') need to be installed to get this method working!',sep=''))
  }
  
  if (!is.null(modelTypes)) {
    modelTypes <- tolower(modelTypes)
    for (i in seq_along(modelTypes)) {
      if (modelTypes[i] %in% c('po','presenceonly','presence-only','presence')) modelTypes[i] <- 'po'
      else if (modelTypes[i] %in% c('pa','presenceabsence','presence-absence')) modelTypes[i] <- 'pa'
      else if (modelTypes[i] %in% c('pb','presenceb','presence-background','presence-pseudo','presence-pseudoabsence','ppa','psa')) modelTypes[i] <- 'pb'
      else if (modelTypes[i] %in% c('ab','abundance')) modelTypes[i] <- 'ab'
      else if (modelTypes[i] %in% c('n','nominal','multinominal')) modelTypes[i] <- 'n'
      else {
        warning(paste('modelType',modelTypes[i],'is unknown, it is ignored!'))
        modelTypes[i] <- NULL
      }
    }
    m@modelTypes <- modelTypes
  }
  
  #-------
  if (!is.null(fitParams) && is.list(fitParams)) {
    n <- names(fitParams)
    if (is.null(n)) stop('fitParams is not appropriately defined; example: list(formula="standard.formula",data="sdmDataFrame")')
    m@fitParams <- fitParams
  }
  #------
  if (!is.null(fitSettings) && is.list(fitSettings)) {
    n <- names(fitSettings)
    if (is.null(n)) stop('fitSettings is not appropriately defined; example: list(family=link(binomial),ntrees=1000)')
    if ('' %in% n) {
      w <- which(n == '')
      for (ww in w) {
        if (is.character(n[ww])) names(fitSettings)[ww] <- n[w]
        else stop('fitSettings is not appropriately defined; example: list(family=link(binomial),ntrees=1000)')
      }
    }
    m@fitSettings <- fitSettings
  }
  #------
  if (!is.null(settingRules)) {
    if (!is.function(settingRules)) stop('settingRules should be a function!')
    m@settingRules <- settingRules
  }
  #-----
  if (!is.null(fitFunction) && exists(as.character(substitute(fitFunction)),mode='function')) {
    if (class(substitute(fitFunction)) == 'name') {
      fn <- as.character(substitute(fitFunction))
      if (environmentName(environment(fitFunction)) == "R_GlobalEnv") {
        # assign to the environment in the container of methods!
        if (is.null(m@.temp.env)) m@.temp.env <- new.env()
        assign(fn,fitFunction,envir = m@.temp.env) ####
        m@packages <- unique(c(m@packages,'.temp'))
        # when environment if attached, the conflict with the existing object in .GlobalEnv should be resolved!
      } 
      m@fitFunction <- eval(parse(text=paste("function(params) {do.call(",fn,",params)}")))
      
    } else if (class(substitute(fitFunction)) == 'call') {
      
      if (environmentName(environment(fitFunction)) == "R_GlobalEnv" || environmentName(environment(fitFunction)) == "sdm") {
        if (is.null(m@.temp.env)) m@.temp.env <- new.env()
        if (!paste(m@name,'.fit',sep='') %in% ls(envir=m@.temp.env)) fn <- paste(m@name,'.fit',sep='') ####
        else stop('the user defined function in fitFunction cannot be registered because an object with a similar name exists in the container!')
        
        assign(fn,fitFunction,envir = m@.temp.env) ####
        m@packages <- unique(c(m@packages,'.temp'))
      } else {
        if (as.character(substitute(fitFunction))[1] == "::") fn <- as.character(substitute(fitFunction))[3]
        else stop('fitFunction cannot be identified!')
      }
      
      m@fitFunction <- eval(parse(text=paste("function(params) {do.call(",fn,",params)}")))
    } else stop('fitFunction cannot be identified!')
    
  } 
  #---------
  
  if (!is.null(predictFunction)) {
    if (exists(as.character(substitute(predictFunction)),mode='function')) {
      
      if (class(substitute(predictFunction)) == 'name') {
        fn <- as.character(substitute(predictFunction))
        if (environmentName(environment(predictFunction)) == "R_GlobalEnv" || environmentName(environment(predictFunction)) == "sdm") {
          # assign to the environment in the container of methods!
          if (is.null(m@.temp.env)) m@.temp.env <- new.env()
          assign(fn,predictFunction,envir = m@.temp.env) ####
          m@packages <- unique(c(m@packages,'.temp'))
          # when environment if attached, the conflict with the existing object in .GlobalEnv should be resolved!
        } 
        m@predictFunction <- eval(parse(text=paste("function(params) {do.call(",fn,",params)}")))
        
      } else if (class(substitute(predictFunction)) == 'call') {
        
        if (environmentName(environment(predictFunction)) == "R_GlobalEnv") {
          if (is.null(m@.temp.env)) m@.temp.env <- new.env()
          if (!paste(m@name,'.predict',sep='') %in% ls(envir=m@.temp.env)) fn <-paste(m@name,'.predict',sep='') ####
          else stop('the user defined function in predictFunction cannot be registered because an object with a similar name exists in the container!')
          assign(fn,predictFunction,envir = m@.temp.env) ####
          m@packages <- unique(c(m@packages,'.temp'))
        } else {
          if (as.character(substitute(predictFunction))[1] == "::") fn <- as.character(substitute(predictFunction))[3]
          else stop('predictFunction cannot be identified!')
        } 
        
        m@predictFunction <- eval(parse(text=paste("function(params) {do.call(",fn,",params)}")))
      } else stop('predictFunction cannot be identified!')
      
    } else stop(paste("Function",as.character(substitute(predictFunction)),"is not found!"))
    
    if (!is.null(predictParams)) {
      if (is.list(predictParams)) {
        n <- names(predictParams)
        if (is.null(n)) stop('predictParams is not appropriately defined; example: list(newdata="sdmDataFrame")')
        m@predictParams <- predictParams
      } else stop('predictParams should be a list')
    }
    
    if (!is.null(predictSettings)) {
      if (is.list(predictSettings)) m@predictSettings <- predictSettings
      else stop('predictSettings should be a list!')
    }
  }
  #-------- 
  if (!is.null(tuneParams)) {
    if (!is.list(tuneParams)) stop('tuneParams should be a list; example: list(ntrees=seq(500,3000,by=200))')
    n <- names(tuneParams)
    if (is.null(n)) stop('tuneParams is not appropriately defined; example: list(ntrees=seq(500,3000,by=200))')
    m@tuneParams <- tuneParams
  } 
  #------------  
  if (inherits(metadata,'.Metadata')) m@metadata <- metadata
  else  me <- .newMetadata(...)
  if(!is.null(me)) m@metadata <- me
  m
}
#-----------
.movEnv <- function(e1,e2) {
  n1 <- ls(envir = e1)
  for (n in n1) assign(n,e1[[n]],envir = e2)
  rm(list=n1,envir = e1)
  e2
}
#----------
.movEnv2sdm <- function(e) {
  n1 <- ls(envir = e)
  for (n in n1) {
    if (!exists(n,envir = as.environment("package:sdm"))) assign(n,e[[n]],envir = as.environment("package:sdm"))
  } 
  rm(list=n1,envir = e)
}
#---------

.newMetadata <- function(...) {
  dot <- list(...)
  if (length(dot) > 0) {
    w <- unlist(lapply(dot, function(x) inherits(x,'.Metadata')))
    if (any(w)) return(dot[[which(w)]])
    else {
      ndot <- unlist(lapply(tolower(names(dot)),function(x) paste(strsplit(x,'')[[1]][1:3],collapse='')))
      w <- ndot %in% c('ful','cre','aut','web','cit','hel','des','dat','lic','url')
      if (any(w)) {
        m <- new(".Metadata")
        
        w <- which(ndot == 'aut')
        if (length(w) > 0) {
          if (is.list(dot[[w]])) m@authors <- dot[[w]]
          else if (is.character(dot[[w]])) m@authors <- list(dot[[w]])
        }
        
        w <- which(ndot == 'cre')
        if (length(w) > 0) {
          if (is.list(dot[[w]])) m@creators <- dot[[w]]
          else if (is.character(dot[[w]])) m@creators <- list(dot[[w]])
        }
        
        w <- which(ndot == 'tit')
        if (length(w) > 0) m@title <- dot[[w]]
        
        w <- which(ndot == 'url' | ndot == 'web')
        if (length(w) > 0) m@url <- dot[[w]]
        
        w <- which(ndot == 'cit')
        if (length(w) > 0) m@citations <- dot[[w]]
        
        w <- which(ndot == 'hel')
        if (length(w) > 0) m@Help <- dot[[w]]
        
        w <- which(ndot == 'des')
        if (length(w) > 0) m@description <- dot[[w]]
        
        w <- which(ndot == 'dat')
        if (length(w) > 0) m@date <- dot[[w]]
        
        w <- which(ndot == 'lic')
        if (length(w) > 0) m@license <- dot[[w]]
        return(m)
      }
    }
  }
}


#################################################

###############
.sdmMethods <- new('.sdmMethodsContainer')
.sdmMethods$addMethod(.create.sdmCorrelativeMethod(name=c('glm','GLM','lm'),
                                                    packages='stats',
                                                    modelTypes = c('pa','pb','ab','n'),
                                                    fitParams = list(formula='standard.formula',data='sdmDataFrame'),
                                                    fitSettings = list(family=binomial(link='logit'),weights=NULL,model=FALSE),
                                                    fitFunction = glm,
                                                    settingRules = function(x,fitSettings) {
                                                      if (x@distribution == 'ab') fitSettings[['family']] <- poisson
                                                      fitSettings
                                                    },
                                                    tuneParams = NULL,
                                                    predictParams=list(object='model',newdata='sdmDataFrame'),
                                                    predictSettings=list(type='response'),
                                                    predictFunction=predict.glm,
                                                    #------ metadata (optional):
                                                    title='Generalized Linear Model',
                                                    creator='Babak Naimi',
                                                    authors=c('R Core team'), # authors of the main method
                                                    email='naimi.b@gmail.com',
                                                    url='http://r-gis.net',
                                                    citation=list(bibentry('book',title = "Generalized linear models",
                                                                           author = as.person("P. McCullagh [aut], J. A. Nelder [aut]"),
                                                                           year = "1989",
                                                                           publisher = "Chapman and Hall",
                                                                           address = "London")
                                                    ),
                                                    description='glm is used to fit generalized linear models, specified by giving a symbolic description of the linear predictor and a description of the error distribution [see the help for glm function in stats package]'
),FALSE)
#----
.sdmMethods$addMethod(.create.sdmCorrelativeMethod(name=c('gam','GAM'),
                                                   packages='mgcv',
                                                   modelTypes = c('pa','pb','ab','n'),
                                                   fitParams = list(formula='gam.mgcv.formula',data='sdmDataFrame'),
                                                   fitSettings = list(family=binomial(link='logit'),weights=NULL,subset=NULL,na.action='na.omit',offset=NULL,method='GCV.Cp',optimizer=c("outer","newton"),select=FALSE,knots=NULL,sp=NULL,min.sp=NULL,H=NULL,gamma=1,fit=TRUE,paraPen=NULL,G=NULL),
                                                   fitFunction = gam,
                                                   settingRules = function(x='sdmSettings',y='sdmChategories') {
                                                     o <- list()
                                                     if (x@distribution == 'ab') o[['family']] <- poisson
                                                     o
                                                   },
                                                   tuneParams = NULL,
                                                   predictParams=list(object='model',newdata='sdmDataFrame'),
                                                   predictSettings=list(type='response'),
                                                   predictFunction=predict.gam,
                                                   #------ metadata (optional):
                                                   title='Generalized Additive Models with integrated smoothness estimation',
                                                   creator='Babak Naimi',
                                                   authors=c('Simon N. Wood'), # authors of the main method
                                                   email='naimi.b@gmail.com',
                                                   url='http://r-gis.net',
                                                   citation=list(bibentry('book',title = " Generalized Additive Models: An Introduction with R",
                                                                          author = as.person("S. N. Wood [aut]"),
                                                                          year = "2006",
                                                                          publisher = "Chapman and Hall/CRC press")
                                                   ),
                                                   description='Fits a generalized additive model (GAM) to data, the term "GAM" being taken to include any quadratically penalized GLM and a variety of other models estimated by a quadratically penalised likelihood type approach [see the help for gam function in mgcv package]'
),FALSE)
#-----
.sdmMethods$addMethod(.create.sdmCorrelativeMethod(name=c('brt','BRT','gbm','GBM'),
                      packages='gbm',
                      modelTypes = c('pa','pb','ab','n'),
                      fitParams = list(formula='standard.formula',data='sdmDataFrame'),
                      fitSettings = list(distribution='bernoulli',
                                         n.trees=1000,
                                         interaction.depth=1,
                                         n.minobsinnode = 10,
                                         shrinkage = 0.001,
                                         bag.fraction = 0.5,
                                         train.fraction = 1.0,
                                         cv.folds=0,
                                         keep.data = TRUE,
                                         verbose = "CV",
                                         class.stratify.cv=NULL),
                      fitFunction = gbm,
                      settingRules = function(x,fitSettings,predictSettings,userSettings=NULL) {
                        if (!is.null(userSetting)) fitSettings <- .assign(fitSettings,userSettings)
                        else if (x@distribution == 'ab') fitSettings[['distribution']] <- "poisson"
                        else if (x@distribution == 'n') fitSettings[['distribution']] <- "multinomial"
                        
                        list(fitSettings=fitSettings,predictSettings=predictSettings)
                      },
                      tuneParams = NULL,
                      predictParams=list(object='model',newdata='sdmDataFrame'),
                      predictSettings=list(n.trees=1000,type='response'),
                      predictFunction=predict.gbm,
                      #------ metadata (optional):
                      title='Boosted Regression Trees',
                      creator='Babak Naimi',
                      authors=c('Greg Ridgeway'), # authors of the main method
                      email='naimi.b@gmail.com',
                      url='http://r-gis.net',
                      citation=list(bibentry('Article',title = "A working guide to boosted regression trees",
                                             author = as.person("J. Elith [aut], J. R. Leathwick [aut], T. Hastie [aut]"),
                                             year = "2008",
                                             journal = "Journal of Animal Ecology",
                                             number="77",
                                             pages="802-813",
                                             publisher="Wiley Online Library")
                      ),
                      description='Fits Boosting regression trees (BRT), called also generalized boosting regression model (GBM). Boosting is the process of iteratively adding basis functions in a greedy fashion so that each additional basis function further reduces the selected loss function [see the help for gbm function in gbm package]'
),FALSE)


.sdmMethods$addMethod(.create.sdmCorrelativeMethod(name=c('rf','RF','randomForest','rforest'),
                                                   packages='randomForest',
                                                   modelTypes = c('pa','pb','ab','n'),
                                                   fitParams = list(formula='standard.formula',data='sdmDataFrame'),
                                                   fitSettings = list(ntree=1000,
                                                                      replace=TRUE,
                                                                      importance=TRUE
                                                                      ),
                                                   fitFunction = randomForest,
                                                   settingRules = function(x,fitSettings,predictSettings,userSettings=NULL) {
                                                     if (!is.null(userSetting)) fitSettings <- .assign(fitSettings,userSettings)
                                                     
                                                     list(fitSettings=fitSettings,predictSettings=predictSettings)
                                                   },
                                                   tuneParams = NULL,
                                                   predictParams=list(object='model',newdata='sdmDataFrame'),
                                                   predictSettings=list(type='response'),
                                                   predictFunction=predict,
                                                   #------ metadata (optional):
                                                   title='Random Forest',
                                                   creator='Babak Naimi',
                                                   authors=c('Andy Liaw','Matthew Wiener'), # authors of the main method
                                                   email='naimi.b@gmail.com',
                                                   url='http://r-gis.net',
                                                   citation=list(bibentry('Article',title = "Random Forests",
                                                                          author = as.person("L. Breiman [aut]"),
                                                                          year = "2001",
                                                                          journal = "Machine Learning",
                                                                          number="45(1)",
                                                                          pages="5-32"
                                                                          )
                                                   ),
                                                   description="implements Breiman's random forest algorithm (based on Breiman and Cutler's original Fortran code) for classification and regression."
),FALSE)


.sdmMethods$addMethod(.create.sdmCorrelativeMethod(name=c('svm','SVM','ksvm'),
                                                   packages='kernlab',
                                                   modelTypes = c('pa','pb','ab','n'),
                                                   fitParams = list(x='standard.formula',data='sdmDataFrame'),
                                                   fitSettings = list(type='eps-svr',kernel='rbfdot',epsilon=0.1,prob.model=FALSE,tol=0.001,shrinking=TRUE),
                                                   fitFunction = ksvm,
                                                   settingRules = function(x,fitSettings) {
                                                     if (x@distribution == 'n') fitSettings[['type']] <- 'C-svc'
                                                     fitSettings
                                                   },
                                                   tuneParams = NULL,
                                                   predictParams=list(object='model',newdata='sdmDataFrame'),
                                                   predictSettings=list(type='response'),
                                                   predictFunction=kernlab::predict,
                                                   #------ metadata (optional):
                                                   title='Support Vector Machines',
                                                   creator='Babak Naimi',
                                                   authors=c('Alexandros Karatzoglou'), # authors of the main method
                                                   email='naimi.b@gmail.com',
                                                   url='http://r-gis.net',
                                                   citation=list(bibentry('Article',title = "LIBSVM: a library for Support Vector Machines",
                                                                          author = as.person("C. Chih-Chung [aut], L. Chih-Jen [aut]"),
                                                                          year='2015',
                                                                          journal = "http://www.csie.ntu.edu.tw/~cjlin/libsvm"
                                                                          )
                                                   ),
                                                   description='Support Vector Machines are an excellent tool for classification, novelty detection, and regression'
),FALSE)




.sdmMethods$addMethod(.create.sdmCorrelativeMethod(name=c('cart','CART','tree'),
                                                   packages='tree',
                                                   modelTypes = c('pa','pb','ab','n'),
                                                   fitParams = list(formula='standard.formula',data='sdmDataFrame'),
                                                   fitSettings = list(method = "recursive.partition",split = "deviance",x=FALSE,y=FALSE,wts=FALSE,model=FALSE),
                                                   fitFunction = tree,
                                                   settingRules = function(x,fitSettings) {
                                                     fitSettings
                                                   },
                                                   tuneParams = NULL,
                                                   predictParams=list(object='model',newdata='sdmDataFrame'),
                                                   predictSettings=list(type='vector'),
                                                   predictFunction=predict,
                                                   #------ metadata (optional):
                                                   title='Classification or Regression Tree',
                                                   creator='Babak Naimi',
                                                   authors=c('B. D. Ripley'), # authors of the main method
                                                   email='naimi.b@gmail.com',
                                                   url='http://r-gis.net',
                                                   citation=list(bibentry('book',title = "Classification and Regression Trees",
                                                                          author = as.person("L. Breiman [aut], J.H. Friedman [aut], R.A. Olshen [aut], C.J. Stone [aut]"),
                                                                          year='1984',
                                                                          publisher = "Wadsworth"
                                                   )
                                                   ),
                                                   description='A tree is grown by binary recursive partitioning using the response in the specified formula and choosing splits from the terms of the right-hand-side'
),FALSE)



.sdmMethods$addMethod(.create.sdmCorrelativeMethod(name=c('fda','FDA'),
                                                   packages='mda',
                                                   modelTypes = c('pa','pb','n'),
                                                   fitParams = list(formula='standard.formula',data='sdmDataFrame'),
                                                   fitSettings = list(method = polyreg,keep.fitted=FALSE),
                                                   fitFunction = fda,
                                                   settingRules = function(x,fitSettings) {
                                                     fitSettings
                                                   },
                                                   tuneParams = list(method=c(polyreg,mars,gen.ridge,bruto)),
                                                   predictParams=list(object='model',newdata='sdmDataFrame'),
                                                   predictSettings=list(type='posterior'),
                                                   predictFunction=function(object,newdata,type) {
                                                     predict(object,newdata,type=type)[,'1']
                                                   },
                                                   #------ metadata (optional):
                                                   title='Flexible Discriminant Analysis',
                                                   creator='Babak Naimi',
                                                   authors=c('Trevor Hastie; Robert Tibshirani'), # authors of the main method
                                                   email='naimi.b@gmail.com',
                                                   url='http://r-gis.net',
                                                   citation=list(bibentry('Article',title = "Flexible Disriminant Analysis by Optimal Scoring",
                                                                          author = as.person("T. Hastie [aut], R. Tibshirani [aut], Buja [aut]"),
                                                                          year='1994',
                                                                          journal = "JASA",
                                                                          pages="1255-1270"
                                                   )
                                                   ),
                                                   description='Flexible discriminant analysis.'
),FALSE)

.sdmMethods$addMethod(.create.sdmCorrelativeMethod(name=c('mda','MDA'),
                                                   packages='mda',
                                                   modelTypes = c('pa','pb','n'),
                                                   fitParams = list(formula='standard.formula',data='sdmDataFrame'),
                                                   fitSettings = list(method = polyreg,keep.fitted=FALSE),
                                                   fitFunction = mda,
                                                   settingRules = function(x,fitSettings) {
                                                     fitSettings
                                                   },
                                                   tuneParams = list(method=c(polyreg,mars,gen.ridge,bruto)),
                                                   predictParams=list(object='model',newdata='sdmDataFrame'),
                                                   predictSettings=list(type='posterior'),
                                                   predictFunction=function(object,newdata,type) {
                                                     predict(object,newdata,type=type)[,'1']
                                                   },
                                                   #------ metadata (optional):
                                                   title='Mixture Discriminant Analysis',
                                                   creator='Babak Naimi',
                                                   authors=c('Trevor Hastie; Robert Tibshirani'), # authors of the main method
                                                   email='naimi.b@gmail.com',
                                                   url='http://r-gis.net',
                                                   citation=list(bibentry('Article',title = "Flexible Disriminant Analysis by Optimal Scoring",
                                                                          author = as.person("T. Hastie [aut], R. Tibshirani [aut], Buja [aut]"),
                                                                          year='1994',
                                                                          journal = "JASA",
                                                                          pages="1255-1270"
                                                   )
                                                   ),
                                                   description='Mixture discriminant analysis.'
),FALSE)




.sdmMethods$addMethod(.create.sdmCorrelativeMethod(name=c('mars','MARS','earth'),
                                                   packages='earth',
                                                   modelTypes = c('pa','pb','ab','n'),
                                                   fitParams = list(formula='standard.formula',data='sdmDataFrame'),
                                                   fitSettings = list(weights=NULL,pmethod='none',trace=0,glm=list(family=binomial)),
                                                   fitFunction = earth,
                                                   settingRules = function(x,fitSettings) {
                                                     if (x@distribution == 'ab') fitSettings[['glm']] <- list(family=poisson)
                                                     fitSettings
                                                   },
                                                   tuneParams = list(glm=c(NULL,'default')),
                                                   predictParams=list(object='model',newdata='sdmDataFrame'),
                                                   predictSettings=list(type='response'),
                                                   predictFunction=predict,
                                                   #------ metadata (optional):
                                                   title='Multivariate Adaptive Regression Splines',
                                                   creator='Babak Naimi',
                                                   authors=c('Stephen Milborrow'), # authors of the main method
                                                   email='naimi.b@gmail.com',
                                                   url='http://r-gis.net',
                                                   citation=list(bibentry('Article',title = "Multivariate Adaptive Regression Splines",
                                                                          author = as.person("Friedman [aut]"),
                                                                          year='1991',
                                                                          journal = "Annals of Statistics",
                                                                          number="19/1",
                                                                          pages="1-141"
                                                   )
                                                   ),
                                                   description="Build a regression model using the techniques in Friedman's papers 'Multivariate Adaptive Regression Splines' and 'Fast MARS'."
),FALSE)




.sdmMethods$addMethod(.create.sdmCorrelativeMethod(name=c('rpart','RPART'),
                                                   packages='rpart',
                                                   modelTypes = c('pa','pb','ab','n'),
                                                   fitParams = list(formula='standard.formula',data='sdmDataFrame'),
                                                   fitSettings = list(model=FALSE,x=FALSE,y=FALSE,method='anova'),
                                                   fitFunction = rpart,
                                                   settingRules = function(x,fitSettings,predictSettings) {
                                                     if (x@distribution == 'ab') {
                                                       fitSettings[['method']] <- 'poisson'
                                                     } else if (x@distribution == 'n') {
                                                       fitSettings[['method']] <- 'class'
                                                       predictSettings[['type']] <- 'class'
                                                      }
                                                     list(fitSettings=fitSettings,predictSettings=predictSettings)
                                                   },
                                                   tuneParams = NULL,
                                                   predictParams=list(object='model',newdata='sdmDataFrame'),
                                                   predictSettings=list(type='vector'),
                                                   predictFunction=predict,
                                                   #------ metadata (optional):
                                                   title='Recursive Partitioning and Regression Trees',
                                                   creator='Babak Naimi',
                                                   authors=c('Terry Therneau, Beth Atkinson, Brian Ripley'), # authors of the main method
                                                   email='naimi.b@gmail.com',
                                                   url='http://r-gis.net',
                                                   citation=list(bibentry('book',title = "Classification and Regression Trees",
                                                                          author = as.person("L. Breiman [aut], J.H. Friedman [aut], R.A. Olshen [aut], C.J. Stone [aut]"),
                                                                          year='1984',
                                                                          publisher = "Wadsworth"
                                                   )
                                                   ),
                                                   description="Recursive Partitioning and Regression Trees."
),FALSE)


.sdmMethods$addMethod(.create.sdmCorrelativeMethod(name=c('glmnet','GLMNET','glmelastic','glmlasso'),
                                                   packages='glmnet',
                                                   modelTypes = c('pa','pb','ab','n'),
                                                   fitParams = list(formula='standard.formula',data='sdmDataFrame'),
                                                   fitSettings = list(family='binomial',alpha=1),
                                                   fitFunction = function(formula,data,family,...) {
                                                     x <- .getData.sdmMatrix(formula,data)
                                                     y <- .getData.sdmY(formula,data)
                                                     if (family == 'binomial') m <- cv.glmnet(x,y,family="binomial",type.measure = 'auc')
                                                     else m <- cv.glmnet(x,y,family=family)
                                                     glmnet(x=x,y=y,family=family,lambda=m$lambda.1se,...)
                                                   },
                                                   settingRules = function(x,fitSettings,predictSettings) {
                                                     if (x@distribution == 'ab') {
                                                       fitSettings[['family']] <- 'poisson'
                                                     } else if (x@distribution == 'n') {
                                                       fitSettings[['family']] <- 'multinomial'
                                                     }
                                                     list(fitSettings=fitSettings,predictSettings=predictSettings)
                                                   },
                                                   tuneParams = NULL,
                                                   predictParams=list(object='model',formula='standard.formula',newx='sdmDataFrame'),
                                                   predictSettings=list(type='response'),
                                                   predictFunction=function(object,formula,newx,type) {
                                                     newx <- .getData.sdmMatrix(formula,newx)
                                                     predict.glmnet(object,newx,type=type)[,1]
                                                   },
                                                   #------ metadata (optional):
                                                   title='GLM with lasso or elasticnet regularization',
                                                   creator='Babak Naimi',
                                                   authors=c('Jerome Friedman, Trevor Hastie, Noah Simon and Rob Tibshirani'), # authors of the main method
                                                   email='naimi.b@gmail.com',
                                                   url='http://r-gis.net',
                                                   citation=list(bibentry('Article',title = "Regularization Paths for Generalized Linear Models via Coordinate Descent,",
                                                                          author = as.person("J. Friedman [aut], T. Hastie [aut], R. Tibshirani [aut]"),
                                                                          year='2008',
                                                                          journal = "Journal of Statistical Software",
                                                                          number="33/1",
                                                                          pages="1-22"
                                                   )
                                                   ),
                                                   description="Fit a generalized linear model via penalized maximum likelihood. The regularization path is computed for the lasso or elasticnet penalty at a grid of values for the regularization parameter lambda."
),FALSE)



.sdmMethods$addMethod(.create.sdmCorrelativeMethod(name=c('maxlike','MaxLike'),
                                                   packages='base',
                                                   modelTypes = c('pb'),
                                                   fitParams = list(formula='standard.formula',data='sdmDataFrame'),
                                                   fitSettings = list(link='logit',hessian=TRUE,normalize=TRUE),
                                                   fitFunction = .maxlike,
                                                   settingRules = function(x,fitSettings,predictSettings) {
                                                     #
                                                   },
                                                   tuneParams = NULL,
                                                   predictParams=list(object='model',newdata='sdmDataFrame'),
                                                   predictSettings=NULL,
                                                   predictFunction=predict,
                                                   #------ metadata (optional):
                                                   title='Model occurrence probability using presence-only data',
                                                   creator='Babak Naimi',
                                                   authors=c('Richard Chandler and Andy Royle'), # authors of the main method
                                                   email='naimi.b@gmail.com',
                                                   url='http://r-gis.net',
                                                   citation=list(bibentry('Article',title = "Likelihood analysis of species occurrence probability from presence-only data for modelling species distributions",
                                                                          author = as.person("J.A. Royle [aut], R.B. Chandler [aut], C. Yackulic [aut], J. D. Nichols [aut]"),
                                                                          year='2012',
                                                                          journal = "Methods in Ecology and Evolution"
                                                                          
                                                   )
                                                   ),
                                                   description="Estimates the probability of occurrence using presence-only data and covariates as background data."
),FALSE)
#----

# 
# #################
# .sdmMethods$getFitFunctions(c('glm','brt'))
# 
# 
# if (!isGeneric("newMethod")) {
#   setGeneric("newMethod", function(type,...)
#     standardGeneric("newMethod"))
# }
# 
# 
# setMethod('newMethod', signature(type='character'), 
#           function(type,...) {
#             
#           }
# )
# 
# 
# 
# 
# 
# 
# 
# m@fitFunction
# m@predictFunction
# m@.temp.env
# m@metadata
# m@settingRules
# m <- eval(m)
# saveRDS(m,'glm_model.rds')
# saveRDS(.create.sdmCorrelativeMethod(name='glm',modelTypes = 'pa',fitParams = list(formula='sdmFormula',data='sdmDataFrame'),
#                                      fitSettings = list(family=binomial(link='logit'),weights=NULL,model=FALSE),fitFunction = glm,
#                                      predictParams=list(object='sdmModel',newdata='sdmDataFrame'),
#                                      predictSettings=list(type='response'),predictFunction=predict.glm),file='glm_model.rds')
# 
# readRDS('glm_model.rds')
# save(m,file='glm_model.RData')
# eval(get(load('glm_model.RData')))
# rm(m)
# sa
# m
# 
# #############
# m <- .create.sdmCorrelativeMethod(name='glm',modelTypes = 'pa',fitParams = list(formula='sdmFormula',data='sdmDataFrame'),
#                                   fitSettings = list(family=binomial(link='logit'),weights=NULL,model=FALSE),fitFunction = glm,
#                                   predictParams=list(object='sdmModel',newdata='sdmDataFrame'),
#                                   predictSettings=list(type='response'),predictFunction=predict.glm)
# 
# 
# a <- .sdmMethods$new()
# 
# a$addMethod(m)
# a$getPredictFunctions()


