# Author: Babak Naimi, naimi.b@gmail.com
# Date :  June 2014
# Version 2.1
# Licence GPL v3


setClassUnion("vectorORnull", c("vector", "NULL")) 
setClassUnion("characterORnull", c("character", "NULL"))
setClassUnion("CRSorNULL", c("CRS", "NULL"))
setClassUnion("formulaORnull", c("formula", "NULL"))
setClassUnion("numericORnull", c("numeric", "NULL"))
setClassUnion("characterORmissing", c("character", "missing"))
setClassUnion("listORnull", c("list", "NULL"))

setClass(".Index",
         representation(
           index.name = 'character',
           ID='numeric',
           time='vectorORnull'
         ),
         prototype(
           index.name = '',
           ID=c(),
           time=NULL
         ),
         validity = function(object)  {
           return(TRUE)
         }
)
#-------
setClass(".Occurrence",
         representation(
           species.name = 'character',
           Occurrence='vector',
           index='.Index',
           presence.only='logical',
           pseudo.absense.index='numericORnull',
           pseudo.absense.replicates='numericORnull',
           pseudo.absence.method='characterORnull' # if length=1, the same method used for all replicates, otherwise, the first item is for first replicate, and so on
         ),
         prototype(
           species.name = '',
           pseudo.absense.index=NULL,
           pseudo.absense.replicates=NULL,
           pseudo.absence.method=NULL
          ),
         validity = function(object)  {
           return(length(object@Occurrence) == length(object@index@ID))
         }
)
#-------
setClass(".multipleOccurrence",
         representation(
           species.names = 'character',
           Occurrence='matrix',
           index='.Index',
           time='vectorORnull',
           presence.only='logical',
           pseudo.absense.index='numericORnull',
           pseudo.absense.replicates='numericORnull',
           pseudo.absence.method='characterORnull'
         ),
         prototype(
           time=NULL,
           pseudo.absense.index=NULL,
           pseudo.absense.replicates=NULL,
           pseudo.absence.method=NULL
         ),
         validity = function(object)  {
           return(nrow(object@Occurrence) == length(object@index@ID))
         }
)
#-------

setClass(".Features",
         representation(
           featureNames = 'character',
           factors='characterORnull',
           features='data.frame',
           index='.Index'
         ),
         prototype(
           featureNames = '',
           factors=NULL
         ),
         validity = function(object)  {
           v1 <- ncol(object@features) == length(object@featureNames)
           v2 <- nrow(object@features) == length(object@index@ID)
           v3 <- TRUE
           if (!is.null(object@factors)) v3 <- all(unlist(lapply(object@factors,function(x) {is.factor(object@features[,x])})))
           return(v1 & v2 & v3)
         }
)
#-------------

setClass(".coords",
         representation(
           coords = 'matrix',
           index='.Index',
           crs='CRSorNULL'
         ),
         prototype(
           crs=NULL
         ),
         validity = function(object)  {
           return(nrow(object@coords) == length(object@index@ID))
         }
)
#-------
setClassUnion(".IndexORnull", c(".Index", "NULL"))
setClassUnion(".coordsORnull", c(".coords", "NULL"))
setClassUnion(".FeaturesORnull", c(".Features", "NULL"))


setClass(".singleSpeciesData",
         representation(
           Occurrence='.Occurrence',
           Features='.FeaturesORnull',
           coordinates='.coordsORnull'
         ),
         prototype(
           Features=NULL,
           coordinates=NULL
         ),
         validity = function(object)  {
           v1 <- v2 <- TRUE
           if (!is.null(object@Features)) v1 <- validObject(object@Features)
           if(!is.null(object@coordinates)) v2 <- validObject(object@coordinates)
           return(validObject(object@Occurrence) & v1 & v2)
         }
)
#-------

setClass(".multipleSpeciesData",
         representation(
           Occurrence='.multipleOccurrence',
           Features='.FeaturesORnull',
           coordinates='.coordsORnull'
         ),
         prototype(
           Features=NULL,
           coordinates=NULL
         ),
         validity = function(object)  {
           v1 <- v2 <- TRUE
           if (!is.null(object@Features)) v1 <- validObject(object@Features)
           if(!is.null(object@coordinates)) v2 <- validObject(object@coordinates)
           return(validObject(object@Occurrence) & v1 & v2)
         }
)
#--------------
setClass(".SpeciesDataList",
         representation(
           species.names='character',
           SpeciesDataList='list'
         ),
         validity = function(object)  {
           return(TRUE)
         }
)
#-----------
setClass(".FileSDMdata",
         representation(
           name='character',
           keepRasters='logical',
           RasterFile='character'
          
           ),
         prototype(
           name='',
           keepRasters=FALSE,
           RasterFile=''
           )
         )
#--------
setClassUnion(".singleSpeciesDataORnull", c(".singleSpeciesData", "NULL"))
setClassUnion(".multipleSpeciesDataORnull", c(".multipleSpeciesData", "NULL"))
setClassUnion(".SpeciesDataListORnull", c(".SpeciesDataList", "NULL"))

#-------

setClass("singleSpecies",
         representation(
           train='.singleSpeciesData',
           test='.singleSpeciesDataORnull'
           
         ),
         prototype(
           test=NULL
         ),
         validity = function(object)  {
           v1 <- TRUE
           if (!is.null(object@test)) v1 <- validObject(object@test)
           return(validObject(object@train) & v1)
         }
)
#--------
setClass("multipleSpecies",
         representation(
           train='.multipleSpeciesData',
           test='.multipleSpeciesDataORnull'
           
         ),
         prototype(
           test=NULL
         ),
         validity = function(object)  {
           v1 <- TRUE
           if (!is.null(object@test)) v1 <- validObject(object@test)
           return(validObject(object@train) & v1)
         }
)
#--------
setClass("SpeciesDataList",
         representation(
           train='.SpeciesDataList',
           test='.SpeciesDataListORnull'
           
         ),
         prototype(
           test=NULL
         ),
         validity = function(object)  {
           v1 <- TRUE
           if (!is.null(object@test)) v1 <- validObject(object@test)
           return(validObject(object@train) & v1)
         }
)

setClassUnion("speciesData", c("singleSpecies", "multipleSpecies", "SpeciesDataList"))
#---------
#############
#------------
# setClass(".modelSettings",
#          representation(
#            models='character',
#            modelSettings='list'
#          ),
#          prototype(
#            models=c(),
#            modelSettings=list()
#          ),
#          validity=function(object) {
#            return(sort(models) == sort(names(modelSettings) ))
#          }
# )
#---------
# 
# setClass(".featureSettings",
#          representation(
#            featureNames='character',
#            feature.types='character',
#            interaction.depth='numeric',
#            formula='formulaORnull'
#          ),
#          prototype(
#            featureNames=c(),
#            feature.types='auto',
#            interaction.depth=1,
#            formula=NULL
#          ),
#          validity=function(object) {
#            return(TRUE)
#          }
# )
# #-----
# setClassUnion(".featureSettingsORnull", c(".featureSettings", "NULL"))


#------
setClass(".pseudo.absence",
         representation(
           method = 'character',
           n.replicates='numeric',
           featureNames='character',
           coord.names='character',
           pseudo.absence='list'
         ),
         validity = function(object)  {
           return(TRUE)
         }
)
#---------

setClass(".replicates",
         representation(
           method = 'character',
           n.replicates='numeric',
           fold='list',
           index.name='character',
           row.index='list'
         ),
         validity = function(object)  {
           return(TRUE)
         }
)

setClassUnion(".replicatesORnull", c(".replicates", "NULL"))
#------
setClass(".basicSettings",
         representation(
           featureNames='character',
           feature.types='listORnull',
           interaction.depth='numeric',
           formula='formulaORnull',
           replicates='.replicatesORnull',
           test.percentage='numericORnull',
           pseudo.absence.method='characterORnull',
           pseudo.absence.number='numericORnull',
           variable.importance='logical',
           variable.selection='characterORnull'
         ),
         prototype(
           featureNames=c(),
           feature.types=NULL,
           interaction.depth=1,
           formula=NULL,
           replicates=NULL,
           test.percentage=NULL,
           pseudo.absence.method=NULL,
           pseudo.absence.number=NULL,
           variable.importance=TRUE,
           variable.selection=NULL
         ),
         validity=function(object) {
           return(TRUE)
         }
)



setClassUnion(".basicSettingsORnull", c(".basicSettings", "NULL"))


setClass("sdmSettings",
         representation(
           methods='character',
           basicSettings='.basicSettingsORnull',
           modelSettings='list'
         ),
         prototype(
           basicSettings=NULL,
           modelSettings=list()
         ),
         validity=function(object) {
           v1 <- TRUE
           if (length(object@methods) > 0) v1 <- all(sort(object@methods) == sort(names(object@modelSettings)))
           return(v1)
         }
)

#-----

##################################

setClass(".glmSettings",
         representation(
           basicSettings='.basicSettingsORnull',
           family="ANY",
           otherSettings='list',
           predictSettings='list'
         ),
         prototype(
           basicSettings=NULL,
           family=binomial(link='logit'),
           otherSettings=list(),
           predictSettings=list(type='response')
         ),
         validity=function(object) {
           return(TRUE)
         }
)

#------

setClass(".brtSettings",
         representation(
           basicSettings='.basicSettingsORnull',
           distribution = "character",
           n.trees = "numeric",
           interaction.depth = "numeric",
           n.minobsinnode = "numeric",
           shrinkage = "numeric",
           bag.fraction = "numeric",
           train.fraction = "numeric",
           cv.folds="numeric",
           otherSettings='list',
           predictSettings='list'
           
         ),
         prototype(
           basicSettings=NULL,
           distribution = "bernoulli",
           n.trees = 1000,
           interaction.depth = 1,
           n.minobsinnode = 10,
           shrinkage = 0.001,
           bag.fraction = 0.5,
           train.fraction = 1.0,
           cv.folds=0,
           otherSettings=list(),
           predictSettings=list(n.trees=1000,type='response')
         ),
         validity=function(object) {
           return(TRUE)
         }
)

#--------
setClass(".rfSettings",
         representation(
           basicSettings='.basicSettingsORnull',
           ntree='numeric',
           importance='logical',
           otherSettings='list',
           predictSettings='list'
         ),
         prototype(
           basicSettings=NULL,
           ntree=1000,
           importance=FALSE,
           otherSettings=list(),
           predictSettings=list(type='response')
         ),
         validity=function(object) {
           return(TRUE)
         }
)

#-----------


setClass(".svmSettings",
         representation(
           basicSettings='.basicSettingsORnull',
           type="character",
           otherSettings='list',
           predictSettings='list'
         ),
         prototype(
           basicSettings=NULL,
           type="eps-svr",
           otherSettings=list(),
           predictSettings=list(type='response')
         ),
         validity=function(object) {
           return(TRUE)
         }
)
#-------
setClass(".gamSettings",
         representation(
           basicSettings='.basicSettingsORnull',
           family='ANY',
           otherSettings='list',
           predictSettings='list'
         ),
         prototype(
           basicSettings=NULL,
           family=binomial,
           otherSettings=list(),
           predictSettings=list(type='response')
         ),
         validity=function(object) {
           return(TRUE)
         }
)
#-----------

setClass(".maxentSettings",
         representation(
           basicSettings='.basicSettingsORnull',
           factors = "characterORnull",
           args = "characterORnull",
           removeDuplicates = "logical",
           path = "characterORnull",
           otherSettings='list',
           predictSettings='list'
         )
)

###############

setClass("sdmEvaluate",
         representation(
           observed='numeric',
           predicted='numeric',
           prevalence='numeric',
           AUC='numeric',
           COR='numeric',
           threshod_based='data.frame'
         ),
         validity=function(object) {
           return(TRUE)
         }
)
#------------
setClass("sdmEvaluates",
         representation(
           model='character',
           n.run='numeric',
           scenarios='data.frame',
           evaluationList='list'
         ),
         validity=function(object) {
           return(TRUE)
         }
)
#----------
setClass(".sdmModels",
         representation(
           model='character',
           n.run='integer',
           run.index='integer',
           run.success='logical',
           scenarios='data.frame',
           modelObjectList='list',
           evaluation='sdmEvaluates'
         ),
         prototype(
           n.run=as.integer(0),
           run.index=as.integer(0),
           run.success=logical(),
           modelObjectList=list(),
           evaluation=new('sdmEvaluates')
         ),
         validity=function(object) {
           return(TRUE)
         }
)
#----

setClass(".sdmMultiModel",
         representation(
           species.name='character',
           modelList='list'
         ),
         validity=function(object) {
           return(TRUE)
         }
)

#-----------
setClass(".sdmMultiModelList",
         representation(
           species.names='character',
           multiModelList='list'
         ),
         prototype(
           species.names=character(),
           multiModelList=list()
         ),
         validity=function(object) {
           return(length(object@species.names) == length(object@multiModelList))
         }
)


setClass("sdmModel",
         representation(
           data='speciesData',
           settings='sdmSettings',
           models='.sdmMultiModelList'
         ),
         validity=function(object) {
           return(TRUE)
         }
)
#------

#------

