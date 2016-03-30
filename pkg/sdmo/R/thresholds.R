# Author: Babak Naimi, naimi.b@gmail.com
# Date :  July 2014
# Version 1.0
# Licence GPL v3


.statFix <- function(x) {
  if (is.numeric(x)) {
    x <- ifelse(x > 15,15,x)
    x <- ifelse(x < 1,1,x)
    x <- unique(x)
    x <- c('sensitivity','specificity','TSS','Kappa','NMI','phi','ppv','npv','ccr','mcr','or','ommission','commission','predicted.prevalence')[x]
  } else {
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
      else if (any(!is.na(pmatch(c("ph"),x[i])))) x[i] <- 'phi'
    }
    x <- unique(x)
    w <- which(x %in% c('sensitivity','specificity','TSS','Kappa','NMI','phi','ppv','npv','ccr','mcr','or','ommission','commission','predicted.prevalence'))
    x <- x[w]
  }
  x
}


.threshold <- function(o,p,th,stat=0) {
  if (missing(th)) th <- sort(unique(p))
  else th <- sort(unique(th))
  e <- matrix(nrow=length(th),ncol=16)
  colnames(e) <- c('threshold','sensitivity','specificity','TSS','Kappa','NMI','phi','ppv','npv','ccr',
                   'mcr','or','ommission','commission','prevalence','obsPrevalence')
  
  e[,1] <- th
  for (i in seq_along(th)) {
    w <- which(p >= th[i])
    pt <- rep(0,length(p))
    pt[w] <- 1
    e[i,2:16] <- .evaluate.cmx(.cmx(o,pt))
  }
  
  w <- which(is.na(e[,"ppv"]) | is.na(e[,'npv']))
  if (length(w) > 0) e <- e[-w,]
  
  # 1: Se=SP
  w1 <- which.min(abs(e[,"sensitivity"] - e[,"specificity"]))
  # 2: Max(Se+Sp)
  w2 <- which.max(e[,"sensitivity"]+e[,"specificity"])
  # 3: Min.cost:
  w3 <- which.min((1 - e[,"sensitivity"])*e[,"prevalence"] + (1 - e[,"specificity"] )*(1 - e[,"prevalence"]))
  # 4: ROC:
  w4 <- which.min(((1 - e[,"sensitivity"])^2) + ((e[,"specificity"] -1)^2))
  # 5: Max(Kappa):
  w5 <- which.max(e[,"Kappa"])
  # 6: Max(npv+ppv) 
  w6 <- which.max(e[,"ppv"] + e[,"npv"])
  # 7: ppv=npv
  w7 <- which.min(abs(e[,"ppv"] - e[,"npv"]))
  # 8: Max(NMI):
  w8 <- which.max(e[,"NMI"])
  # 9: Max(CCR):
  w9 <- which.max(e[,"ccr"])
  # 10: PredictedPrevalence=ObservedPrevalence
  w10 <- which.min(abs(e[,"prevalence"] - e[,"obsPrevalence"]))
  # 11: Fixed_sensitivity:
  #w11 <- which.min(e[,"sensitivity"] > se)
  
  th.criteria <- c("sp=se","max(se+sp)","min(cost)","minROCdist","max(kappa)","max(ppv+npv)","ppv=npv","max(NMI)","max(ccr)","prevalence")
  
  th <- e[c(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10),unique(c(1,stat+1))]
  data.frame(criteria=th.criteria,th)
}


.getEvalThresholds <- function(o,p,stat) {
  th <- sort(unique(p))
  e <- matrix(nrow=length(th),ncol=2)
  colnames(e) <- c('threshold',stat)
  
  e[,1] <- th
  for (i in seq_along(th)) {
    w <- which(p >= th[i])
    pt <- rep(0,length(p))
    pt[w] <- 1
    e[i,2] <- .evaluate.cmx(.cmx(o,pt))[stat]
  }
  if (stat == 'ppv' | stat == 'npv') e <- e[which(!is.na(e[,stat])),]
  e
}
#-----------
# 
# 
if (!isGeneric("thresholds")) {
  setGeneric("thresholds", function(x, ...)
    standardGeneric("thresholds"))
}	


setMethod('thresholds', signature(x='sdmEvaluate'),
          function(x, stat='',  ...) {
            #
          }
)

