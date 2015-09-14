# Author: Babak Naimi, naimi.b@gmail.com
# Date :  July 2014
# Version 1.0
# Licence GPL v3

.cmx <- function(o,p) {
  cmx<-matrix(nrow=2,ncol=2)
  colnames(cmx) <- rownames(cmx) <- c('P','A')
  cmx[1,1] <- length(which(o == 1 & p == 1))
  cmx[2,2] <- length(which(o == 0 & p == 0))
  cmx[1,2] <- length(which(o == 0 & p == 1))
  cmx[2,1] <- length(which(o == 1 & p == 0))
  cmx[] <- as.numeric(cmx)
  cmx
}


.evaluate.cmx <- function(cmx) {
  TP<-cmx[1,1];FP<-cmx[1,2];TN<-cmx[2,2];FN<-cmx[2,1]
  N <- sum(cmx)
  prev <- (TP+FN) / N
  pred.prev <- (TP + FP) / N
  ccr <- (TP+TN) / N
  sens <- TP / (TP + FN)
  spec <- TN / (FP + TN)
  ppv <- TP / (TP + FP)
  npv <- TN / (TN + FN)
  or <- (TP * TN) / (FP * FN)
  commission = FP/(FP + TN)
  ommission = FN/(TP + FN)
  mcr = (FP + FN)/N
  phi <- (TP*TN - FP*FN)/(sqrt((TP+FN)*(TN+FP)*(TP+FP)*(TN+FN)))
  
  Kappa <- ((TN + TP) - ((((TP + FN)*(TP + FP)) + ((FP + TN)*(FN + TN))) /N)) / 
    (N-((((TP+FN)*(TP+FP))+((FP+TN)*(FN+TN)))/N))
  
  v <- as.vector(cmx) ; pp <- apply(cmx,1,sum) ; op <- apply(cmx,2,sum)
  v <- ifelse(v == 0,0.00001,v)
  pp <- ifelse(pp == 0,0.00001,pp)
  op <- ifelse(op == 0,0.00001,op)
  NMI <- 1-((-sum(v*log(v)) + sum(pp*log(pp))) / (N*log(N) - sum(op*log(op))))
  TSS <- sens+spec-1
  return(round(c(sensitivity=sens,specificity=spec,TSS=TSS,Kappa=Kappa,NMI=NMI,phi=phi,ppv=ppv,npv=npv,ccr=ccr,
                 mcr=mcr,or=or,ommission=ommission,commission=commission,predicted.prevalence=pred.prev,prevalence=prev),3))
}
#---- 
#-----------
.auc <- function(o,p) {
  w1 <- which(o == 1)
  w0 <- which(o == 0)
  auc <- as.numeric(NA)
  if (length(w1) > 0 & length(w0) > 0) {
    auc <- as.vector(wilcox.test(p[w1], p[w0])$statistic)/(length(w1)*length(w0))
  }
  round(auc,3)
}
#-----

.cor <- function(o,p) {
  co <- try(cor(p, o), silent = TRUE)
  if (class(co) != "try-error") {
    return(round(co,3))
  } else return(as.numeric(NA))
}

#-------
.roc <- function(o,p) {
  th <- as.vector(quantile(p,0:100/100))
  e <- matrix(nrow=length(th),ncol=2)
  colnames(e) <- c('sensitivity','1-specificity')
  
  for (i in seq_along(th)) {
    w <- which(p >= th[i])
    pt <- rep(0,length(p))
    pt[w] <- 1
    e[i,] <- .evaluate.cmx(.cmx(o,pt))[1:2]
  }
  e[,2] <- 1-e[,2]
  e <- rbind(e,c(0,0))
  e[,c(2,1)]
}
#-----------
# 
# if (!isGeneric("sdmEvaluate")) {
#   setGeneric("sdmEvaluate", function(data,model,...)
#     standardGeneric("sdmEvaluate"))
# }  
# 
# 
# setMethod('sdmEvaluate', signature(model='sdmModel'),
#           function(data, model,...) {
#             #
#           }
# )


.evaluate <- function(o,p) {
  e <- new('sdmEvaluate')
  e@observed <- o
  e@predicted <- p
  e@AUC <- .auc(o,p)
  e@COR <- .cor(o,p)
  e@threshod_based <- .threshold(o,p,stat=c(1:9,14))
  e@prevalence <- round(length(which(o == 1)) / length(o),3)
  e
}

if (!isGeneric("evaluates")) {
  setGeneric("evaluates", function(o,p,...)
    standardGeneric("evaluates"))
}  

setMethod('evaluates', signature(o='vector',p='vector'),
          function(o, p,...) {
            o <- .occurrence(o)
            w <- which(!is.na(o) & !is.na(p))
            p <- p[w]; o <- o[w]
            if (length(o) != length(p)) stop('observed and predicted vectors should have the same length')
            if (!.isBinomial(o) || !all(sort(unique(o)) == c(0,1))) stop('observed data should have 1 and 0 records (i.e., presence-absence)...')
            .evaluate(o,p)
          }
)

if (!isGeneric("getRoc")) {
  setGeneric("getRoc", function(o,p,...)
    standardGeneric("getRoc"))
}  

setMethod('getRoc', signature(o='vector',p='vector'),
          function(o, p) {
            o <- .occurrence(o)
            w <- which(!is.na(o) & !is.na(p))
            p <- p[w]; o <- o[w]
            if (length(o) != length(p)) stop('observed and predicted vectors should have the same length')
            if (!.isBinomial(o) || !all(sort(unique(o)) == c(0,1))) stop('observed data should have 1 and 0 records (i.e., presence-absence)...')
            .roc(o,p)
          }
)
