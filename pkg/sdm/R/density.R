# Author: Babak Naimi, naimi.b@gmail.com
# Date :  July. 2016
# Version 1.0
# Licence GPL v3


if (!isGeneric("density")) {
  setGeneric("density", function(x, ...)
    standardGeneric("density"))
}


setMethod('density', signature(x='sdmEvaluate'), 
          function(x,...) {
            p=x@predicted
            x=x@observed
            w <- which(!is.na(p))
            p <- p[w]; x <- x[w]
            w <- which(!is.na(x))
            p <- p[w]; x <- x[w]
            
            w1 <- which(x == 1)
            w2 <- which(x == 0)
            if (length(w1) > 0) {
              d <- density(p[w1])
              plot(d,col='darkblue',xlim=c(0,1),lwd=2,main='',xlab='Predicted Probabilities')
              if (length(w2) > 0) {
                lines(density(p[w2],bw=d$bw),col='red',lwd=2)
                legend('topleft',legend=c('Presence','Absence'),col=c('darkblue','red'),lty=c(1,1),text.width = 0.2,lwd=2)
              } else {
                legend('topleft',legend=c('Presence'),col=c('darkblue'),lty=c(1),text.width = 0.2,lwd=2)
              }
            } else {
              if (length(w2) > 0) {
                plot(p[w2],col='red',xlim=c(0,1),lwd=2,main='',xlab='Predicted Probabilities')
                legend('topleft',legend=c('Absence'),col=c('red'),lty=c(1),text.width = 0.2,lwd=2)
              }
            }
          
          }
)
#---------
