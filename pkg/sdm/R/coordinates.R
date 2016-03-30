# Author: Babak Naimi, naimi.b@gmail.com
# Date :  Feb. 2015
# Version 1.0
# Licence GPL v3

#-------------
setMethod('coordinates', signature(obj='sdmdata'),
          function(obj,...) {
            if (!is.null(obj@info) && !is.null(obj@info@coords)) obj@info@coords[,2:3]
          }
)
