# Author: Babak Naimi, naimi.b@gmail.com
# Date :  July 2014
# Version 1.0
# Licence GPL v3

.filebase <- function (x, overwrite=FALSE) {
  x <- trim(x)
  if (identical(basename(x), x)) x <- file.path(getwd(), x)
  
  if (file.exists(x)) {
    if (!file.info(x)$isdir) stop(paste("a file with a similar name to", basename(x),"exists, use another name or delete the file!"))
    if (!overwrite) stop(paste(basename(x),"exists. use overwrite=TRUE to overwrite it"))
    if (length(dir(x)) > 0 & length(dir(x,pattern="sdm$")) == 0) stop(paste(x,"can not be overwritten as it is not an sdm Object; use another name or delete the folder!"))
    unlink(x, recursive = TRUE,force=TRUE)
  }
  dir.create(x,recursive = TRUE)
  file.exists(x)
}



if (!isGeneric("write.sdm")) {
  setGeneric("write.sdm", function(object, filename, overwrite=FALSE,...)
    standardGeneric("write.sdm"))
}


setMethod ('write.sdm' , signature(object='sdmModel', filename='character'),
           function (object, filename, overwrite=FALSE, keep.raster=TRUE,...) {
#              if (.filebase(filename,overwrite=overwrite)) {
               
#                writeRaster(x@raster,paste(filename,'/',basename(filename),'.grd',sep=""),format="raster",datatype=datatype,bandorder=bandorder)
#                write(paste("[General]\nCreator= R package 'sdm'\ncreated= ",Sys.time(),
#                            "\n[time]\ntclass = ",attr(x@time,"tclass")[1],
#                            "\ntzone = ",attr(x@time,"tzone"),"\n[Data]",sep=""),
#                      file=paste(filename,'/',basename(filename),'.sdm',sep=""))
#                write.table(data.frame(strftime(index(x@time)),as.vector(x@time)),
#                            row.names=F,file=paste(filename,'/',basename(filename),'.rts',sep=""),
#                            sep=",",append=T,col.names=F)
#              } else stop("File is not cereated!")
           }
)


if (!isGeneric("read.sdm")) {
  setGeneric("read.sdm", function(filename,...)
    standardGeneric("read.sdm"))
}

setMethod ('read.sdm' ,signature(filename='character'),
           function(filename,which) {
#              filename <- trim(filename)
#              if (identical(basename(filename), filename)) filename <- file.path(getwd(), filename)
#              n <- paste(filename,"/",basename(filename),sep="")
#              nn <- paste(n,".rts",sep="")
#              if (file.exists(nn)) {
#                tclass <- strsplit(readLines(nn,6)[5]," = ")[[1]][2]
#                tzone <- strsplit(readLines(nn,6)[6]," = ")[[1]][2]
#                dt <- read.table(nn,header=FALSE,skip=7,sep=",")
#                dt[,1] <- as.character(dt[,1])
#                if (tclass == "POSIXct") time <- as.POSIXct(dt[,1],tz=ifelse(is.na(tzone),"",tzone))
#                if (tclass == "Date") time <- as.Date(dt[,1],tz=ifelse(is.na(tzone),"",tzone))
#                if (tclass == "yearmon") time <- as.yearmon(dt[,1])
#                if (tclass == "yearqtr") time <- as.yearqtr(dt[,1])
#                o <- new("RasterBrickTS")
#                o@raster <- brick(paste(n,".grd",sep=""))
#                o@time <- xts(dt[,2],time)
#                return(o)
#              } else stop("The specified file does not exist or is not a raster time series!")
           }
)