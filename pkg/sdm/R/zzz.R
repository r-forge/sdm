.onLoad <- function(libname, pkgname) {
  pkg.info <- utils::packageDescription('sdm') 
  packageStartupMessage(paste("sdm ", pkg.info[["Version"]], " (", pkg.info["Date"], ")", sep=""))
  #.containers_env <<- new.env()
  #assign('a',.replicateMethods$new(),envir = .containers_env)
#   if (length(ls(envir = .sdmMethods$userFunctions)) > 0) {
#     e <- .sdmMethods$userFunctions
#     .movEnv2sdm(e)
#   }
  invisible(0)
}

.onUnload <- function(libpath) {
  if (".sdmMethods$userFunctions" %in% search()) detach('.sdmMethods$userFunctions')
  invisible(0)
}
