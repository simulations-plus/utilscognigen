# Copyright 2020-$date Cognigen Corporation, a Simulations Plus Company

.onLoad <- function(libname, pkgname) {
  
  packageStartupMessage(
    sprintf(
      'Loading utilscognigen Version %s',
      utils::packageVersion('utilscognigen')
    )
  )
  
}

# quiets R CMD check note
utils::globalVariables('ioenv')
