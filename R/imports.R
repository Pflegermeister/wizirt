#' @import mirt
#' @import PerFit
#' @import ltm
#' @import parsnip

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom parsnip set_engine
#' @export
parsnip::set_engine

#' @importFrom parsnip set_mode
#' @export
parsnip::set_mode


.onLoad <- function(libname, pkgname){
  a <- capture.output(tryCatch(make_irt(), error = function (e){}))
  rm(a)
}

