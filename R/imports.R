#' @import mirt
#' @import PerFit
#' @import ltm
#' @import parsnip

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' parsnip::set_engine
#'
#' @param engine for set_engine(). Character. Currently supported engines are 'mirt' and 'ltm' for Rasch, 1PL, 2PL, and 3PL models. 'eRm' is supported for Rasch models only.
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

