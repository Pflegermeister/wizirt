#' Plot method for wizirt objects
#'
#' @description The plot method for wizirt objects.
#' @param x an object returned from fit_wizirt().
#' @param type Character. Must be 'tech', 'desc', 'na_item', 'na_person'. See details for more information.
#' @details type must be one of
##' \itemize{
##'  \item{"tech"}{A table of technical information from the estimation of the model}
##'  \item{"desc"}{A table of summary information about the data and the estimated parameters.}
##'  \item{"na_item"}{A table of the number and proportion of missing data for each item.}
##'  \item{"na_person"}{A table of the number and proportion of missing data for each person.}
##'  \item{"item"}{A table of item statistics.}
##'  \item{"person"}{A table of person statistics.}
##' }
#' @method print wizirt
#' @export
print.wizirt <- function(x, type = 'tech'){

  if (!type %in% c('tech', 'desc', 'na_item', 'na_person')) {
    rlang::abort(glue::glue('Print method "{type}" is not available.'))
  }
  if(type == 'tech'){

    parms = c('package',
              'function',
              'version',
              'call',
              'factors',
              'item type',
              'converged',
              'method',
              'log-likelihood',
              'criteria',
              'iterations')

    vals = c(x$fit$model$engine$pkg,
             x$fit$model$engine$func,
             paste(x$fit$model$engine$ver),
             paste(trimws(capture.output(x$fit$model$engine$call)), collapse = ""),
             x$fit$model$n_factors,
             x$fit$model$item_type,
             x$fit$estimation$convergence,
             x$fit$estimation$method,
             x$fit$estimation$log_lik,
             x$fit$estimation$criteria,
             x$fit$estimation$iterations)

    tab <- tibble::tibble(parameter = parms,
                   value = vals)

  }
  if(type == 'desc'){
    parms <- c('N Items',
               'Avg Difficulty',
               'Avg Diff (CTT)',
               'N Persons',
               'Avg Ability',
               'Avg % Correct',
               'Avg % Completion')
    vals <- c(
      ncol(x$fit$data),
      round(mean(x$fit$parameters$coefficients$difficulty,
                 na.rm =T), 2),
      round(mean(colMeans(x$fit$data, na.rm = T)),2),
      nrow(x$fit$data),
      round(mean(x$fit$parameters$persons$ability,
                 na.rm = T),2),
      round(mean(rowMeans(replace(x$fit$data, is.na(x$fit$data), 0))),2)*100,
      round(mean(rowMeans(!is.na(x$fit$data), na.rm = T)), 2)*100
    )

    tab <- tibble::tibble(parameter = parms,
                   value = vals)

  }
  if(type == 'na_item') {
    tab <- data.frame(cbind(count = colSums(is.na(x$fit$data)),
                            prop = colMeans(is.na(x$fit$data)))) %>%
      tibble::rownames_to_column('item') %>%
      tibble::tibble()
  }
  if(type == 'na_person') {


    tab <- tibble::tibble(person = x$fit$parameters$persons$ids,
                          count = rowSums(is.na(x$fit$data)),
                          prop = rowMeans(is.na(x$fit$data)))
  }
  if(type == 'person') {


    tab <- x$fit$parameters$persons
  }
  if(type == 'item') {


    tab <- x$fit$parameters$coefficients
  }

  return(tab)

}

#' Print method for wizirt item-fit objects
#'
#' @param x An object exported from irt_item_fit()
#' @method print wizirt_ifa
#' @export
print.wizirt_ifa <- function(x){
  x$item_stats
}

#' Print method for wizirt item-fit objects
#'
#' @param x An object exported from irt_person_fit()
#' @method print wizirt_pfa
#' @param patterns Logical. Should the response patterns be printed as well?
#' @export
print.wizirt_pfa <- function(x, patterns = FALSE){
  item_col = max(which(grepl("_cut", colnames(x$person_estimates)))) + 1
  if(patterns == TRUE){
    return(tidyr::unite(x$person_estimates, pattern, item_col:ncol(x$person_estimates), sep = '') )
  } else {
    return(x$person_estimates[1:(item_col-1)])
  }

}

#' Print method for wizirt assumption objects
#'
#' @param x An object exported from irt_assume()
#' @param type Character. One of 'all', 'ld', 'unid', 'rel', 'abs'. Default is 'all'.
#' @method print wizirt_assume
#' @export
print.wizirt_assume <- function(x, type = 'all'){
  if(type == 'all'){
    return(dplyr::bind_rows(x$unidim %>% dplyr::rename(pars = 'stat'),
                     x$rel_fit %>% dplyr::filter(!stat %in% c("N", "n_pars")) %>% dplyr::rename(pars = 'stat', value = 'values'),
                     tibble::tibble(pars = c('Num LD pairs < .05'),
                                    value = sum(assumptions$ld$pvals < .05))))
  }
  if(type == 'ld'){
    return(x$ld)
  }
  if(type == 'unid'){
    return(x$unidim)
  }
  if(type == 'rel'){
    return(x$rel_fit)
  }
  if(type == 'abs'){
    return(x$abs_fit)
  }
  rlang::abort(glue::glue('Uknown type argument: "{type}"'))

}
