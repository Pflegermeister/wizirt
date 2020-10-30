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
  tab <- NULL
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
             x$fit$model$engine$call,
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
  if (is.null(tab)) rlang::abort(glue::glue('Print method "{type}" is not available.'))
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
#' @param item_order A vector of item names or item positions specifying the order they should be printed in for the patterns. If NULL, items printed in the order they appear in the data. Can also be 'diff' to print patterns sorted by CTT difficulty.
#' @export
print.wizirt_pfa <- function(x, patterns = FALSE, item_order = NULL){

  items <- x$person_estimates %>%
    dplyr::select(-dplyr::contains(c("ability", "std_err", "ids", x$spec$stats))) %>%
    colnames()

  if(patterns == TRUE){
    if(is.null(item_order)){
      return(tidyr::unite(x$person_estimates, pattern, items, sep = '') )
    }
    if (all(item_order == 'by_diff')) {
      item_order <- x$person_estimates %>%
        dplyr::select(items) %>%
        colMeans(na.rm = T) %>%
        sort(decreasing = T) %>% names()
    } else {
      item_order <- x$person_estimates %>%
        dplyr::select(items) %>%
        dplyr::select(tidyselect::all_of(item_order)) %>% names()
    }


    return(tidyr::unite(x$person_estimates %>%
                   dplyr::select(-items,
                                 tidyselect::all_of(items)),
                 pattern, items, sep = ''))

  } else {
    return(x$person_estimates %>%
             dplyr::select(-items))
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
