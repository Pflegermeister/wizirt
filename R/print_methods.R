print.irt <- function(x, type = 'tech'){

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

  } else  if(type == 'desc'){
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

  } else {
    rlang::abort(glue::glue('Print method "{type}" is not available.'))
  }
  show(tab)
  return(tab)

}
