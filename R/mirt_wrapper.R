irt_mirt <- function(data,
                     rownames = NULL,
                     item_type = "Rasch",
                     irt_pars = TRUE,
                     tol = 1e-5){
  if (is.null(rownames)) rownames = seq_len(nrow(data))
  out <- list(data = NULL,
              model = list(engine = list(pkg = NULL,
                                         ver = NULL,
                                         func = NULL,
                                         call = NULL),
                           n_factors = NULL,
                           item_type = NULL),
              estimation = list(convergence = NULL,
                                method = NULL,
                                criteria = NULL,
                                iterations = NULL,
                                log_lik = NULL,
                                df = get_df(item_type, data)),
              parameters = list(
                coefficients = NULL,
                persons = NULL
              ),
              original_object = NULL)
  # Running mirt
  if ( item_type == '1PL'){
    model <- mirt::mirt.model(glue::glue('F1 = 1-{ncol(data)} \n',
                                'CONSTRAIN =(1-{ncol(data)},a1)'))
    mirt_model <- mirt::mirt(data, model = model, itemtype = '2PL',
                             verbose = F, TOL = tol, SE = T)
  } else if (item_type %in% c('Rasch', '2PL', '3PL')){
    mirt_model <- mirt::mirt(data, model = 1, itemtype = item_type,
                             verbose = F, TOL = tol, SE = T)
  }

  # Standardizing mirt

  out$data = tibble::as_tibble(mirt_model@Data$data)

  out$model$engine$pkg = 'mirt'
  out$model$engine$ver = packageVersion('mirt')
  out$model$engine$func = 'mirt'
  out$model$engine$call = glue::glue(gsub('tol', '{tol}',
                                          gsub('item_type', '"{item_type}"',
                                               paste0(trimws(capture.output(mirt_model@Call)),
                                                      collapse = " "))))
  out$model$engine$call = paste(trimws(capture.output(out$model$engine$call$fit$model$engine$call)), collapse = "")
  out$model$n_factors = 1
  out$model$item_type = item_type

  out$estimation$convergence = mirt_model@OptimInfo$converged
  out$estimation$method = mirt_model@Options$method
  out$estimation$log_lik = mirt_model@Fit$logLik
  out$estimation$iterations = mirt::extract.mirt(mirt_model, 'iterations')
  out$estimation$criteria = mirt_model@Options$TOL
  out$original_object = mirt_model


  out$parameters$coefficients = tibble::tibble(tibble::rownames_to_column(`colnames<-`(data.frame(mirt::coef(mirt_model, simplify = T, IRTpars = irt_pars)$items)[,c(2,1,3)], c("difficulty", "discrimination", 'guessing')), "item"))


  out$parameters$persons = dplyr::rename(tibble::as_tibble(mirt::fscores(mirt_model, full.scores.SE = T)), ability = "F1", std_err = "SE_F1" ) %>% dplyr::mutate(ids = rownames)

  out

}
