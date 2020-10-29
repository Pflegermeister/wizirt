irt_ltm <- function(data, rownames = NULL, item_type = "Rasch", irt_pars = TRUE){
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

  if (item_type == "Rasch") {
    verb_text <- capture.output(ltm_model <- ltm::rasch(data, constraint = cbind(ncol(data) + 1, 1), IRT.param = irt_pars, control=list(verbose = T)))
    iter_min <- (verb_text[grepl("iter {1,}[[:digit:]]{1,}", verb_text)] %>%
                   sapply(stringr::str_extract, "iter {1,}[[:digit:]]{1,}") %>%
                   sapply(stringr::str_extract, "[[:digit:]]{1,}") %>%
                   as.numeric() %>%
                   max())
    iters <- glue::glue("< {iter_min+10}")
  } else if (item_type == "1PL") {
    verb_text <- capture.output(ltm_model <- ltm::rasch(data, IRT.param = irt_pars, control=list(verbose = T)))
    iter_min <- (verb_text[grepl("iter {1,}[[:digit:]]{1,}", verb_text)] %>%
                   sapply(stringr::str_extract, "iter {1,}[[:digit:]]{1,}") %>%
                   sapply(stringr::str_extract, "[[:digit:]]{1,}") %>%
                   as.numeric() %>%
                   max())
    iters <- glue::glue("< {iter_min+10}")
  } else if (item_type == "2PL") {
    verb_text <- capture.output(ltm_model <- ltm::ltm(data~z1, IRT.param = irt_pars, control=list(verbose = T)))
    iters <- verb_text[grepl("iteration: {1,}[[:digit:]]{1,}", verb_text)] %>%
      sapply(stringr::str_extract, "iteration: {1,}[[:digit:]]{1,}") %>%
      sapply(stringr::str_extract, "[[:digit:]]{1,}") %>%
      as.numeric() %>%
      max()
  } else if (item_type == "3PL") {
    verb_text <- capture.output(ltm_model <- ltm::tpm(data, IRT.param = irt_pars, control=list(verbose = T)))
    iter_min <- (verb_text[grepl("iter {1,}[[:digit:]]{1,}", verb_text)] %>%
                   sapply(stringr::str_extract, "iter {1,}[[:digit:]]{1,}") %>%
                   sapply(stringr::str_extract, "[[:digit:]]{1,}") %>%
                   as.numeric() %>%
                   max())
    iters <- glue::glue("< {iter_min+10}")
  } else {
    rlang::abort(glue::glue("item_type '{item_type}' is unknown."))
  }

  call <- paste0(trimws(capture.output(ltm_model$call)), collapse = " ") %>%
    stringr::str_replace(", control = list\\(verbose = T\\)", "")

  out$data = tibble::as_tibble(ltm_model$X)

  out$model$engine$pkg = 'ltm'
  out$model$engine$ver = packageVersion('ltm')
  out$model$engine$func = 'rasch'
  out$model$engine$call = call

  out$model$n_factors = 1
  out$model$item_type = item_type

  out$estimation$convergence = ltm_model$convergence == 0
  out$estimation$method = ltm_model$control$method # This is the method for optimization, not estimation. It seems optimization is Maximum likelihood.
  out$estimation$log_lik = ltm_model$log.Lik
  out$estimation$iterations = iters
  out$estimation$criteria = NA # xxxx

  out$original_object = ltm_model

  out$parameters$coefficients = coefs_ltm(ltm_model, item_type)

  out$parameters$persons = `colnames<-`(dplyr::left_join(tidyr::unite(data, "response_pattern"),
                                                         tidyr::unite(factor.scores(ltm_model)$score.dat,
                                                                      "response_pattern" ,
                                                                      1:(ncol(factor.scores(ltm_model)$score.dat)-4)),
                                                         by = "response_pattern")[,-1:-3], c("ability", "std_err")) %>%
    dplyr::mutate(ids = rownames)
  out
}

coefs_ltm <- function(ltm_model, item_type){
  if (item_type == '3PL') {
    coefs <- coef(ltm_model) %>%
      as.data.frame() %>%
      tibble::rownames_to_column("item") %>%
      dplyr::select(1, difficulty = 3, discrimination = 4, guessing = 2) %>%
      tibble::as_tibble()
  } else {
    coefs <- coef(ltm_model) %>%
      as.data.frame() %>%
      tibble::rownames_to_column("item") %>%
      dplyr::select(1, difficulty = 2, discrimination = 3) %>%
      dplyr::mutate(guessing = 0) %>%
      tibble::as_tibble()
  }
  coefs
}


