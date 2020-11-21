irt_erm <- function(data,
                    rownames = NULL,
                    item_type = "Rasch",
                    irt_pars = TRUE,
                    tol = 1e-5) {
  if (is.null(rownames)) rownames <- seq_len(nrow(data))
  out <- list(
    data = NULL,
    model = list(
      engine = list(
        pkg = NULL,
        ver = NULL,
        func = NULL,
        call = NULL
      ),
      n_factors = NULL,
      item_type = NULL
    ),
    estimation = list(
      convergence = NULL,
      method = NULL,
      criteria = NULL,
      iterations = NULL,
      log_lik = NULL,
      abs_fit = NULL,
      df = get_df(item_type, data)
    ),
    parameters = list(
      coefficients = NULL,
      persons = NULL
    ),
    original_object = NULL
  )

  # Running erm
  if (item_type != "Rasch") {
    rlang::abort("The eRm engine is only available for Rasch models.")
  }
  erm_model <- eRm::RM(data)
  ppar <- eRm::person.parameter(erm_model)

  # Standardizing erm

  out$data <- tibble::as_tibble(erm_model$X)

  out$model$engine$pkg <- "eRm"
  out$model$engine$ver <- packageVersion("eRm")
  out$model$engine$func <- "RM"
  out$model$engine$call <- capture.output(erm_model$call)

  out$model$n_factors <- 1
  out$model$item_type <- item_type

  out$estimation$convergence <- erm_model$convergence <= 2
  out$estimation$method <- "CML"
  out$estimation$log_lik <- erm_model$loglik
  out$estimation$iterations <- erm_model$iter
  out$estimation$criteria <- NA

  out$estimation$abs_fit <- NA

  out$original_object <- erm_model

  out$parameters$coefficients <- tibble::tibble(item = names(data), difficulty = -unname(erm_model$betapar), discrimination = 1, guessing = 0)

  out$parameters$persons <- tibble::tibble(ability = unname(ppar$thetapar$NAgroup1), std_err = unname(ppar$se.theta$NAgroup1)) %>% dplyr::mutate(ids = rownames)

  out
}
