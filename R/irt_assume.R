#' A function for checking the assumptions of IRT.
#' @param wizirt_fit An object from fit_wizirt
#' @param verbose Logical. Should the names of the other packages used in this function be printed? Default is FALSE.
#' #' @return A list with tibbles of data in them.
#' @export
irt_assume <- function (wizirt_fit, verbose = FALSE) {
  if (verbose) {
    message('Using pkgs:')
    message(glue::glue('  - sirt {packageVersion("sirt")} for DETECT, ASSI, RATIO and LD covariance'))
    message(glue::glue('  - ltm {packageVersion("ltm")} for LD'))
    message(glue::glue('  - mirt {packageVersion("mirt")} for relative fit'))
  }


  # Unidimensionality
  unid <- check_unidimensionality(wizirt_fit)
  unidim <- tibble::enframe(unid$stats, name = 'stat')
  data <- wizirt_fit$fit$data
  # Conditional Dependence
  ld <- ltm::rcor.test(data)
  local_dependence <- ld$cor.mat
  ld2 <- ld$p.values %>%
    as.data.frame() %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("V"), factor, levels = 1:length(wizirt_fit$fit$parameters$coefficients$item),
                                labels = wizirt_fit$fit$parameters$coefficients$item)) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("V"), as.character)) %>%
    dplyr::rename(item_1 = 'V1', item_2 = 'V2')
  local_dependence[lower.tri(local_dependence, diag = T)] = NA

  ld <- local_dependence %>%
    as.data.frame() %>%
    tibble::rownames_to_column("item_1") %>%
    tidyr::pivot_longer(cols = -item_1,
                        names_to = "item_2",
                        values_to = "LD") %>%
    dplyr::arrange(dplry::desc(LD)) %>% tidyr::drop_na()

  ld <- ld %>% dplyr::left_join(ld2, by = c("item_1", "item_2"))

  ld <- ld %>% dplyr::left_join(unid$ld_cov %>%
                                  dplyr::select(item1, item2, ccov),
                                by = c('item_1' = 'item1', 'item_2' = 'item2'))
  # Relative Fit

  rel_fit <- irt_fit_stats(wizirt_fit)

  out <- list(unidim = unidim, ld = ld, rel_fit = rel_fit, abs_fit = NULL) %>%
    `class<-`(c('wizirt_assume', class(.)))

  return(out)
}

## unidimensionality




check_unidimensionality <- function(wizirt_fit){
  invisible(capture.output(detect_stats <- sirt::conf.detect(as.matrix(wizirt_fit$fit$data),
                                     wizirt_fit$fit$parameters$persons$ability,
                                     itemcluster = rep(1, length(colnames(wizirt_fit$fit$data))), # item objectives
                                     progress = F)))
  detect <- detect_stats$detect.summary$unweighted[1:5] %>% round(2) %>%
    `names<-`(c("DETECT", "ASSI", "RATIO", 'MADCOV100', 'MCOV100'))
  out <- list(stats = detect, ld_cov = detect_stats$ccovtable$ccov.table)
  out
}


# Some code from mirt I modified.
# https://github.com/philchalmers/mirt/blob/e6bd918abee1efb2a3c259ed69888594806a3d6d/R/03-estimation.R

irt_fit_stats <- function(wizirt_fit){

  N <- nrow(wizirt_fit$fit$data)
  logLik <- wizirt_fit$fit$estimation$log_lik
  tmp <- ncol(wizirt_fit$fit$data)

  AIC <- (-2) * logLik + 2 * tmp

  AICc <- AIC + 2 * tmp * (tmp + 1) / (N - tmp - 1L)

  BIC <- (-2) * logLik + tmp*log(N)

  SABIC <- (-2) * logLik + tmp*log((N+2)/24)

  HQ <- (-2) * logLik + 2*tmp*log(log(N))

  return(tibble::tibble(tibble::rownames_to_column(data.frame(values = rbind(log_lik = logLik,
                                                                             N = N,
                                                                             n_pars = tmp,
                                                                             AIC = AIC,
                                                                             AICc = AICc,
                                                                             BIC = BIC,
                                                                             SABIC = SABIC,
                                                                             HQ = HQ)), "stat")))
  # p.G2 <- 1 - pchisq(G2,df) # Still working on these
  # RMSEA.G2 <- rmsea(X2=G2, df=df, N=N) # Still working on these
}



