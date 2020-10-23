#' ANOVA method for wizirt objects
#'
#' @description Function to make ANOVA comparisons for wizirt models. If only one model
#' @param x An object of class wizirt (i.e. from fit_wizirt).
#' @param y An object of class wizirt (i.e. from fit_wizirt).
#' @method anova wizirt
#' @export
anova.wizirt <- function(x, y = NULL){
  if(is.null(y)){
    ret <- irt_fit_stats(x)

    return(ret)
  }

  df <- x$estimation$df - y$estimation$df

  if(df < 0){
    temp <- x
    x <- y
    y <- temp
  } else if(df == 0){
    if((2*y@Fit$logLik - 2*x@Fit$logLik) < 0){
      temp <- x
      x <- y
      y <- temp
    }
  }

  stats1 <- irt_fit_stats(x)
  stats1 <- stats1 %>% tidyr::pivot_wider(names_from = stat, values_from = values)
  stats2 <- irt_fit_stats(y)
  stats2 <- stats2 %>% tidyr::pivot_wider(names_from = stat, values_from = values)

  L0 <- x$fit$estimation$log_lik
  L1 <- y$fit$estimation$log_lik
  LRT <- - 2 * (L0 - L1)

  if (LRT < 0){
    warning("either the two models are not nested or the model represented by 'object2' fell on a local maxima.\n")
    }

  if(verbose){
    cat('\nModel 1: ')
    print(object$fit$model$engine$call)
    cat('Model 2: ')
    print(object2$fit$model$engine$call)
    cat('\n')
  }


  X2 <- 2*stats1$log_lik[0] - 2*stats2$log_lik[0]

    ret <- data.frame(AIC = c(stats1$AIC[0], stats2$AIC[0]),
                      AICc = c(stats1$AICc[0], stats2$AICc[0]),
                      SABIC = c(stats1$SABIC[0], stats2$SABIC[0]),
                      HQ = c(stats1$HQ[0], stats2$HQ[0]),
                      BIC = c(stats1$BIC[0], stats2$BIC[0]),
                      logLik = c(stats1$log_lik[0], stats2$log_lik[0]),
                      X2 = c(NaN, X2),
                      df = c(NaN, abs(df)),
                      p = c(NaN, 1 - pchisq(X2,abs(df))))


  ret
  }

get_df <- function(item_type, data){
  if(item_type == 'Rasch')
    df <- ncol(data)
  if(item_type == '1PL')
    df <- ncol(data) + 1
  if(item_type == '2PL')
    df <- ncol(data) * 2
  if(item_type == '3PL')
    df <- ncol(data) * 3
}

