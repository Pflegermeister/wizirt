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

  df <- y$fit$estimation$df - x$fit$estimation$df

  if(df < 0){
    temp <- x
    x <- y
    y <- temp
  } else if(df == 0){
    if((2*y$fit$estimation$log_lik - 2*x$fit$estimation$log_lik) < 0){
      temp <- x
      x <- y
      y <- temp
    }
  }
  df <- y$fit$estimation$df - x$fit$estimation$df

  stats1 <- irt_fit_stats(x)
  stats1 <- stats1 %>% tidyr::pivot_wider(names_from = stat, values_from = values)
  stats2 <- irt_fit_stats(y)
  stats2 <- stats2 %>% tidyr::pivot_wider(names_from = stat, values_from = values)

  X2 <- 2*stats1$log_lik[1] - 2*stats2$log_lik[1]

    ret <- data.frame(AIC = c(stats1$AIC[1], stats2$AIC[1]),
                      AICc = c(stats1$AICc[1], stats2$AICc[1]),
                      SABIC = c(stats1$SABIC[1], stats2$SABIC[1]),
                      HQ = c(stats1$HQ[1], stats2$HQ[1]),
                      BIC = c(stats1$BIC[1], stats2$BIC[1]),
                      logLik = c(stats1$log_lik[1], stats2$log_lik[1]),
                      X2 = c(NaN, X2),
                      df = c(NaN, abs(df)),
                      p = c(NaN, 1 - pchisq(X2,abs(df))),
                      call = c(x$fit$model$engine$call, y$fit$model$engine$call))


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
  df
}

