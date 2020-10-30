#' A function used to get additional person-level information, such as person-level fit measures and person response functions. I accidentally output plots in addition to data in one part. That will be moved to a plot method later.
#'
#' If there is missing data present, non-parametric imputation  will be done to get the cutoff for the measures.
#' @param wizirt_fit An object coming from the fit_wizirt function.
#' @param stats A character or character string identifying person-level fit measures. Default is "Ht", but accepts all of the person-fit measures from the PerFit package.
#' @param items Which items toe plot? Either a numeric vector of item positions in the column names of the data, or a vector of the item names to include. If nothing is specified all items are included.
#' @param level Numeric. Significance level for bootstrap distribution (value between 0 and 1). Default is 0.05.
#' @param na.rm Logical. If FALSE, NAs are still removed, there is just a warning printed.
#' @return A list with person-level statistics, person-response functions, data for person-response functions, and an empty slot for multi-level information that will be coming soon.
#' @examples
#' pfa <- wizirt2:::irt_person_fit(my_model)
#' @export
irt_person_fit <- function(wizirt_fit,
                           stats = c("Ht"),
                           items = NULL,
                           level= .05,
                           na.rm = FALSE
){
  out <- list(
    person_estimates = NULL,
    prf = NULL,
    spec = list(
      stats = NULL
    )#,
    #rownames = wizirt_fit$fit
  )
  if (is.null(items)){
    #message('all items')
    items = colnames(wizirt_fit$fit$data)[1:ncol(wizirt_fit$fit$data)]
  }
  if(is.numeric(items)){
    items = colnames(wizirt_fit$fit$data)[items]
  }

  df <- wizirt_fit$fit$data %>%
    dplyr::select(which(items %in% items))

  sirt_stats <- stats[which(!stats %in% c('lz', 'lzstar', 'NCI', 'E.KB',
                                          'D.KB', 'A.KB', 'Ht', 'ZU3', 'U3',
                                          'Cstar', 'C.Sato','G'))]
  if("infit" %in% sirt_stats & !("outfit" %in% sirt_stats)){
    sirt_stats <- c(sirt_stats[sirt_stats != "infit"], "infit", "outfit")
  }

  stats <- stats[which(stats %in% c('lz', 'lzstar', 'NCI', 'E.KB',
                                    'D.KB', 'A.KB', 'Ht', 'ZU3', 'U3',
                                    'Cstar', 'C.Sato','G'))]
  if(identical(stats, character(0))){
    stats <- "Ht"
    drop_ht <- T
  } else{
    drop_ht <- F
  }

  out$spec$stats <- c(stats, sirt_stats)

  # person_estimates...
  stats_list <- list()
  flagged <- list()
  for (i in stats){
    fit <- eval(parse(text = glue::glue('PerFit::{i}(df,',
                                        'IP = cbind(wizirt_fit$fit$parameters$coefficients[,3:2], guessing = 0),',
                                        'Ability = wizirt_fit$fit$parameters$persons$ability',
                                        ')')))
    if (na.rm == FALSE & mean(is.na(wizirt_fit$fit$data)) > 0 ) {
      rlang::warn("NAs omitted while estimating cut offs for person-fit statistics.")
    }
    free_fit <- eval(parse(text = glue::glue('PerFit::{i}(df,',
                                             'IP = cbind(wizirt_fit$fit$parameters$coefficients[,2:3], guessing = 0),',
                                             'Ability = wizirt_fit$fit$parameters$persons$ability,',
                                             'NA.method = "NPModel")')))

    stats_list[[i]] <- fit$PFscores$PFscores
    stats_list[[glue::glue('{i}_cut')]] <- PerFit::cutoff(free_fit, Blvl = level)$Cutoff

    flagged[[i]] <- sapply(fit$PFscores$PFscores, function(x) ifelse(i %in% c('Ht', 'A.Kb', 'E.Kb', 'lz','lzstar', 'NCI', 'r.pbis'),
                                                                     x < stats_list[[glue::glue('{i}_cut')]],
                                                                     x > stats_list[[glue::glue('{i}_cut')]]))

  }

  if(!identical(sirt_stats, character(0))){
    if (wizirt_fit$fit$model$item_type != "Rasch"){
      rlang::warn(glue::glue("wizirt cannot get person statistic(s) {paste0(sirt_stats, collapse = ', ')} for non-Rasch models."))
    } else {
      out$person_estimates = tibble::tibble(data.frame(wizirt_fit$fit$parameters$persons,
                                                       tibble::as_tibble(stats_list),
                                                       personfit.stat(data,
                                                                      print(my_model, type = "person")$ability,
                                                                      print(my_model, type = "item")$difficulty) %>%
                                                         dplyr::select(tidyselect::all_of(sirt_stats)),
                                                       df))
      if(drop_ht){
        out$person_estimates <- out$person_estimates %>% dplyr::select(-contains("Ht"))
      }
    }
  } else {
    out$person_estimates = tibble::tibble(data.frame(wizirt_fit$fit$parameters$persons,
                                                     tibble::as_tibble(stats_list),
                                                     df))
  }

  flagged = (dplyr::bind_rows(flagged) %>% dplyr::rowwise() %>% rowSums()) > 0


  if(drop_ht){
    flagged = rep(FALSE, length(flagged))
  }

  out$prf <- gg_prf(df,
                    flagged = flagged,
                    examinees = wizirt_fit$fit$parameters$persons$ids,
                    h = 0.09,
                    N.FPts = 30,
                    alpha = 0.05,
                    NA.method = "Pairwise",
                    IP = cbind(wizirt_fit$fit$parameters$coefficients[,2:3], guessing = 0),
                    IRT.PModel = rlang::as_name(wizirt_fit$fit$model$item_type),
                    Ability = wizirt_fit$fit$parameters$persons$ability,
                    Ability.PModel = "ML",
                    mu = 0,
                    sigma = 1)

  class(out) <- c("wizirt_pfa", class(out))
  out

}

# the gg_prf function will need to be adapted to make parametric prfs.
gg_prf <- function (matrix, flagged, examinees, h = 0.09, N.FPts = 15, alpha = 0.05,
                    NA.method = "Pairwise", IP = NULL, IRT.PModel = "2PL",
                    Ability = NULL, Ability.PModel = "ML", mu = 0, sigma = 1)
{
  if (IRT.PModel == "Rasch") {
    IRT.PModel <- "1PL"
  }

  matrix <- as.matrix(matrix)
  N <- dim(matrix)[1]
  I <- dim(matrix)[2]
  PerFit:::Sanity.dma(matrix, N, I)
  res.NA <- PerFit:::MissingValues(matrix, NA.method, Save.MatImp = F, IP,
                                   IRT.PModel, Ability, Ability.PModel, mu, sigma)

  matrix <- res.NA[[1]]
  res1 <- PerFit:::PRF(matrix, h, N.FPts)
  res2 <- PerFit:::PRF.VarBands(matrix, h, N.FPts, alpha)
  basis.bspline <- fda::create.bspline.basis(rangeval = c(0, 1),
                                             norder = 4, nbasis = (4 + 9))

  basis.values <- fda::eval.basis(evalarg = seq(0, 1, length.out = N.FPts),
                                  basisobj = basis.bspline)
  PRF.VarBandsLow <- basis.values %*% res2$FDO.VarBandsLow$coefs
  PRF.VarBandsHigh <- basis.values %*% res2$FDO.VarBandsHigh$coefs



  plot_dat <- res1$PRFest %>%
    t() %>%
    `colnames<-`(paste0("x",1:ncol(.))) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(ids = examinees, Aberrant = flagged) %>% # working to add color for aberrant
    tidyr::pivot_longer(cols = c(-ids, -Aberrant), names_to = "xlab", values_to = "y") %>%
    dplyr::mutate(x = rep(seq(0, 1, length.out = N.FPts),
                          times = ncol(res1$PRFest))) %>%
    dplyr::left_join(PRF.VarBandsLow %>%
                       t() %>%
                       `colnames<-`(paste0("x",1:ncol(.))) %>%
                       tibble::as_tibble() %>%
                       dplyr::mutate(ids = examinees) %>%
                       tidyr::pivot_longer(cols = -ids,
                                           names_to = "xlab",
                                           values_to = "ymin") %>%
                       dplyr::mutate(x = rep(seq(0, 1, length.out = N.FPts),
                                             times = ncol(res1$PRFest))),
                     by = c("ids", "xlab", "x")) %>%
    dplyr::left_join(PRF.VarBandsHigh %>%
                       t() %>%
                       `colnames<-`(paste0("x",1:ncol(.))) %>%
                       tibble::as_tibble() %>%
                       dplyr::mutate(ids = examinees) %>%
                       tidyr::pivot_longer(cols = -ids,
                                           names_to = "xlab",
                                           values_to = "ymax") %>%
                       dplyr::mutate(x = rep(seq(0, 1, length.out = N.FPts),
                                             times = ncol(res1$PRFest))),
                     by = c("ids", "xlab", "x"))

  plot_dat
  # I will need to constrain it so that for large numbers of aberrant responders
  # the responses are on multiple pages


}

