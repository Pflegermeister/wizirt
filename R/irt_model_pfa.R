#' A function to explore reasons for person misfit.
#' @param wizirt_fit An object of class wizirt (i.e. from fit_wizirt)
#' @param pfa An object of class wizirt_pfa (i.e. from irt_person_fit)
#' @param bins Numeric. How many bins to break items into. Sorted by difficulty then broken into bins. If items cannot fit equally into bins, the bins of easier items will take extras.
#' @param predictors Either a matrix or dataframe with a row per person and a column per predictor.
#' @export
irt_model_pfa <- function(wizirt_fit, pfa = NULL, predictors = NULL, bins = 5){

  if(is.null(pfa)){
    pfa <- irt_person_fit(wizirt_fit)
  }
  # prepare mlm_data function here

  mlm_data <- compile_mlm_data(wizirt_fit = wizirt_fit, pfa = pfa, predictors = predictors, bins = bins)

    mod_list <- list()
    for (i in pfa$spec$stats){
      mod_list[[i]] <- eval(parse(text = glue::glue('blme::blmer(',
                                                    '{i} ~',
                                                    '(1|ids) + .,',
                                                    'data = mlm_data)')))


    out <- list(icc = sapply(mod_list, performance::icc), models = mod_list) %>% `class<-`(c('pfa_mlm', class(.)))
    }
    return(out)
}


pfa_fit_subset <- function(bin, data = mlm_data, stats = pfa$spec$stats){

  mlm_sub <- data %>%
    dplyr::filter(item_bin == bin) %>%
    tidyr::pivot_wider(id_cols = c(ids, ability), names_from = item, values_from = value) %>%
    dplyr::select(-ids)

  mlm_coefs <- data %>%
    dplyr::filter(item %in% colnames(mlm_sub)) %>%
    dplyr::distinct(item, .keep_all = T) %>%
    dplyr::select(difficulty, discrimination)

  stats_list <- list()
  for (i in stats) {
    fit <- eval(parse(text = glue::glue('PerFit::{i}(mlm_sub %>% dplyr::select(-ability),',
                                        'IP = cbind(mlm_coefs, guessing = 0),',
                                        'Ability = mlm_sub %>% dplyr::pull(ability)',
                                        ')')))
    stats_list[[i]] <- fit$PFscores$PFscores
  }

  tibble::as_tibble(stats_list) %>%
    dplyr::mutate(ids = unique(data$ids), ability = mlm_sub %>% dplyr::pull(ability), bin = bin)

}

get_bins <- function(wizirt_fit, bins){
  r <- wizirt_fit$fit$parameters$coefficients %>% nrow()
  remainder <- rep(1:0, times = c((r%%bins), bins-(r%%bins)))
  items <- wizirt_fit$fit$parameters$coefficients %>% dplyr::arrange(difficulty) %>% dplyr::pull(item)
  data.frame(item = items,
             item_bin = rep(1:bins, times = (rep(r%/%bins, times = bins) + remainder)))
}

# predictors A named list of vectors or a data frame of pred
compile_mlm_data <- function(wizirt_fit, pfa, predictors = NULL, bins = bins){
  out <- pfa$person_estimates %>%
    tidyr::pivot_longer(cols = colnames(wizirt_fit$fit$data), names_to = 'item') %>%
    dplyr::left_join(wizirt_fit$fit$parameters$coefficients, by = 'item')

  out <- out %>% dplyr::left_join(get_bins(wizirt_fit = wizirt_fit, bins = bins), by = 'item')

  mlm_data <- lapply(1:bins, pfa_fit_subset, data = out, stats = pfa$spec$stats)

  mlm_data <- dplyr::bind_rows(mlm_data) %>% dplyr::arrange(ids, ability, bin)

  if(!is.null(predictors)){
    preds <- tibble::tibble(dplyr::bind_cols(ids = wizirt_fit$fit$parameters$persons$ids, predictors))
    mlm_data <- dplyr::left_join(mlm_data, preds, by = 'ids')
  }

  return(mlm_data)
}

