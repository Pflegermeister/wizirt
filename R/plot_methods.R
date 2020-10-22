#' Plot method for wizirt objects
#'
#' @description All plots in wizirt use ggplot2 as a backend. Type 'theta_diff' also uses gghalves. For the item-focused plots, the types can be included in the same string to overlay the plots (e.g. resid_trace).
#' For large numbers of persons and/or items, you may want to consider limiting the number printed at a time on some plots.
#' @param type Character string. Currently, can be 'trace' for item characteristic curves or 'info' for item information.
#' 'obs', 'trace', 'info', 'resid', 'stand', 'tinfo', 'theta', 'diff', 'theta_diff', and 'np_prf'
#' @param items Which items to plot? Either a numeric vector of item positions in the column names of the data, or a vector of the item names to plot. If nothing is specifed all items will be plotted.
#' @param persons Which persons to plot? Either a numeric vector of person positions in the row names of the data, or a vector of the person ids to plot. If nothing is specifed all persons will be plotted.
#' @param facets Logical. Should the plots be faceted? Default is TRUE.
#' @param quads Numeric. For plots using residuals (i.e. resid, stand). How many quantiles should the data be broken into?
#' @param return_data Logical. Should the plot data be returned. If TRUE returns a list with the plot and the data.
#' @param pfa An object from irt_person_fit(). If omitted, irt_person_fit() is called within the function with the default settings.
#' @param  <-  An object from irt_item_fit(). If omitted, irt_item_fit() is called within the function with the default settings.
#' @method plot wizirt
#' @export
plot.wizirt <- function(wizirt_fit,
                     type = 'tinfo',
                     items = NULL,
                     persons = NULL,
                     facets = TRUE,
                     quads = 10,
                     return_data = FALSE,
                     pfa = NULL,
                     ifa = NULL){

  plt_data <- list()
  plt <- NULL

  if (is.null(persons)){
    #message('all items')
    persons = wizirt_fit$fit$parameters$persons$ids
  }
  if(all(persons %in% seq_len(nrow(wizirt_fit$fit$data)))){
    persons = wizirt_fit$fit$parameters$persons$ids[persons]
  }

  if (is.null(items)){
    #message('all items')
    items = colnames(wizirt_fit$fit$data)[seq_len(ncol(wizirt_fit$fit$data))]
  }
  if(is.numeric(items)){
    items = colnames(wizirt_fit$fit$data)[items]
  }

  if (grepl('obs', type)){
    plt <- 'obs'
    if(is.null(pfa)) {
      pfa <- irt_person_fit(wizirt_fit, stat = 'Ht', items = items)
    }

    df <- pfa$person_estimates %>%
      dplyr::select(ids, ability, tidyselect::all_of(items)) %>%
      tidyr::pivot_longer(cols = c(-ids, -ability), names_to = 'item') %>%
      dplyr::filter(item %in% items, ids %in% persons)

    plt_data[['obs']] <- df

    if (facets == F) {

      p <- df %>%
        ggplot2::ggplot(ggplot2::aes(x = ability, y = value, color = item)) +
        ggplot2::geom_jitter(alpha = .5, cex = 2, width = 0, height = .05)

    } else{
      p <- df %>%
        ggplot2::ggplot(ggplot2::aes(x = ability, y = value)) +
        ggplot2::geom_jitter(alpha = .5, cex = 2, width = 0, height = .05) +
        ggplot2::facet_wrap(~item)
    }

    p <- p +
      ggplot2::labs(title = 'Examinee Responses by Ability')
  }

  if (grepl('trace', type)) {
    df <- irf_probs(wizirt_fit) %>%
      dplyr::filter(item %in% items)

    plt_data[['trace']] <- df

    if (is.null(plt)){
      plt <- 'trace'
      if (facets == F) {

        p <- df %>%
          ggplot2::ggplot(ggplot2::aes(x = x,
                                       y = y,
                                       color = item)) +
          ggplot2::geom_line() +
          ggplot2::theme(legend.position = 'bottom')

      } else{

        p <- df %>%
          ggplot2::ggplot(ggplot2::aes(x = x,
                                       y = y)) +
          ggplot2::geom_line(color = '#130d42') +
          ggplot2::theme(legend.position = 'bottom') +
          ggplot2::facet_wrap(~item)
      }
      p <- p +
        ggplot2::labs(title = 'Item characteristic curves', y = 'P(X ==1)')

    } else { # if plt is not null

      plt <- paste(plt, 'trace', sep = '_')

      p <- p + ggplot2::geom_line(ggplot2::aes(x = x,
                                               y = y, group = item),
                                  data = df)

    }
  }

  if (grepl('resid', type)) {

    breaks <- quantile(wizirt_fit$fit$parameters$persons$ability,
                       seq(0,1,length.out = quads + 1))
    breaks[1] <- -Inf
    df <- cbind(Ability = wizirt_fit$fit$parameters$persons$ability,
                breaks = cut(wizirt_fit$fit$parameters$persons$ability,
                             breaks,
                             labels = 1:quads),
                wizirt_fit$fit$data) %>%
      as.data.frame() %>%
      dplyr::group_by(breaks) %>%
      dplyr::summarise(dplyr::across(c(Ability,
                                       colnames(wizirt_fit$fit$data)),
                                     mean,
                                     .names = "{.col}"), .groups = 'drop_last') %>%
      tidyr::pivot_longer(cols = -breaks:-Ability,
                          names_to = 'item',
                          values_to = 'Probability') %>%
                                     dplyr::filter(item %in% items)

    plt_data[['resid']] <- df

    if (is.null(plt)){
      plt <- 'resid'
      if (facets == F) {

        p <- df %>%
          ggplot2::ggplot(ggplot2::aes(x = Ability, y = Probability)) +
          ggplot2::geom_point()

      } else {
        p <- df %>%
          ggplot2::ggplot(ggplot2::aes(x = Ability, y = Probability)) +
          ggplot2::geom_point() +
          ggplot2::facet_wrap(~item)
      }
      p <- p +
        ggplot2::labs(title = 'Item Residual Plots', y = 'Residuals')
    } else { # plt != NULL
      plt <- paste(plt, 'resid', sep = '_')
      p <- p + ggplot2::geom_point(ggplot2::aes(x = Ability,
                                                y = Probability,
                                                group = item),
                                   data = df
      )

    }


  }

  if (grepl('stand', type)) {
    breaks <- quantile(wizirt_fit$fit$parameters$persons$ability,
                       seq(0,1,length.out = quads + 1))
    breaks[1] <- -Inf
    df <- cbind(Ability = wizirt_fit$fit$parameters$persons$ability,
                breaks = cut(wizirt_fit$fit$parameters$persons$ability,
                             breaks,
                             labels = 1:quads),
                wizirt_fit$fit$data) %>%
      as.data.frame() %>%
      dplyr::group_by(breaks) %>%
      dplyr::summarise(dplyr::across(c(Ability,
                                       colnames(wizirt_fit$fit$data)),
                                     mean,
                                     .names = "{.col}"))
    df <- df %>%
      tidyr::pivot_longer(cols = -breaks:-Ability,
                          names_to = 'item',
                          values_to = 'Probability') %>%
      dplyr::mutate(Ability = round(Ability, 10)) %>%
      dplyr::left_join(irf_probs(wizirt_fit, theta = df$Ability) %>%
                         dplyr::rename(Ability = 'x') %>%
                         dplyr::mutate(Ability = round(Ability, 10))) %>%
      dplyr::mutate(stn_res = (Ability - y)/sd(Ability - y)) %>%
                                     dplyr::filter(item %in% items)


    plt_data[['stand']] <- df

    if (is.null(plt)){
      plt <- 'stand'
      if (facets == F) {

        p <- df  %>%
          dplyr::filter(item %in% items) %>%
          ggplot2::ggplot(ggplot2::aes(x = Ability,
                                       y = stn_res,
                                       color = item)) +
          ggplot2::geom_point() +
          ggplot2::geom_hline(yintercept = 0)

      } else {
        p <- df %>%
          dplyr::filter(item %in% items) %>%
          ggplot2::ggplot(ggplot2::aes(x = Ability, y = stn_res)) +
          ggplot2::geom_point() +
          ggplot2::geom_hline(yintercept = 0) +
          ggplot2::facet_wrap(~item)
      }
      p <- p +
        ggplot2::labs(title = 'Standardized Residual Plot', y = 'Standardized Residuals')
    } else { # plt != NULL
      plt <- paste(plt, 'stand', sep = '_')
      p <- p + ggplot2::geom_point(ggplot2::aes(x = Ability,
                                                y = stn_res,
                                                group = item),
                                   data = df
      )

    }

  }

  if (grepl('info', type) & !grepl('tinfo', type) ) {
    if(is.null(ifa)) {
      ifa <- irt_item_fit(wizirt_fit)
    }
    df <- ifa$item_information

    plt_data[['info']] <- df %>%
                                    dplyr::filter(item %in% items)

    if(is.null(plt)){
      if (facets == FALSE){
        p <- df %>%
          ggplot2::ggplot(ggplot2::aes(x = theta,
                                       y = info,
                                       color = item)) +
          ggplot2::geom_line() +
          ggplot2::theme(legend.position = 'bottom') +
          ggplot2::labs(title = 'Item information functions')

      } else {
        p <- df %>%
          ggplot2::ggplot(ggplot2::aes(x = theta,
                                       y = info)) +
          ggplot2::geom_line() +
          ggplot2::geom_line(color = '#130d42') +
          ggplot2::facet_wrap(~item) +
          ggplot2::labs(title = 'Item information functions')

      }
    } else { # plt != NULL
      plt <- paste(plt, 'info', sep = '_')
      p <- p + ggplot2::geom_line(ggplot2::aes(x = theta,
                                               y = info,
                                               group = item),
                                  data = df)

    }


  }

  # Test Generally Plots

  if (type == 'tinfo') {
    if(is.null(ifa)) {
      ifa <- irt_item_fit(wizirt_fit)
    }
    df <- ifa$item_information %>%
      dplyr::filter(item %in% items) %>%
      dplyr::group_by(theta) %>%
      dplyr::summarize(info = sum(info), .groups = 'drop_last')
    plt_data[['tinfo']] <- df

      plt <- 'tinfo'
      p <- df %>%
        ggplot2::ggplot(ggplot2::aes(x = theta, ymax = info)) +
        ggplot2::geom_ribbon(ymin = 0, fill = "#094bab", alpha = .3, color = '#130d42') +
        ggplot2::theme_classic() +
        ggplot2::labs(title = 'Test Information Function')
  }

  if (type == 'theta'| grepl('theta', type) & grepl('SE', type)) {
    df <- wizirt_fit$fit$parameters$persons  %>%
      dplyr::filter(ids %in% persons)
    plt_data[['theta']] <- df

    if (is.null(plt)) {
      plt <- 'theta'

      p <- df %>%
        ggplot2::ggplot(ggplot2::aes(x = ability)) +
        ggplot2::geom_density(fill = "#094bab",
                              alpha = .3,
                              color = '#130d42') +
        ggplot2::labs(title = 'Distribution of Person Abilities')
    } else {
      plt <- paste(plt, 'theta', sep = '_')
      p <- p + ggplot2::geom_density(fill = "#094bab",
                                     alpha = .3,
                                     color = '#130d42', data = df)
    }
  }

  if (type == 'SE'| grepl('theta', type) & grepl('SE', type)) {
    df <- wizirt_fit$fit$parameters$persons %>%
      dplyr::filter(ids %in% persons)
    plt_data[['SE']] <- df

    if (is.null(plt)) {
      plt <- 'SE'

      p <- df %>%
        ggplot2::ggplot(ggplot2::aes(x = ability, y = std_err)) +
        ggplot2::geom_line() +
        ggplot2::labs(title = 'Standard Error of Measured Abilities')
    } else {
      plt <- paste(plt, 'SE', sep = '_')
      p <- p + ggplot2::geom_line(ggplot2::aes(x = ability, y = std_err), lty = 2, data = df) +
        ggplot2::labs(subtitle = 'Black dotted line is SE')
    }
  }

  if (type == 'diff') {
    df <- wizirt_fit$fit$parameters$coefficients[,1:2]
    plt_data[['diff']] <- df %>%

    if (is.null(plt)) {
      plt <- 'diff'

      p <- df %>%
        ggplot2::ggplot(ggplot2::aes(x = difficulty)) +
        ggplot2::geom_density(fill = "#094bab",
                              alpha = .3,
                              color = '#130d42') +
        ggplot2::labs(title = 'Distribution of Item Locations')
    } else {
      plt <- paste(plt, 'theta', sep = '_')
      p <- p + ggplot2::geom_density(fill = "#094bab",
                                     alpha = .3,
                                     color = '#130d42', data = df)
    }
  }

  if (grepl('theta', type)&grepl('diff', type)){

    plt_data[['theta']] <- wizirt_fit$fit$parameters$persons %>%
      dplyr::filter(ids %in% persons)
    plt_data[['diff']] <- wizirt_fit$fit$parameters$coefficients %>%
      dplyr::filter(item %in% items)

    p <- ggplot2::ggplot(data = plt_data[['theta']])+
      ggplot2::annotate('rect', xmin = -3, xmax = 3,
                        ymin = min(c(plt_data[['diff']]$difficulty,
                                     plt_data[['theta']]$ability) ) - 2,
                        ymax = min(plt_data[['theta']]$ability), fill = 'firebrick', alpha = .1) +
      ggplot2::annotate('rect', xmin = -3, xmax = 3,
                        ymax = max(c(plt_data[['diff']]$difficulty,
                                     plt_data[['theta']]$ability) ) + 2,
                        ymin = max(plt_data[['theta']]$ability), fill = 'firebrick', alpha = .1)+
      gghalves::geom_half_violin(ggplot2::aes(y = ability, fill = 'a'), side = 'l') +
      gghalves::geom_half_violin(mapping = ggplot2::aes(y = difficulty, fill = 'b'),
                                 data = plt_data[['diff']], side = 'r') +
      ggplot2::coord_cartesian(ylim = c(min(plt_data[['diff']]$difficulty - .25),
                                        max(plt_data[['theta']]$ability + .25)), xlim = c(-.5, .5)) +
      ggplot2::theme_classic() +
      ggplot2::scale_fill_manual(name = '',
                                 values =c('a'='lightblue','b'='#161e70'),
                                 labels = c('Person Ability', 'Item Difficulty')) +
      ggplot2::labs(title = 'Distribution of Person Abilities and Item Locations',
                    x = '', y = 'Theta') +
      ggplot2::theme(axis.title.x=ggplot2::element_blank(),
                     axis.text.x=ggplot2::element_blank(),
                     axis.ticks.x=ggplot2::element_blank())


  }

  # Person Plots

  if(grepl('np_prf', type)){
    if(is.null(pfa)) {
      pfa <- irt_person_fit(wizirt_fit, stat = 'Ht', items = items)
    }
    df <- pfa$prf %>%
      dplyr::filter(ids %in% persons)
    plt_data[['np_prf']] <- df
    if (facets == FALSE){
      p <- df %>%
        ggplot2::ggplot(ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_line(ggplot2::aes(group = factor(ids), color = Aberrant)) +
        ggplot2::ylim(c(0,1)) +
        ggplot2::scale_color_manual(values = c("black", "#cc0c00")) +
        ggplot2::labs(title = "Person Response Functions",
                      x = "Item Difficulty",
                      y = "P(x = 1)")
    } else {
      p <- df %>%
        ggplot2::ggplot(ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_line() +
        ggplot2::geom_ribbon(ggplot2::aes(ymax = ymax, ymin = ymin, fill = Aberrant), # working to add color to aberrant
                             alpha = .3) +
        ggplot2::facet_wrap(~ids) +
        ggplot2::ylim(c(0,1)) +
        ggplot2::theme(strip.text.x = ggplot2::element_text(margin = ggplot2::margin(0, 0, 2, 0))) +
        ggplot2::scale_fill_manual(values = c("#094bab", "#cc0c00")) +
        ggplot2::labs(title = "Person Response Functions",
                      x = "Item Difficulty",
                      y = "P(x = 1)")
    }
    # I will add more person plots in the future.
    # I would like to add a person residual thing
    # As well as observed vs expected person plots
    # and parametric plots
    # Actually, we could probably get all the same plots for persons as we can for items


  }

  p <- p + ggplot2::theme_classic()

  if (return_data){
    return(list(plot = p, data = plt_data))
  }

  return(p)
}




irf_probs <- function(wizirt_fit, theta = seq(-6, 6, length.out = 100)){
  data <- wizirt_fit$fit$parameters$coefficients

  prob <- lapply(theta, function(x) data$guessing +
                   (1 - data$guessing)/(1 + exp(-1.7*data$discrimination*
                                                  (x-data$difficulty))))
  names(prob) <- theta
  cbind.data.frame(item = data$item, tibble::as_tibble(prob, .name_repair = 'unique')) %>%
    tidyr::pivot_longer(cols = -1, names_to = 'x', values_to = 'y') %>%
    dplyr::mutate(x = as.numeric(x))
}
