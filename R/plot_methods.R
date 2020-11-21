#' Plot method for wizirt objects
#'
#' @description All plots in wizirt use ggplot2 as a backend. Type 'theta_diff' also uses gghalves. For the item-focused plots, the types can be included in the same string to overlay the plots (e.g. resid_trace).
#' For large numbers of persons and/or items, you may want to consider limiting the number printed at a time on some plots.
#' @param type Character. Currently, can be 'obs', 'trace', 'info', 'resid', 'stand', 'tinfo', 'theta', 'diff', 'theta_diff', 'np_prf', 'ld', or 'ld_pairs'.
#' @param items Which items to plot? Either a numeric vector of item positions in the column names of the data, or a vector of the item names to plot. If nothing is specifed all items will be plotted.
#' @param persons Which persons to plot? Either a numeric vector of person positions in the row names of the data, or a vector of the person ids to plot. If nothing is specifed all persons will be plotted.
#' @param facets Logical. Should the plots be faceted? Default is TRUE.
#' @param quads Numeric. For plots using residuals (i.e. resid, stand). How many quantiles should the data be broken into?
#' @param return_data Logical. Should the plot data be returned. If TRUE returns a list with the plot and the data.
#' @param pfa An object from irt_person_fit(). If omitted, irt_person_fit() is called within the function with the default settings (only for np_prf)
#' @param ifa An object from irt_item_fit(). If omitted, irt_item_fit() is called within the function with the default settings (only for tinfo)
#' @method plot wizirt
#' @examples
#' data("responses")
#' my_model <- wizirt(data = responses[, -1])
#' plot(my_model, type = "tinfo")
#' plot(my_model, type = "theta")
#' plot(my_model, type = "diff")
#' plot(my_model, type = "theta_diff")
#' plot(my_model, type = "obs", items = 1:4)
#' plot(my_model, type = "trace", items = 1:4)
#' plot(my_model, type = "trace", items = 1:4, facets = FALSE)
#' plot(my_model, type = "resid", items = 1:4)
#' plot(my_model, type = "stand", items = 1:4)
#' plot(my_model, type = "info", items = 1:4)
#' plot(my_model, type = "resid_trace", items = 1:4)
#' plot(my_model, type = "np_prf", facets = T, persons = 1:4)
#' plot(my_model, type = "np_prf", facets = F)
#' @export
plot.wizirt <- function(wizirt_fit,
                        type = "tinfo",
                        items = NULL,
                        persons = NULL,
                        facets = TRUE,
                        quads = 10,
                        return_data = FALSE,
                        pfa = NULL,
                        ifa = NULL) {
  plt_data <- list()
  plt <- NULL

  if (is.null(persons)) {
    # message('all items')
    persons <- wizirt_fit$fit$parameters$persons$ids
  }
  if (all(persons %in% seq_len(nrow(wizirt_fit$fit$data)))) {
    persons <- wizirt_fit$fit$parameters$persons$ids[persons]
  }

  if (is.null(items)) {
    # message('all items')
    items <- colnames(wizirt_fit$fit$data)[seq_len(ncol(wizirt_fit$fit$data))]
  }
  if (is.numeric(items)) {
    items <- colnames(wizirt_fit$fit$data)[items]
  }

  if (grepl("obs", type)) {
    plt <- "obs"

    df <- dplyr::bind_cols(wizirt_fit$fit$parameters$persons, wizirt_fit$fit$data) %>%
      dplyr::select(ids, ability, tidyselect::all_of(items)) %>%
      tidyr::pivot_longer(cols = c(-ids, -ability), names_to = "item") %>%
      dplyr::filter(item %in% items, ids %in% persons)

    plt_data[["obs"]] <- df

    if (facets == F) {
      p <- df %>%
        ggplot2::ggplot(ggplot2::aes(x = ability, y = value, color = item)) +
        ggplot2::geom_jitter(alpha = .5, cex = 2, width = 0, height = .05)
    } else {
      p <- df %>%
        ggplot2::ggplot(ggplot2::aes(x = ability, y = value)) +
        ggplot2::geom_jitter(alpha = .5, cex = 2, width = 0, height = .05) +
        ggplot2::facet_wrap(~item)
    }

    p <- p +
      ggplot2::labs(title = "Examinee Responses by Ability")
  }

  if (grepl("trace", type)) {
    df <- irf_probs(wizirt_fit) %>%
      dplyr::filter(item %in% items)

    plt_data[["trace"]] <- df

    if (is.null(plt)) {
      plt <- "trace"
      if (facets == F) {
        p <- df %>%
          ggplot2::ggplot(ggplot2::aes(
            x = x,
            y = y,
            color = item
          )) +
          ggplot2::geom_line() +
          ggplot2::theme(legend.position = "bottom")
      } else {
        p <- df %>%
          ggplot2::ggplot(ggplot2::aes(
            x = x,
            y = y
          )) +
          ggplot2::geom_line(color = "#130d42") +
          ggplot2::theme(legend.position = "bottom") +
          ggplot2::facet_wrap(~item)
      }
      p <- p +
        ggplot2::labs(title = "Item characteristic curves", y = "P(X ==1)")
    } else { # if plt is not null

      plt <- paste(plt, "trace", sep = "_")

      p <- p + ggplot2::geom_line(ggplot2::aes(
        x = x,
        y = y, group = item
      ),
      data = df
      )
    }
  }

  if (grepl("resid", type)) {
    breaks <- quantile(
      wizirt_fit$fit$parameters$persons$ability,
      seq(0, 1, length.out = quads + 1)
    )
    if (length(unique(breaks)) != quads + 1) rlang::warn("Breaks are not unique. Decreasing quadratures.")
    df <- cbind(
      Ability = wizirt_fit$fit$parameters$persons$ability,
      breaks = cut(wizirt_fit$fit$parameters$persons$ability,
        unique(breaks),
        labels = 1:(length(unique(breaks)) - 1), include.lowest = T
      ),
      wizirt_fit$fit$data
    ) %>%
      as.data.frame() %>%
      dplyr::group_by(breaks) %>%
      dplyr::summarise(dplyr::across(c(
        Ability,
        colnames(wizirt_fit$fit$data)
      ),
      mean,
      .names = "{.col}"
      ), .groups = "drop_last") %>%
      tidyr::pivot_longer(
        cols = -breaks:-Ability,
        names_to = "item",
        values_to = "Probability"
      ) %>%
      dplyr::filter(item %in% items)

    plt_data[["resid"]] <- df

    if (is.null(plt)) {
      plt <- "resid"
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
        ggplot2::labs(title = "Item Residual Plots", y = "Residuals")
    } else { # plt != NULL
      plt <- paste(plt, "resid", sep = "_")
      p <- p + ggplot2::geom_point(ggplot2::aes(
        x = Ability,
        y = Probability,
        group = item
      ),
      data = df
      )
    }
  }

  if (grepl("stand", type)) {
    breaks <- quantile(
      wizirt_fit$fit$parameters$persons$ability,
      seq(0, 1, length.out = quads + 1)
    )
    if (length(unique(breaks)) != quads + 1) rlang::warn("Breaks are not unique. Decreasing quadratures.")

    df <- cbind(
      Ability = wizirt_fit$fit$parameters$persons$ability,
      breaks = cut(wizirt_fit$fit$parameters$persons$ability,
        unique(breaks),
        labels = 1:(length(unique(breaks)) - 1), include.lowest = T
      ),
      wizirt_fit$fit$data
    ) %>%
      as.data.frame() %>%
      dplyr::group_by(breaks) %>%
      dplyr::summarise(dplyr::across(c(
        Ability,
        colnames(wizirt_fit$fit$data)
      ),
      mean,
      .names = "{.col}"
      ))
    df <- df %>%
      tidyr::pivot_longer(
        cols = -breaks:-Ability,
        names_to = "item",
        values_to = "Probability"
      ) %>%
      dplyr::mutate(Ability = round(Ability, 10)) %>%
      dplyr::left_join(irf_probs(wizirt_fit, theta = df$Ability) %>%
        dplyr::rename(Ability = "x") %>%
        dplyr::mutate(Ability = round(Ability, 10)), by = c("Ability", "item")) %>%
      dplyr::mutate(stn_res = (Probability - y) / sd(Probability - y)) %>%
      dplyr::filter(item %in% items)


    plt_data[["stand"]] <- df

    if (is.null(plt)) {
      plt <- "stand"
      if (facets == F) {
        p <- df %>%
          dplyr::filter(item %in% items) %>%
          ggplot2::ggplot(ggplot2::aes(
            x = Ability,
            y = stn_res,
            color = item
          )) +
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
        ggplot2::labs(title = "Standardized Residual Plot", y = "Standardized Residuals")
    } else { # plt != NULL
      plt <- paste(plt, "stand", sep = "_")
      p <- p + ggplot2::geom_point(ggplot2::aes(
        x = Ability,
        y = stn_res,
        group = item
      ),
      data = df
      )
    }
  }

  if (grepl("info", type) & !grepl("tinfo", type)) {
    ip <- irf_probs(wizirt_fit)
    df <- ip %>%
      dplyr::left_join(wizirt_fit$fit$parameters$coefficients, by = "item") %>%
      dplyr::mutate(info = (discrimination^2) * ((y - guessing)^2 / (1 - guessing)^2) * ((1 - y) / (y))) %>%
      dplyr::select(-difficulty, -discrimination, -guessing) %>%
      dplyr::filter(item %in% items)

    plt_data[["info"]] <- df

    if (is.null(plt)) {
      if (facets == FALSE) {
        p <- df %>%
          ggplot2::ggplot(ggplot2::aes(
            x = x,
            y = info,
            color = item
          )) +
          ggplot2::geom_line() +
          ggplot2::theme(legend.position = "bottom") +
          ggplot2::labs(title = "Item information functions")
      } else {
        p <- df %>%
          ggplot2::ggplot(ggplot2::aes(
            x = x,
            y = info
          )) +
          ggplot2::geom_line() +
          ggplot2::geom_line(color = "#130d42") +
          ggplot2::facet_wrap(~item) +
          ggplot2::labs(title = "Item information functions")
      }
    } else { # plt != NULL
      plt <- paste(plt, "info", sep = "_")
      p <- p + ggplot2::geom_line(ggplot2::aes(
        x = x,
        y = info,
        group = item
      ),
      data = df
      )
    }
  }

  # Test Generally Plots

  if (type == "tinfo" | grepl("tinfo", type) & grepl("SE", type)) {
    if (is.null(ifa)) {
      ifa <- irt_item_fit(wizirt_fit)
    }
    df <- ifa$item_information %>%
      dplyr::filter(item %in% items) %>%
      dplyr::group_by(theta) %>%
      dplyr::summarize(info = sum(info), .groups = "drop_last")
    plt_data[["tinfo"]] <- df

    plt <- "tinfo"
    p <- df %>%
      ggplot2::ggplot() +
      ggplot2::geom_ribbon(ggplot2::aes(x = theta, ymax = info),
        ymin = 0, fill = "#094bab", alpha = .3, color = "#130d42"
      ) +
      ggplot2::theme_classic() +
      ggplot2::labs(title = "Test Information Function")
  }

  if (type == "theta" | grepl("theta", type) & grepl("SE", type)) {
    df <- wizirt_fit$fit$parameters$persons %>%
      dplyr::filter(ids %in% persons)
    plt_data[["theta"]] <- df

    if (is.null(plt)) {
      plt <- "theta"

      p <- df %>%
        ggplot2::ggplot() +
        ggplot2::geom_density(ggplot2::aes(x = ability),
          fill = "#094bab",
          alpha = .3,
          color = "#130d42"
        ) +
        ggplot2::labs(title = "Distribution of Person Abilities")
    } else {
      plt <- paste(plt, "theta", sep = "_")
      p <- p + ggplot2::geom_density(
        fill = "#094bab",
        alpha = .3,
        color = "#130d42", data = df
      )
    }
  }

  if (type == "SE" | (grepl("theta", type) | grepl("tinfo", type)) & grepl("SE", type)) {
    df <- wizirt_fit$fit$parameters$persons %>%
      dplyr::filter(ids %in% persons) %>%
      dplyr::distinct(ability, std_err)
    plt_data[["SE"]] <- df

    if (is.null(plt)) {
      plt <- "SE"

      p <- df %>%
        ggplot2::ggplot() +
        ggplot2::geom_ribbon(ggplot2::aes(x = ability, ymax = std_err), ymin = 0, fill = "#566D81", alpha = .5) +
        ggplot2::labs(title = "Standard Error of Measured Abilities")
    } else {
      plt <- paste(plt, "SE", sep = "_")
      p <- p + ggplot2::geom_line(ggplot2::aes(x = ability, y = std_err), lty = 2, data = df) +
        ggplot2::labs(subtitle = "Black dotted line is SE", y = "")
    }
  }

  if (type == "diff") {
    df <- wizirt_fit$fit$parameters$coefficients[, 1:2]
    plt_data[["diff"]] <- df %>% dplyr::filter(item %in% items)

    if (is.null(plt)) {
      plt <- "diff"

      p <- df %>%
        ggplot2::ggplot(ggplot2::aes(x = difficulty)) +
        ggplot2::geom_histogram(
          fill = "#094bab",
          alpha = .3,
          color = "#130d42", bins = 30
        ) +
        ggplot2::labs(title = "Distribution of Item Locations")
    } else {
      plt <- paste(plt, "theta", sep = "_")
      p <- p + ggplot2::geom_histogram(
        fill = "#094bab",
        alpha = .3,
        color = "#130d42",
        data = df
      )
    }
  }

  if (grepl("theta", type) & grepl("diff", type)) {
    df <- dplyr::bind_rows(
      wizirt_fit$fit$parameters$persons %>%
        dplyr::filter(ids %in% persons) %>%
        dplyr::select(ability, ids) %>%
        dplyr::rename(theta = "ability") %>%
        dplyr::mutate(
          type = "person",
          ids = as.character(ids)
        ),
      wizirt_fit$fit$parameters$coefficients %>%
        dplyr::filter(item %in% items) %>%
        dplyr::select(item, difficulty) %>%
        dplyr::rename(ids = "item", theta = "difficulty") %>%
        dplyr::mutate(type = "item", ids = as.character(ids))
    )

    p <- df %>% ggplot2::ggplot(ggplot2::aes(x = theta, y = type)) +
      ggplot2::geom_jitter(width = 0) +
      ggplot2::labs(
        title = "Distribution of Person Abilities and Item Locations",
        y = "", x = "Theta"
      )

    plt_data[["theta_diff"]] <- wizirt_fit$fit$parameters$persons %>%
      dplyr::filter(ids %in% persons)
  }

  # Person Plots

  if (grepl("np_prf", type)) {
    if (is.null(pfa)) {
      pfa <- irt_person_fit(wizirt_fit, stat = "Ht", items = items)
    }
    df <- pfa$prf %>%
      dplyr::filter(ids %in% persons)
    plt_data[["np_prf"]] <- df
    if (facets == FALSE) {
      p <- df %>%
        ggplot2::ggplot(ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_line(ggplot2::aes(group = factor(ids), color = Aberrant)) +
        ggplot2::ylim(c(0, 1)) +
        ggplot2::scale_color_manual(values = c("black", "#cc0c00")) +
        ggplot2::labs(
          title = "Person Response Functions",
          x = "Item Difficulty",
          y = "P(x = 1)"
        )
    } else {
      p <- df %>%
        ggplot2::ggplot(ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_line(ggplot2::aes(color = Aberrant)) +
        ggplot2::geom_ribbon(ggplot2::aes(ymax = ymax, ymin = ymin, fill = Aberrant), # working to add color to aberrant
          alpha = .3
        ) +
        ggplot2::facet_wrap(~ids) +
        ggplot2::ylim(c(0, 1)) +
        ggplot2::theme(strip.text.x = ggplot2::element_text(margin = ggplot2::margin(0, 0, 2, 0))) +
        ggplot2::scale_color_manual(values = c("#094bab", "#cc0c00")) +
        ggplot2::scale_fill_manual(values = c("#094bab", "#cc0c00")) +
        ggplot2::labs(
          title = "Person Response Functions",
          x = "Item Difficulty",
          y = "P(x = 1)"
        )
    }
    # I will add more person plots in the future.
    # I would like to add a person residual thing
    # As well as observed vs expected person plots
    # and parametric plots
    # Actually, we could probably get all the same plots for persons as we can for items
  }

  # assumptions
  ## local dependence
  if (type == "ld_pairs") {
    assumptions <- irt_assume(wizirt_fit)
    df <- assumptions$ld %>%
      tidyr::pivot_longer(cols = item_1:item_2, values_to = "item") %>%
      dplyr::filter(item %in% items) %>%
      dplyr::mutate(name2 = dplyr::case_when(
        name == "item_1" ~ "item_2",
        T ~ "item_1"
      )) %>%
      tidyr::pivot_longer(cols = c(name, name2)) %>%
      tidyr::pivot_wider(names_from = name, values_from = item)

    plt_data[["ld_pairs"]] <- df

    p <- df %>%
      ggplot2::ggplot(ggplot2::aes(x = name, y = name2, fill = pvals < .05)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_manual(values = c("transparent", "#9e0317")) +
      ggplot2::theme_classic() +
      ggplot2::labs(
        title = "Local Dependence by Item Pair",
        y = "Item",
        x = "Item"
      )
  }
  if (type == "ld") {
    assumptions <- irt_assume(wizirt_fit)
    df <- assumptions$ld %>%
      dplyr::filter(item_1 %in% items, item_2 %in% items)
    plt_data[["ld"]] <- df

    p <- df %>%
      tidyr::pivot_longer(cols = item_1:item_2, values_to = "item") %>%
      ggplot2::ggplot(ggplot2::aes(x = LD_std, y = item)) +
      ggplot2::geom_boxplot(fill = "#566D81", alpha = .8) +
      ggplot2::labs(title = "Local Dependence by Item", y = "Item") +
      ggplot2::theme_classic()
  }

  p <- p + ggplot2::theme_classic()

  if (return_data) {
    return(list(plot = p, data = plt_data))
  }

  return(p)
}

#' A funirction for handling large numbers of students and items in plots.
#'
#' @description This function helps to break the plots into several plots when many items or persons are present.
#' @param object An object of class wizirt_fit.
#' @param type Character. Currently, can be 'obs', 'trace', 'info', 'resid', 'stand', 'tinfo', 'theta', 'diff', 'theta_diff', 'np_prf', 'ld', or 'ld_pairs'.
#' @param items_per Numeric. How many items to plot per page?
#' @param persons_per Numeric. How many persons to plot per page?
#' @param pfa An object from irt_person_fit(). If omitted, irt_person_fit() is called within the function with the default settings.
#' @export
plot_wrap <- function(object, type, items_per = NULL, persons_per = NULL, pfa = NULL) {
  p <- list()
  rem <- ifelse(is.null(items_per),
    nrow(object$fit$data) %/% persons_per,
    ncol(object$fit$data) %/% items_per
  )
  if (rem <= 1) {
    print(plot(object, type = type))
  } else {
    if (!is.null(items_per)) {
      for (i in seq_len(rem + 1)) {
        if ((seq_len(items_per) + items_per * (i - 1))[1] > ncol(object$fit$data)) {
          return(p)
        }
        p[[i]] <- plot(object, type = type, items = seq_len(items_per) + items_per * (i - 1))
      }
    } else {
      for (i in seq_len(rem + 1)) {
        if ((seq_len(persons_per) + persons_per * (i - 1))[1] > nrow(object$fit$data)) {
          return(p)
        }
        p[[i]] <- plot(object,
          type = type,
          persons = seq_len(persons_per) + persons_per * (i - 1),
          pfa = pfa
        )
      }
    }
  }
  return(p)
}


irf_probs <- function(wizirt_fit, theta = seq(-6, 6, length.out = 100)) {
  data <- wizirt_fit$fit$parameters$coefficients

  prob <- lapply(theta, function(x) {
    data$guessing +
      (1 - data$guessing) / (1 + exp(-1.7 * data$discrimination *
        (x - data$difficulty)))
  })
  names(prob) <- theta
  cbind.data.frame(item = data$item, tibble::as_tibble(prob, .name_repair = "unique")) %>%
    tidyr::pivot_longer(cols = -1, names_to = "x", values_to = "y") %>%
    dplyr::mutate(x = as.numeric(x))
}
