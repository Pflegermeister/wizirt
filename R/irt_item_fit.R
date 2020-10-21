#' A function used to get additional item-level information, such as ICC probabilities, and item information.
#'
#' @param wizirt_fit An object coming from the fit_wizirt function.
#' @param stats A character or character string identifying item-level fit measures. Must be one of c('Zh', 'X2', 'G2', 'infit'). More are coming very soon.  Default is 'X2'.
#' @return A list with item_stats (item-level statistics), item_information (data for item and test information), and item_probabilities (data for ICC curves).
#' @examples
#' ifa <- wizirt2:::irt_item_fit(my_model)
#' @export
irt_item_fit <- function(wizirt_fit, stats = c('X2')){

  if(!wizirt_fit$fit$model$item_type %in% c("Rasch", "1PL", "2PL")){
    rlang::abort(glue::glue("Cannot calculate item-fit statistics for models of type: {wizirt_fit$fit$model$item_type}"))
  }

  out <- list(item_stats = NULL,
              item_information = NULL,
              item_probabilities = NULL)

  # Item_stats ---------------------------------------------------------------------------------------
  # If anyone tries to use groups this won't work. xxxx

  mod <- to_mirt(wizirt_fit)

  item_stats <- mirt::itemfit(mod, fit_stats = stats,
                              Theta = matrix(wizirt_fit$fit$parameters$persons$ability))

  item_stats <- dplyr::left_join(wizirt_fit$fit$parameters$coefficients,
                                 item_stats, by = 'item')

  # item_information ---------------------------------------------------------------------------------


  item_information <- data.frame(item = names(wizirt_fit$fit$data), data.frame(t(sapply(1:ncol(wizirt_fit$fit$data), function(i){ item <- mirt::extract.item(mod, i)
  mirt::iteminfo(item, Theta = seq(-6, 6, length.out = 100))}))))
  item_information <- item_information %>% `colnames<-`(c('item', seq(-6, 6, length.out = 100)))
  item_information <- item_information %>%
    tidyr::pivot_longer(cols = -1, names_to = 'theta', values_to = 'info') %>%
    dplyr::mutate(theta = as.numeric(theta))

  # item_probabilities ---------------------------------------------------------------------------------
  plt_list <- list()

  for(i in 1:ncol(wizirt_fit$fit$data)){
    plt <- mirt::itemplot(mod, i)
    pltdata <- data.frame(lapply(plt$panel.args, function(x) do.call(cbind, x))[[1]])
    y = pltdata$y
    names(y) <- pltdata$x
    plt_list[[i]] <- y
  }

  plt_data <- tibble::as_tibble(do.call(rbind, plt_list))
  plt_data$item <- colnames(wizirt_fit$fit$data)
  item_probabilities <- plt_data %>% tidyr::pivot_longer(cols = -item, names_to = "x", values_to = "y") %>% dplyr::mutate(x = as.numeric(x))
  # joining ------------------------------------------------------------------------------------------

  out$item_stats <- item_stats
  out$item_information <- item_information
  out$item_probabilities <- item_probabilities

  class(out) <- c(paste0("_", class(out)), "wizirt_ifa")
  out
}

to_mirt <- function(wizirt_fit){
  fd <- `colnames<-`(cbind(matrix(0, ncol = ncol(wizirt_fit$fit$data),
                                  nrow = nrow(wizirt_fit$fit$data)),
                           matrix(1, ncol= ncol(wizirt_fit$fit$data),
                                  nrow = nrow(wizirt_fit$fit$data))),
                     c(paste(colnames(wizirt_fit$fit$data), 1, sep = "_") ,
                       paste(colnames(wizirt_fit$fit$data), 2, sep = "_")))

  fd <- fd[,as.vector(rbind(1:ncol(wizirt_fit$fit$data), (ncol(wizirt_fit$fit$data)+1):ncol(fd)))]
  fd <- `colnames<-`(matrix(fd, ncol = ncol(fd)), colnames(fd))

  Call = wizirt_fit$fit$model$engine$call

  Data = list(data = data.frame(wizirt_fit$fit$data),
              group = factor(rep("all", nrow(wizirt_fit$fit$data)), levels = "all", labels = 1),
              groupNames = "all",
              ngroups = 1,
              mins = rep(0, ncol(wizirt_fit$fit$data)),
              fulldata = list(fd),
              K = rep(2L, ncol(wizirt_fit$fit$data)),
              nitems = ncol(wizirt_fit$fit$data),
              model = list(tabdata = matrix(wizirt_fit$fit$data)))

  Model = list(itemloc = seq(from = 1, to = (ncol(wizirt_fit$fit$data)*2+1), by = 2),
               nfact = 1,
               Theta = wizirt_fit$fit$parameters$persons$ability)

  a1 = wizirt_fit$fit$parameters$coefficients$discrimination
  d = wizirt_fit$fit$parameters$coefficients$difficulty
  g = rep(-999, length(wizirt_fit$fit$parameters$coefficients$difficulty)) # Notice this breaks for 3PL
  u = rep(999, length(wizirt_fit$fit$parameters$coefficients$difficulty))

  est = `names<-`(dplyr::case_when(wizirt_fit$fit$model$item_type == "Rasch" ~ c(F, T, F, F),
                                   T ~ c(T, T, F, F)), c("a1", "d", "g", "u"))

  pars_den <- function (obj, Theta)
  {
    gpars <- ExtractGroupPars(obj)
    mu <- gpars$gmeans
    sigma <- gpars$gcov
    d <- mirt_dmvnorm(Theta, mean = mu, sigma = sigma)
    d <- ifelse(d < 1e-300, 1e-300, d)
    d
  }
  pars = list()
  for(i in 1:ncol(wizirt_fit$fit$data)){
    pars[[i]] = new("dich",
                    par = c(a1[[i]], d[[i]], g[[i]], u[[i]]),
                    est = est,
                    nfact = 1L,
                    itemclass = 1L,
                    ncat = length(unique(wizirt_fit$fit$data[[i]])))
  }
  pars[[i + 1]] = new("GroupPars",
                      density = 0,
                      dentype = "Gaussian",
                      den = pars_den,
                      nfact = 1L,
                      itemclass = 0L)

  # I need to put this somewhere for future review.
  # Here I have really hacked up the mirt classes to make them fit.
  # If I can make the translation more elegant than I can go back and forth easily.
  # However, this is not elegant at all.

  ParObjects = list(pars = pars)

  Internals = list(CUSTIM.IND = 0)

  Fit = list(F = matrix(0, ncol = wizirt_fit$fit$model$n_factors, nrow = nrow(wizirt_fit$fit$parameters$persons)))

  Options = list(exploratory = FALSE, quadpts = mirt:::select_quadpts(nfact = wizirt_fit$fit$model$n_factors))

  mod <- new('SingleGroupClass',
             Call = Call,
             Data=Data,
             Model=Model,
             Fit = Fit,
             ParObjects=ParObjects,
             Options = Options,
             Internals=Internals)
  return(mod)
}
