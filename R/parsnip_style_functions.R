#' Estimate an IRT model
#'
#' Estimate an IRT model using various engines
#' @inheritParams irt
#' @param engine Character. Currently supported engines are 'mirt' and 'ltm' for Rasch, 1PL, 2PL, and 3PL models. 'eRm' is supported for Rasch models only.
#' @inheritParams fit_wizirt
#' @export
wizirt <- function(data, rownames = NULL, item_type = "Rasch", engine = "mirt", tol = 1e-05){
  irt_pars = TRUE # irt_pars cannot equal false yet
  irt(item_type = item_type, irt_pars = irt_pars) %>%
    set_engine(engine = engine) %>%
    fit_wizirt(data = data)
}

#' Describe the model you will run
#'
#' @description irt() is used to describe a model, it must be used in conjunction with set_engine(), and fit_wizirt().
#' @param mode Must be 'regression' currently.
#' @param item_type Character. Must be one of 'Rasch', '1PL', '2PL' or '3PL'.
#' @param rownames Optional unique row IDs for the data (i.e. examinee IDs). If omitted, uses 1:nrow(data).
#' @param tol Numeric. Convergence criterion. Currently only implemented when engine is mirt.
#' @export
irt <- function(mode = "regression", item_type = NULL, rownames = NULL, tol = 1e-5){
  irt_pars = TRUE # irt_pars cannot equal false yet
  args <- list(item_type = rlang::enquo(item_type),
               irt_pars = rlang::enquo(irt_pars),
               rownames = rlang::enquo(rownames),
               tol = rlang::enquo(tol))
  out <- list(args = args,
              eng_args = NULL,
              mode = mode,
              method = NULL,
              engine = NULL)
  class(out) <- parsnip::make_classes("irt")
  out
}

#' Run the irt model you described
#'
#' @description fit_wizirt() runs a model that has been built with irt() and set_engine().
#' @param object An object from set_engine()
#' @param data An Person x Items matrix or dataframe of dichotomous response values (e.g. correct/incorrect). Rows are persons and columns are items, one row per person, one column per item. No other information allowed.
#' @export
fit_wizirt <- #fit_wizirt.model_spec How do I get the fit_wizirt without the '.model_spec'?
  function(object,
           data,
           control = parsnip:::control_parsnip(), # formula %>%  will be theta ~ items + covariates and # will remove items not wanted.
           ...
  ) {
    #object <- check_mode(object, levels(y)) # I want to retain this to limit changes, however I am not sure where this fits in.
    dots <- rlang::quos(...)
    if (is.null(object$engine)) {
      eng_vals <- parsnip:::possible_engines(object)
      object$engine <- eng_vals[1]
      if (control$verbosity > 0) {
        rlang::warn(glue::glue("Engine set to `{object$engine}`."))
      }
    }

    cl <- match.call(expand.dots = TRUE)
    eval_env <- rlang::env()
    eval_env$responses <- data
    fit_interface <- check_wizirt_interface(eval_env$responses, cl) #xxxx check 9/24/2020 dependencies

    # populate `method` with the details for this model type
    object <- parsnip:::add_methods(object, engine = object$engine)

    parsnip:::check_installs(object)

    interfaces <- paste(fit_interface, object$method$fit$interface, sep = "_")

    # Now call the wrappers that transition between the interface
    # called here ("fit" interface) that will direct traffic to
    # what the underlying model uses. For example, if a formula is
    # used here, `fit_interface_formula` will determine if a
    # translation has to be made if the model interface is x/y/

    res <- # Ok so this will have to set it to wizirt_form

      wizirt_form( # xxxx check 9/24/2020 for dependencies
        object = object,
        env = eval_env,
        control = control,
        ...
      )
    #rlang::abort(glue::glue("{interfaces} is unknown."))

    model_classes <- class(res$fit)
    class(res) <- c("wizirt", "model_fit", model_classes[1])
    res
  }

wizirt_form <-
  function(object, control, env, ...) {

    if (object$mode == "classification") {
      rlang::abort("Classification models not currently supported by wizirt.")
    }

    # evaluate quoted args once here to check them
    #object <- check_args(object) # had to drop for the time being

    # sub in arguments to actual syntax for corresponding engine
    object <- parsnip:::translate(object, engine = object$engine)

    fit_call <- make_wizirt_call(object, env = env)

    res <- list(
      #lvl = y_levels,
      spec = object
    )

    elapsed <- system.time(
      res$fit <- parsnip:::eval_mod(
        fit_call,
        capture = control$verbosity == 0,
        catch = control$catch,
        env = env,
        ...
      )
    )
    res$preproc <- list(y_var = all.vars(env$formula[[2]]))
    res$elapsed <- elapsed
    res
  }

make_wizirt_call <- function(object, env = NULL) {
  fit_args <- object$method$fit$args

  # Get the arguments related to data:
  if (is.null(object$method$fit$data)) {
    data_args <- c(formula = "model", data = "data") # Ok, so how is this used?
  } else {
    data_args <- object$method$fit$data
  }

  # add data arguments
  for (i in seq_along(data_args)) {
    fit_args[[ unname(data_args[i]) ]] <- rlang::sym(names(data_args)[i])
  }

  # sub in actual formula
  fit_args[[ unname(data_args["formula"]) ]]  <- env$formula


  fit_call <- parsnip:::make_call(
    fun = object$method$fit$func["fun"],
    ns = object$method$fit$func["pkg"],
    fit_args
  )
  fit_call
}

check_wizirt_interface <- function(data, cl, model) {
  parsnip:::inher(data, c("data.frame", "matrix"), cl)


  # Determine the `fit()` interface
  matrix_interface <- !is.null(data) && is.matrix(data)
  df_interface <- !is.null(data) && is.data.frame(data)

  if (matrix_interface)
    return("data.frame")
  if (df_interface)
    return("data.frame")
  rlang::abort("Error when checking the interface")
}


make_irt <- function(){
  parsnip::set_new_model("irt")

  # regression will essentially mean, IRT, classification will mean LCA. That will be implemented eventually.
  parsnip::set_model_mode(model = "irt", mode = "regression")

  parsnip::set_model_engine("irt", mode = "regression", eng = "mirt")

  parsnip::set_dependency("irt", eng = "mirt", pkg = "mirt")

  parsnip::set_model_arg(
    model = "irt",
    eng = "mirt",
    parsnip = "item_type",
    original = "item_type",
    func = list(fun = "item_type"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "irt",
    eng = "mirt",
    parsnip = "tol",
    original = "tol",
    func = list(fun = "tol"),
    has_submodel = FALSE
  )

  irt <- function(mode = "regression", item_type = NULL){
    args <- list(item_type = rlang::enquo(item_type))
    out <- list(args = args,
                eng_args = NULL,
                mode = mode,
                method = NULL,
                engine = NULL)
    class(out) <- parsnip::make_classes("irt")
    out
  }

  parsnip::set_fit(
    model = "irt",
    eng = "mirt",
    mode = "regression",
    value = list(
      interface = "matrix",
      protect = c(''),
      func = c(fun = "irt_mirt"),
      defaults = list()
    )
  )

  parsnip::show_model_info("irt")


  parsnip::set_model_engine("irt", mode = "regression", eng = "ltm")

  parsnip::set_dependency("irt", eng = "ltm", pkg = "ltm")

  parsnip::set_model_arg(
    model = "irt",
    eng = "ltm",
    parsnip = "item_type",
    original = "item_type",
    func = list(fun = "item_type"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "irt",
    eng = "ltm",
    parsnip = "irt_pars",
    original = "irt_pars",
    func = list(fun = "irt_pars"),
    has_submodel = FALSE
  )

  parsnip::set_fit(
    model = "irt",
    eng = "ltm",
    mode = "regression",
    value = list(
      interface = "matrix",
      protect = c(''),
      func = c(fun = "irt_ltm"),
      defaults = list()
    )
  )
}

