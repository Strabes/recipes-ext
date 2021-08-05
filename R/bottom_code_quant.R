#' Bottom code numeric features based on quantile
#'
#' `step_bottom_code_quant` creates a *specification* of a recipe
#'  step that will bottom code numeric data using a quantile learned
#'  on a training set.
#'
#' @param prob A float providing the quantile at which to bottom code
#' @param ref_val A float, learned from the training data,
#'  representing the `prob` quantile of the predictor.
#' @inheritParams recipes::step_center
#'
#' @export
step_bottom_code_quant <- function(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  ref_val = NULL,
  prob = 0.02,
  skip = FALSE,
  id = recipes::rand_id("bottom_code_quant")
) {

  terms <- recipes::ellipse_check(...)

  recipes::add_step(
    recipe,
    step_bottom_code_quant_new(
      terms = terms,
      trained = trained,
      role = role,
      ref_val = ref_val,
      prob = prob,
      skip = skip,
      id = id
    )
  )
}

step_bottom_code_quant_new <-
  function(terms, role, trained, ref_val, prob, skip, id) {
    recipes::step(
      subclass = "bottom_code_quant",
      terms = terms,
      role = role,
      trained = trained,
      ref_val = ref_val,
      prob = prob,
      skip = skip,
      id = id
    )
  }

#' @importFrom recipes prep
#' @export
prep.step_bottom_code_quant <- function(x, training, info = NULL, ...) {
  col_names <- recipes::terms_select(terms = x$terms, info = info)


  if (!any(names(x) == "prob")) {
    x$prob <- 0.02
  }

  ref_val <- purrr::map(training[, col_names],  stats::quantile, probs = x$prob)

  step_bottom_code_quant_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    ref_val = ref_val,
    prob = x$prob,
    skip = x$skip,
    id = x$id
  )
}

bottom_code <- function(x, val){ifelse(x<val,val,x)}

#' @importFrom recipes bake
#' @export
bake.step_bottom_code_quant <- function(object, new_data, ...) {

  vars <- names(object$ref_val)

  new_data[, vars] <-
    purrr::map2_dfc(new_data[, vars], object$ref_val, bottom_code)

  ## Always convert to tibbles on the way out
  tibble::as_tibble(new_data)
}

#' @export
print.step_bottom_code_quant <-
  function(x, width = max(20, options()$width - 35), ...) {
    cat("Percentile-based bottom coding transformation on ", sep = "")
    recipes::printer(
      # Names before prep (could be selectors)
      untr_obj = x$terms,
      # Names after prep:
      tr_obj = names(x$ref_val),
      # Has it been prepped?
      trained = x$trained,
      # An estimate of how many characters to print on a line:
      width = width
    )
    invisible(x)
  }
