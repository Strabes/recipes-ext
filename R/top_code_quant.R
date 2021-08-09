#' Top code numeric features based on quantile
#'
#' `step_top_code_quant` creates a *specification* of a recipe
#'  step that will top code numeric data using a quantile learned
#'  on a training set.
#'
#' @param prob A float providing the quantile at which to top code
#' @param ref_val A float, learned from the training data,
#'  representing the `prob` quantile of the predictor.
#' @inheritParams recipes::step_center
#'
#' @export
step_top_code_quant <- function(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  ref_val = NULL,
  prob = 0.98,
  skip = FALSE,
  id = recipes::rand_id("top_code_quant")
) {

  terms <- recipes::ellipse_check(...)

  recipes::add_step(
    recipe,
    step_top_code_quant_new(
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

step_top_code_quant_new <-
  function(terms, role, trained, ref_val, prob, skip, id) {
    recipes::step(
      subclass = "top_code_quant",
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
prep.step_top_code_quant <- function(x, training, info = NULL, ...) {
  col_names <- recipes::terms_select(terms = x$terms, info = info)


  if (!any(names(x) == "prob")) {
    x$prob <- 0.98
  }

  ref_val <- purrr::map(training[, col_names],  stats::quantile, probs = x$prob)

  step_top_code_quant_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    ref_val = ref_val,
    prob = x$prob,
    skip = x$skip,
    id = x$id
  )
}

top_code <- function(x, val){ifelse(x>val,val,x)}

#' @importFrom recipes bake
#' @export
bake.step_top_code_quant <- function(object, new_data, ...) {

  vars <- names(object$ref_val)

  new_data[, vars] <-
    purrr::map2_dfc(new_data[, vars], object$ref_val, top_code)

  ## Always convert to tibbles on the way out
  tibble::as_tibble(new_data)
}

#' @export
print.step_top_code_quant <-
  function(x, width = max(20, options()$width - 35), ...) {
    cat("Percentile-based top coding transformation on ", sep = "")
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
