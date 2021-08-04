
#' @export
quant_param <- function(range = c(0,1), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(perc = "Percentiles"),
    finalize = NULL
  )
}
