perc <- function(range = seq(0,1,by=0.25), trans = NULL) {
  dials::new_quant_param(
    type = "float",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(perc = "Percentiles"),
    finalize = NULL
  )
}
