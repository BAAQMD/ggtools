#' @importFrom conflicted conflict_prefer
.onAttach = function(...)
{
  conflict_prefer("scale_x_log10", "ggtools", "ggplot2")
  conflict_prefer("scale_y_log10", "ggtools", "ggplot2")
}
