#' @export
gg_histogram <- function (data = NULL, mapping = aes(), ..., verbose = getOption("verbose")) {

  gg_plot(data, mapping, verbose = verbose) +
    geom_histogram(...)

}
