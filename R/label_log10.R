#' label_log10
#'
#' @param x numeric vector
#' @param digits integer
#' @param TeX logical
#' @param output passed to [latex2exp::TeX()]
#'
#' @importFrom latex2exp TeX
#' @importFrom stringr str_c str_replace_all
#' @importFrom tidyr replace_na
#' @importFrom purrr map
#'
#' @export
label_log10 <- function (
  x,
  digits = 1,
  TeX = TRUE,
  output = "expression"
) {

  str_replace_expr <- function (expr, pattern, replacement) {
    deparsed <- purrr::map(expr, deparse)
    f <- function (x) stringr::str_replace_all(x, pattern, replacement)
    replaced <- purrr::map(deparsed, f)
    collapsed <- purrr::map(replaced, str_c, collapse = "")
    formalized <- purrr::map(collapsed, str2lang) # turn strings back into calls
    return(as.expression(formalized))
  }

  if (isTRUE(TeX)) {

    formatted <- stringr::str_c("10^{", round(log10(x), digits = digits), "}")
    sanitized <- tidyr::replace_na(formatted, "")
    latex_expr <- latex2exp::TeX(sanitized, output = output)
    labels <- str_replace_expr(latex_expr, "\\.\\s+([0-9]+)", ".\\1")

  } else {

    labels <- stringr::str_c("10^", log10(x))

  }

  return(labels)

}
