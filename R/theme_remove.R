#' theme_remove
#'
#' Removes specified theme elements by setting them to [ggplot2::element_blank()].
#'
#' @details
#' All combinations of the elements in `...` will be removed.
#' See examples.
#'
#' @param ... (character) see Details and examples
#'
#' @importFrom purrr cross map_chr
#' @importFrom stringr str_c
#' @importFrom ggplot2 element_blank theme
#' @importFrom rlang exec set_names
#'
#' @export
#'
#' @examples
#' p <- ggplot(mtcars) + geom_point()
#' p + theme_remove("axis", "text")
#' p + theme_remove("axis", c("text", "line"), "x")
#' p + theme_remove("axis", c("text", "line"), c("x", "y"))
theme_remove <- function (...) {
  crossed <- purrr::cross(list(...))
  element_names <- purrr::map_chr(crossed, stringr::str_c, collapse = ".")
  element_objects <- replicate(length(element_names), ggplot2::element_blank())
  named_element_objects <- rlang::set_names(element_objects, element_names)
  theme_object <- rlang::exec(ggplot2::theme, !!!named_element_objects)
  return(theme_object)
}
