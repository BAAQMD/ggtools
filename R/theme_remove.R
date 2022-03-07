theme_remove <- function (...) {
  element_names <- map_chr(purrr::cross(list(...)), str_c, collapse = ".")
  element_objects <- replicate(length(element_names), element_blank())
  rlang::exec(ggplot2::theme, !!!set_names(element_objects, element_names))
}
