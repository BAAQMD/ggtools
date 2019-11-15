humanize_id_vars <- function (
  input_data,
  prefix = "#",
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[humanize_id_vars] ", ...)

  recode_id <- function (x, prefix = "#") {

    if (is.character(x)) {
      return(x)
    }

    str_prefix <- function (x) {
      cleaned <- str_remove_all(x, pattern = str_c("^", prefix, "+"))
      str_c(prefix, cleaned)
    }

    recoded <-
      factor(
        str_prefix(x),
        levels = str_prefix(sort(unique(x))))

    return(recoded)

  }

  id_vars <-
    tidyselect::vars_select(
      names(input_data),
      ends_with("_id"))

  if (length(id_vars) > 0) {

    msg("prefixing id vars with \"", prefix, "\": ", str_csv(id_vars))

    humanized_data <-
      input_data %>%
      mutate_at(
        vars(id_vars),
        ~ recode_id(.))

    return(humanized_data)

  } else {

    return(input_data)

  }

}
