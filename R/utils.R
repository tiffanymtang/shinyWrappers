#' Check if input is NULL in conditionalPanel (ie.., Javascript)
#'
#' @param inputId Input id
#' @param input If \code{TRUE}, check if `input[inputId]` is \code{NULL}.
#'   Otherwise, check if `inputId` is \code{NULL}.
#'
#' @return String of Javascript code.
#'
#' @export
is_null_js <- function(inputId, input = TRUE) {
  if (input) {
    out <- sprintf(
      "(typeof input['%1$s'] !== 'undefined' && input['%1$s'].length > 0)",
      inputId
    )
  } else {
    out <- sprintf(
      "(typeof %1$s !== 'undefined') && %1$s.length > 0", inputId
    )
  }
  return(out)
}


#' Return custom icons
#'
#' @param icon_names Name(s) of built-in icons to return.
#' @param width Width of icon in pixels.
#' @param height Height of icon in pixels.
#' @param ... Additional arguments to pass to `htmltools::img()`.
#'
#' @return Named character vector of html snippets to display icons
#'
#' @export
get_icons <- function(icon_names, width = 20, height = 20, ...) {

  icons_vec <- c(
    ggplot = get_icon(
      src = "ggplot.png", width = width, height = height, ...
    ),
    plotly = get_icon(
      src = "plotly.svg", width = width, height = height, ...
    ),
    table = get_icon(
      src = "table.svg", width = width, height = height, ...
    ),
    histogram = get_icon(
      src = "chart-column.svg", width = width, height = height, ...
    ),
    density = get_icon(
      src = "chart-density.svg", width = width, height = height, ...
    ),
    boxplot = get_icon(
      src = "chart-boxplot.svg", width = width, height = height, ...
    ),
    point = get_icon(
      src = "chart-scatter.svg", width = width, height = height, ...
    ),
    line = get_icon(
      src = "chart-line.svg", width = width, height = height, ...
    )
  )

  out <- lapply(
    icon_names,
    function(icon_name) {
      ifelse(icon_name %in% names(icons_vec), icons_vec[icon_name], icon_name)
    }
  )

  return(setNames(icon_names, out))
}


#' Get html snippet for built-in icons.
#'
#' @inheritParams get_icons
#' @param src Name of built-in icon.
#'
#' @return Character vector of icon path.
#'
#' @export
get_icon <- function(src, width = 20, height = 20, ...) {
  dots_ls <- rlang::dots_list(...)
  if (length(dots_ls) > 0) {
    style_args <- paste(names(dots_ls), dots_ls, sep = "=", collapse = " ")
  } else {
    style_args <- ""
  }
  sprintf(
    "<img src='%s' width=%spx height=%spx %s></img>",
    file.path("www", src), width, height, style_args
  )
}


#' Add css styling
#'
#' @param ... Any number of named arguments with the name being the css
#'   selector and the value being the css style value.
#'
#' @export
css_styler <- function(...) {
  dots_ls <- rlang::dots_list(...) %>%
    purrr::compact()
  if (length(dots_ls) > 0) {
    style_str <- paste(names(dots_ls), dots_ls, sep = ": ", collapse = "; ") %>%
      paste0(";")
  } else {
    style_str <- ""
  }
  return(style_str)
}
