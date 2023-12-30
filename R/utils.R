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
#'
#' @return Named character vector of html snippets to display icons
#'
#' @export
get_icons <- function(icon_names, width = 20, height = 20) {

  icons_vec <- c(
    ggplot = get_icon("ggplot.png", width = width, height = height),
    plotly = get_icon("plotly.svg", width = width, height = height),
    table = get_icon("table.svg", width = width, height = height),
    histogram = get_icon("chart-column.svg", width = width, height = height),
    density = get_icon("chart-density.svg", width = width, height = height),
    boxplot = get_icon("chart-boxplot.svg", width = width, height = height),
    point = get_icon("chart-scatter.svg", width = width, height = height),
    line = get_icon("chart-line.svg", width = width, height = height)
  )
  icon_names <- match.arg(
    icon_names, choices = names(icons_vec), several.ok = TRUE
  )

  # return(setNames(icons_vec[icon_names], NULL))
  return(setNames(icon_names, icons_vec[icon_names]))
}


#' Get html snippet for built-in icons.
#'
#' @param icon_name Name of built-in icon.
#' @param width Width of icon in pixels.
#' @param height Height of icon in pixels.
#'
#' @return Character vector of icon path.
#'
#' @export
get_icon <- function(icon_name, width = 20, height = 20) {
  path <- system.file(file.path("www", icon_name), package = packageName())
  sprintf(
    "<img src='%s' width=%spx height=%spx></img>", path, width, height
  )
}
