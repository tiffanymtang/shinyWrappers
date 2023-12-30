#' Use pretty css style
#'
#' @export
use_pretty_style <- function() {

  shiny::addResourcePath(
    prefix = "www",
    directoryPath = system.file("www", package = packageName())
  )

  shiny::tagList(
    htmltools::includeCSS(
      system.file(file.path("www", "pretty_style.css"), package = packageName())
    ),
    fresh::use_pretty(
      system.file(file.path("www", "pretty_fresh.css"), package = packageName())
    )
  )
}


#' Display inline block
#'
#' @param obj Object to display inline.
#'
#' @export
display_inline <- function(obj) {
  obj %>% htmltools::tagAppendAttributes(style = "display: inline-block;")
}


#' Remove margins
#'
#' @param obj Object to remove margins from.
#'
#' @export
remove_margins <- function(obj) {
  obj %>% htmltools::tagAppendAttributes(style = "margin: 0px;")
}


#' Set margins
#'
#' @param obj Object to set margins.
#' @param top Top margin.
#' @param bottom Bottom margin.
#' @param left Left margin.
#' @param right Right margin.
#'
#' @export
set_margins <- function(obj,
                        top = NULL, bottom = NULL, left = NULL, right = NULL) {
  style <- css_styler(
    `margin-top` = top,
    `margin-bottom` = bottom,
    `margin-left` = left,
    `margin-right` = right
  )
  obj %>%
    htmltools::tagAppendAttributes(style = style)
}


#' Add spinner when loading
#'
#' @description Wrapper around `shinycssloaders::withSpinner()` with a different
#'   default color.
#'
#' @inheritParams shinycssloaders::withSpinner
#' @param obj Object to add spinner to.
#'
#' @examples
#' ## DON'T RUN
#' # shiny::htmlOutput({NAME OF UI ELEMENT}) %>% add_spinner()
#'
#' @export
add_spinner <- function(obj, color = "#18bc9c") {
  obj %>% shinycssloaders::withSpinner(color = "#18bc9c")
}


#' Short horizontal rule
#'
#' @param margin_left Left margin.
#' @param margin_right Right margin.
#' @param color Color of line.
#'
#' @export
hr_short <- function(margin_left = "105px", margin_right = margin_left,
                     color = "lightblue") {
  htmltools::hr() %>%
    htmltools::tagAppendAttributes(
      style = paste(
        sprintf("margin-left: %s", margin_left),
        sprintf("margin-right: %s", margin_right),
        sprintf("border-color: %s", color), "",
        sep = "; "
      )
    )
}


#' Add vertical space
#'
#' @param size Size of vertical space.
#'
#' @export
vspace <- function(size = "3px") {
  htmltools::tags$div(htmltools::tags$p("&nbsp;")) %>%
    htmltools::tagAppendAttributes(
      style = css_styler(`font-size` = size, color = "transparent")
    )
}


#' Add border class
#'
#' @param obj Object to add border to.
#'
#' @export
add_border <- function(obj) {
  obj %>% htmltools::tagAppendAttributes(class = "box-border")
}
