#' Use pretty css style
#'
#' @export
use_pretty_style <- function() {

  shiny::addResourcePath(
    prefix = "www",
    directoryPath = system.file("www", package = packageName())
  )

  my_theme <- fresh::create_theme(
    fresh::adminlte_color(light_blue = "#35a4bf"),
    fresh::adminlte_sidebar(width = "315px", dark_color = "#2c3b41"),
    fresh::adminlte_global(content_bg = "whitesmoke")
  )

  shiny::tagList(
    htmltools::includeCSS(
      system.file(file.path("www", "pretty_style.css"), package = packageName())
    ),
    fresh::use_pretty(
      system.file(file.path("www", "pretty_fresh.css"), package = packageName())
    ),
    fresh::use_theme(my_theme)
  )
}


#' Pretty dashboard header
#'
#' @description Wrapper around `shinydashboard::dashboardHeader()` that allows
#'  for the title to be positioned to the left or center.
#'
#' @inheritParams shinydashboard::dashboardHeader
#' @param title_position Position of the title, either "left" or "center".
#' @param title_style (Optional) list of CSS styling options for the title.
#' @param ... Arguments passed to `shinydashboard::dashboardHeader()`.
#'
#' @export
prettyDashboardHeader <- function(title, title_position = c("left", "center"),
                                  title_style = list(), ...) {

  title_position <- match.arg(title_position)
  header <- shinydashboard::dashboardHeader(title = title, ...)

  if (title_position == "left") {
    style_ls <- list(
      `font-weight` = "bold"
    )
  } else if (title_position == "center") {
    style_ls <- list(
      display = "block",
      height = "50px",
      `font-size` = "20px",
      `line-height` = "50px",
      `text-align` = "center",
      `font-family` = "Helvetica Neue, Helvetica, Arial, sans-serif",
      `font-weight` = "bold",
      color = "white"
    )
  }
  if (!is.null(title_style)) {
    for (arg in names(title_style)) {
      style_ls[[arg]] <- title_style[[arg]]
    }
  }
  style <- do.call(css_styler, style_ls)

  if (title_position == "left") {
    header <- header %>%
      htmltools::tagAppendAttributes(style = style, .cssSelector = ".logo")
  } else if (title_position == "center") {
    header <- header %>%
      htmltools::tagAppendAttributes(
        style = css_styler(`margin-left` = "0px"),
        .cssSelector = ".navbar.navbar-static-top"
      ) %>%
      htmltools::tagAppendAttributes(
        style = css_styler(position = "absolute", left = "0px"),
        .cssSelector = ".sidebar-toggle"
      )

    title_tag_idx <- 2
    title_tag <- header$children[[title_tag_idx]] %>%
      htmltools::tagAppendAttributes(style = style)
    title_tag$attribs$class <- NULL
    header$children[[title_tag_idx]] <- NULL
    header$children[[title_tag_idx]] <- htmltools::tagAppendChild(
      header$children[[title_tag_idx]], title_tag
    )
  }

  return(header)
}


#' Display inline block
#'
#' @param obj Object to display inline.
#'
#' @export
display_inline <- function(obj) {
  obj %>%
    htmltools::tagAppendAttributes(style = css_styler(display = "inline-block"))
}


#' Remove margins
#'
#' @param obj Object to remove margins from.
#'
#' @export
remove_margins <- function(obj) {
  obj %>%
    htmltools::tagAppendAttributes(style = css_styler(margin = "0px"))
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
  obj %>%
    htmltools::tagAppendAttributes(
      style = css_styler(
        `margin-top` = top,
        `margin-bottom` = bottom,
        `margin-left` = left,
        `margin-right` = right
      )
    )
}


#' Add spinner when loading
#'
#' @description Wrapper around `shinycssloaders::withSpinner()` with a different
#'   default color.
#'
#' @inheritParams shinycssloaders::withSpinner
#' @param obj Object to add spinner to.
#' @param spinner Logical. Whether or not to add spinner.
#'
#' @examples
#' ## DON'T RUN
#' # shiny::htmlOutput({NAME OF UI ELEMENT}) %>% add_spinner()
#'
#' @export
add_spinner <- function(obj, spinner = TRUE, color = "#18bc9c") {
  if (spinner) {
    obj %>% shinycssloaders::withSpinner(color = color)
  } else {
    obj
  }
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
      style = css_styler(
        `margin-left` = margin_left,
        `margin-right` = margin_right,
        `border-color` = color
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
#' @param border Logical. Whether or not to add border.
#'
#' @export
add_border <- function(obj, border = TRUE) {
  if (border) {
    obj %>% htmltools::tagAppendAttributes(class = "box-border")
  } else {
    obj
  }
}
