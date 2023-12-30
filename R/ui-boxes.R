#' Pretty box
#'
#' @description Wrapper around `shinydashboardPlus::box()`.
#'
#' @inheritParams shinydashboardPlus::box
#' @param color Box outline color.
#' @param box_only If `TRUE`, only return the box and not the box wrapped inside
#'   `shiny::fluidRow()`.
#' @param ... Arguments passed to `shinydashboardPlus::box()`.
#'
#' @export
prettyBox <- function(..., title = NULL, width = 12, color = NULL,
                       status = "primary", solidHeader = TRUE,
                       box_only = FALSE) {
  if (!is.null(title)) {
    title <- htmltools::tags$b(title)
  }
  if (!box_only) {
    shiny_wrapper <- shiny::fluidRow
  } else {
    shiny_wrapper <- function(x) x
  }
  if (!is.null(color)) {
    color_fun <- function(x) {
      x %>%
        htmltools::tagAppendAttributes(
          style = css_styler(border = sprintf("1px solid %s", color)),
          .cssSelector = ".box"
        ) %>%
        htmltools::tagAppendAttributes(
          style = css_styler(background = color),
          .cssSelector = ".box-header"
        )
    }
  } else {
    color_fun <- function(x) x
  }
  shiny_wrapper(
    shinydashboardPlus::box(
      ..., title = title, color = color,
      width = width, status = status, solidHeader = solidHeader
    ) %>%
      color_fun()
  )
}


#' Inner fluid row that removes negative margins
#'
#' @description Wrapper around `shiny::fluidRow()` that is useful when used
#'   inside prettyBox().
#'
#' @param ... Arguments passed to `shiny::fluidRow()`.
#'
#' @export
innerFluidRow <- function(...) {
  shiny::fluidRow(...) %>% remove_margins()
}
