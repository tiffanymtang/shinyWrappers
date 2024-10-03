#' Add a tooltip icon to a label
#'
#' @param ... Arguments passed to span element (e.g., the label to add the
#'   tooltip to).
#' @param id The id of the tooltip.
#' @param icon The icon to use for the tooltip.
#'
#' @return The label with the tooltip icon.
#'
#' @export
add_tooltip_icon <- function(..., id, icon = shiny::icon("info-circle")) {
  ns <- shiny::NS(id)
  htmltools::span(
    ...,
    htmltools::span(icon, id = ns("tooltip"))
  )
}

#' Add a tooltip to an element
#'
#' @param id The id of the element to add the tooltip to.
#' @param tooltip The tooltip to add.
#' @param placement The placement of the tooltip.
#' @param allowHTML Whether to allow HTML in the tooltip.
#' @param use_id_only Whether to use the id only. Default is \code{FALSE} which
#'   will add the namespace 'tooltip-icon' to the id. Set to \code{TRUE} if the
#'   tooltip icon was not added with \code{\link{add_tooltip_icon}} (e.g., if
#'   the tooltip icon was added manually).
#' @param ... Arguments passed to \code{\link{tippy_this}}.
#'
#' @return The element with the tooltip.
#'
#' @export
add_tooltip <- function(id, tooltip, placement = "right", allowHTML = TRUE,
                        use_id_only = FALSE, ...) {
  if (!use_id_only) {
    ns <- shiny::NS(id)
    id <- ns("tooltip")
  }
  tippy::tippy_this(
    id, tooltip, placement = placement, allowHTML = allowHTML, ...
  )
}


#' Format tooltips
#'
#' @param tooltip The tooltip to add.
#' @param size Font size
#'
#' @return Formatted tooltip
#'
#' @export
format_tooltip <- function(tooltip, size = 14) {
  sprintf("<span style='font-size: %spx;'>%s</span>", size, tooltip)
}
