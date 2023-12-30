#' Basic text box module
#'
#' @name textBoxModule
#' @description Shiny module (UI and server-side functions) to display text.
#'
#' @param id Unique identifier.
#'
#' @keywords internal
NULL


#' Basic text box module - UI
#' @rdname textBoxModule
#' @export
textboxUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::htmlOutput(ns("text"))
}


#' Basic text box module - server
#' @rdname textBoxModule
#' @export
textboxServer <- function(id, text) {
  shiny::moduleServer(id, function(input, output, session) {
    output$text <- shiny::renderText({text})
  })
}
