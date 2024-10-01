#' Reset all inputs
#'
#' @description Reset all inputs to their default values. Note that
#'   `shinyjs::useShinyjs()` needs to be includes in the UI for this to work.
#'
#' @param input Shiny input.
#' @param session Shiny session.
#' @param reset_input_ids A character vector of input IDs to reset. If NULL,
#'   all inputs will be reset.
#'
#' @export
reset_inputs <- function(input, session, reset_input_ids = NULL) {
  shiny::observeEvent(input$reset_input, {
    shinyWidgets::confirmSweetAlert(
      session, inputId = "confirm_reset",
      title = "Are you sure you want to reset all inputs?",
      type = "info",
      btn_labels = c("No", "Yes"),
      btn_colors = c("#FE642E", "#04B404"),
      showCloseButton = TRUE
    )
  })
  shiny::observeEvent(input$confirm_reset, {
    if (isTRUE(input$confirm_reset)) {
      if (is.null(reset_input_ids)) {
        shinyjs::refresh()
      } else {
        for (input_id in reset_input_ids) {
          shinyjs::reset(input_id)
        }
      }
    }
  }, ignoreNULL = TRUE)
}
