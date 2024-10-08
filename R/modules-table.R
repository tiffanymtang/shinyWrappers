#' Basic table module
#'
#' @name tableModule
#' @description Shiny module (UI and server-side functions) to display table
#'   (kable or DT) in a uniform container.
#'
#' @inheritParams plotModule
#' @param id Unique identifier.
#' @param table_fun (Reactive) function to generate table.
#' @param table_options Logical. Whether to update table using default table
#'   options set using `tableOptionsUI(id)`. Only set to \code{TRUE} if
#'   `tableOptionsUI(id)` is called in the UI.
#' @param modes Character string. Table mode to display. Options are
#'   \code{"DT"} and \code{"kable"}.
#' @param caption Table caption.
#' @param ... Arguments to pass to `vthemes::pretty_DT()` or
#'   `vthemes::pretty_kable()`
#'
#' @keywords internal
NULL


#' Basic table module - UI
#' @rdname tableModule
#' @export
tableUI <- function(id, border = FALSE, spinner = FALSE) {
  ns <- shiny::NS(id)
  out <- shiny::uiOutput(ns("table")) %>%
    add_spinner(spinner) %>%
    add_border(border)
  return(out)
}


#' Basic table module - Server
#' @rdname tableModule
#' @export
tableServer <- function(id, table_fun, table_options = TRUE,
                        mode = c("DT", "kable"), caption = NULL,
                        border = FALSE, spinner = TRUE,
                        error_msg = "", ...) {
  mode <- match.arg(mode)

  shiny::moduleServer(id, function(input, output, session) {

    make_table <- shiny::reactive({
      tab <- table_fun()

      if (!is.null(caption)) {
        if (is.reactive(caption)) {
          caption <- caption()
        }
      }

      if (!table_options) {
        digits <- NULL
        sigfig <- FALSE
      } else {
        digits <- input$display_digits
        if (isTRUE(is.na(digits))) {
          digits <- NULL
        }
        sigfig <- input$display_sigfigs
        if (is.null(sigfig)) {
          sigfig <- FALSE
        }
      }

      if (mode == "DT") {
        generate_table <- vthemes::pretty_DT
      } else if (mode == "kable") {
        generate_table <- vthemes::pretty_kable
      }
      out <- generate_table(
        tab, digits = digits, sigfig = sigfig, caption = caption, ...
      )
    })

    if (mode == "DT") {
      output$dt <- DT::renderDT({make_table()})
    } else if (mode == "kable") {
      output$kable <- shiny::renderText({make_table()})
    }
    output$table_error <- shiny::renderText({error_msg})

    output$table <- shiny::renderUI({
      if (mode == "DT") {
        shiny::fluidPage(
          DT::DTOutput(session$ns("dt")) %>%
            add_spinner(spinner) %>%
            add_border(border),
          vspace()
        )
      } else if (mode == "kable") {
        shiny::fluidPage(
          shiny::htmlOutput(session$ns("kable")) %>%
            add_spinner(spinner) %>%
            add_border(border)
        )
      } else {
        shiny::htmlOutput(session$ns("table_error"))
      }
    })
  })
}
