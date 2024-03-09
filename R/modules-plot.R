#' Basic plot module
#'
#' @name plotModule
#' @description Shiny module (UI and server-side functions) to display plot (
#'   ggplot2 or plotly) in a uniform container.
#'
#' @param id Unique identifier.
#' @param border Add border to plot.
#' @param spinner Add spinner to plot.
#' @param plot_fun (Reactive) function to generate plot.
#' @param plot_options Logical. Whether to update plot using default plotting
#'   options set using `plotOptionsUI(id)`. Only set to \code{TRUE} if
#'   `plotOptionsUI(id)` is called in the UI.
#' @param modes Character vector. Plot modes to display. Options are
#'   \code{"ggplot"}, \code{"plotly"}, and \code{"table"}, but note that if
#'   \code{"table"} is used, then \code{tableServer()} must have been also
#'   called in the server with the same \code{id}.
#' @param error_msg Error message to display if specified mode is not
#'   supported.
#'
#' @keywords internal
NULL

#' Basic plot module - UI
#' @rdname plotModule
#' @export
plotUI <- function(id, border = FALSE, spinner = FALSE) {
  ns <- shiny::NS(id)
  out <- shiny::uiOutput(ns("plot")) %>%
    add_spinner(spinner) %>%
    add_border(border)
  return(out)
}


#' Basic plot module - Server
#' @rdname plotModule
#' @export
plotServer <- function(id, plot_fun, plot_options = TRUE,
                       modes = c("ggplot", "plotly"),
                       border = FALSE, spinner = TRUE,
                       error_msg = "") {
  shiny::moduleServer(id, function(input, output, session) {

    make_plot <- shiny::reactive({
      plot <- plot_fun()
      if (!plot_options) {
        return(plot)
      }

      if (!("ggplot" %in% class(plot))) {
        return(plot)
      }

      if (input$display_xangle) {
        x_angle <- 45
        x_hjust <- 1
      } else {
        x_angle <- 0
        x_hjust <- 0.5
      }

      plot <- plot +
        ggplot2::theme(
          axis.title.x = ggplot2::element_text(
            size = input$display_xtitle_size, face = "bold"
          ),
          axis.title.y = ggplot2::element_text(
            size = input$display_ytitle_size, face = "bold"
          ),
          legend.title = ggplot2::element_text(
            size = input$display_ltitle_size, face = "bold"
          ),
          plot.title = ggplot2::element_text(
            size = input$display_title_size, face = "bold"
          ),
          axis.text.x = ggplot2::element_text(
            size = input$display_xtext_size, angle = x_angle, hjust = x_hjust
          ),
          axis.text.y = ggplot2::element_text(size = input$display_ytext_size),
          legend.text = ggplot2::element_text(size = input$display_ltext_size),
          axis.line = ggplot2::element_line(
            linewidth = input$display_awidth, color = "black"
          )
        )
      if (!is.null(input$display_bg)) {
        plot <- plot +
          ggplot2::theme(
            panel.background = ggplot2::element_rect(fill = input$display_bg),
            legend.key = ggplot2::element_rect(fill = input$display_bg)
          )
        if (!input$display_grid) {
          plot <- plot +
            ggplot2::theme(panel.grid = ggplot2::element_blank())
        }
      }
      if (!is.null(input$display_sbg)) {
        plot <- plot + ggplot2::theme(
          strip.background = ggplot2::element_rect(
            fill = input$display_sbg, color = input$display_sbg
          ),
          strip.text = ggplot2::element_text(
            size = input$display_stext_size,
            color = input$display_stext_color,
            face = "bold"
          )
        )
      }

      if (!("x" %in% input$display_atext)) {
        plot <- plot +
          ggplot2::theme(
            axis.text.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank()
          )
      }
      if (!("y" %in% input$display_atext)) {
        plot <- plot +
          ggplot2::theme(
            axis.text.y = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank()
          )
      }
      if (input$display_coord_flip) {
        plot <- plot + ggplot2::coord_flip()
      }

      return(plot)
    })

    plot_height <- shiny::reactive({
      if (is.null(input$display_height)) {
        return(500)
      } else {
        return(input$display_height)
      }
    })

    if ("plotly" %in% modes) {
      output$plotly <- plotly::renderPlotly({
        plt <- make_plot()
        plotly::ggplotly(plt, height = plot_height())
      })
    }
    if ("ggplot" %in% modes) {
      output$ggplot <- shiny::renderPlot({
        make_plot()
      }, height = function() height = plot_height())
    }
    output$plot_error <- shiny::renderText({error_msg})

    output$plot <- shiny::renderUI({
      if (!is.null(input$display_viz)) {
        mode <- input$display_viz
      } else {
        mode <- modes[1]
      }
      if ((mode == "ggplot") && (mode %in% modes)) {
        out <- shiny::plotOutput(session$ns("ggplot"), height = "auto")
      } else if ((mode == "plotly") && (mode %in% modes)) {
        out <- plotly::plotlyOutput(session$ns("plotly"), height = "100%")
      } else if ((mode == "table") && (mode %in% modes)) {
        out <- shiny::uiOutput(session$ns("table"))
      } else {
        out <- shiny::htmlOutput(session$ns("plot_error"))
      }
      out <- out %>%
        add_spinner(spinner) %>%
        add_border(border)
      return(out)
    })
  })
}
