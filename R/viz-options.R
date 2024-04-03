#' Basic Table Options
#'
#' @param id Unique identifier.
#' @param digits Default number of digits to display.
#' @param digits_label Displayed label for digits input.
#' @param sigfig Logical. If \code{TRUE}, use significant digits.
#' @param sigfig_label Displayed label for significant digits checkbox.
#' @param total_width Total width of html element.
#'
#' @return List of shiny tags.
#'
#' @export
tableOptionsUI <- function(id,
                           digits = NA,
                           digits_label = "Digits",
                           sigfig = FALSE,
                           sigfig_label = "Use Significant Digits",
                           total_width = NULL) {
  ns <- shiny::NS(id)

  opts <- shiny::tagList(
    shiny::numericInput(
      inputId = ns("display_digits"),
      label = digits_label,
      value = digits,
      min = 0, max = NA, step = 1
    ),
    checkbox(
      inputId = ns("display_sigfigs"),
      label = sigfig_label,
      value = sigfig
    )
  )

  if (!is.null(total_width)) {
    opts <- shiny::tagList(shiny::column(total_width, opts))
  }
  
  return(opts)
}


#' Basic Plot Options
#'
#' @param id Unique identifier.
#' @param multicol Logical. If \code{TRUE}, use multiple columns.
#' @param heatmap Logical. If \code{TRUE}, include heatmap options.
#' @param strip_options Logical. If \code{TRUE}, include strip options.
#' @param total_width Total width of html element.
#' @param height Height of html element.
#' @param x_axis_text_size Default size of x-axis text.
#' @param y_axis_text_size Default size of y-axis text.
#' @param legend_text_size Default size of legend text.
#' @param strip_text_size Default size of strip text.
#' @param x_axis_title_size Default size of x-axis title.
#' @param y_axis_title_size Default size of y-axis title.
#' @param legend_title_size Default size of legend title.
#' @param title_size Default size of title.
#' @param axis_line_width Default width of axis lines.
#' @param x_text_angle Default angle of x-axis text. If \code{FALSE}, x-axis
#'   text is horizontal. Otherwise, it is angled 45 degrees.
#' @param strip_text_color Default color of strip text.
#' @param bg_color Default background color.
#' @param other_options Additional shiny widgets/tags to add to plot options.
#'
#' @return List of shiny tags.
#'
#' @export
plotOptionsUI <- function(id, multicol = FALSE,
                          heatmap = FALSE, strip_options = FALSE,
                          total_width = 12, height = 500,
                          x_axis_text_size = 12, y_axis_text_size = 12,
                          legend_text_size = 12, strip_text_size = 14,
                          x_axis_title_size = 14, y_axis_title_size = 14,
                          legend_title_size = 14, title_size = 16,
                          axis_line_width = 1, x_text_angle = FALSE,
                          strip_text_color = "white", bg_color = "grey98",
                          other_options = NULL) {
  ns <- shiny::NS(id)

  if (!heatmap) {
    bg_options <- shiny::tagList(
      material_switch(
        ns("display_grid"), "Show grid lines", value = TRUE
      ),
      shiny::textInput(
        ns("display_bg"), "Background Color", bg_color
      )
    )
  } else {
    bg_options <- NULL
  }

  if (strip_options) {
    strip_text_options <- shiny::tagList(
      shiny::numericInput(
        ns("display_stext_size"), "Strip Text Size", strip_text_size
      )
    )
    strip_format_options <- shiny::tagList(
      shiny::textInput(
        ns("display_stext_color"), "Strip Text Color", "white"
      ),
      shiny::textInput(
        ns("display_sbg"), "Strip Background Color", "#2c3e50"
      )
    )
  } else {
    strip_text_options <- NULL
    strip_format_options <- NULL
  }

  title_options <- shiny::tagList(
    shiny::numericInput(
      ns("display_xtitle_size"), "X-Axis Title Size", x_axis_title_size
    ),
    shiny::numericInput(
      ns("display_ytitle_size"), "Y-Axis Title Size", y_axis_title_size
    ),
    shiny::numericInput(
      ns("display_ltitle_size"), "Legend Title Size", legend_title_size
    ),
    shiny::numericInput(
      ns("display_title_size"), "Title Size", title_size
    )
  )

  text_options <- shiny::tagList(
    shiny::numericInput(
      ns("display_xtext_size"), "X-Axis Text Size", x_axis_text_size
    ),
    shiny::numericInput(
      ns("display_ytext_size"), "Y-Axis Text Size", y_axis_text_size
    ),
    shiny::numericInput(
      ns("display_ltext_size"), "Legend Text Size", legend_text_size
    )
  )

  axis_width_options <- shiny::tagList(
    shiny::numericInput(
      ns("display_awidth"), "Axis Line Width", axis_line_width
    )
  )

  axis_options <- shiny::tagList(
    checkbox_group(
      ns("display_atext"), label = "Show Axis Text",
      choices = c("x", "y"), selected = c("x", "y"), inline = TRUE
    ),
    material_switch(
      ns("display_xangle"), "Angle X-Axis Text", x_text_angle
    ),
    material_switch(
      ns("display_coord_flip"), "Flip Coordinate Axes", value = FALSE
    )
  )

  height_options <- shiny::numericInput(
    ns("display_height"), "Plot Height (px)", height
  )

  if (!multicol) {
    opts <- shiny::tagList(
      text_options,
      strip_text_options,
      title_options,
      strip_format_options,
      axis_width_options,
      axis_options,
      bg_options,
      height_options,
      other_options
    )
  } else {
    opts_col1 <- title_options
    opts_col2 <- shiny::tagList(
      text_options,
      strip_text_options,
      axis_width_options
    )
    opts_col3 <- shiny::tagList(
      axis_options,
      bg_options,
      strip_format_options,
      height_options,
      other_options
    )
    opts <- shiny::tagList(
      shiny::column(total_width / 3, opts_col1),
      shiny::column(total_width / 3, opts_col2),
      shiny::column(total_width / 3, opts_col3)
    )
  }

  return(opts)
}


#' Geom-specific options for plotting
#'
#' @param id Unique identifier.
#' @param subsample Logical. If \code{TRUE}, include subsample data options.
#' @param text_size Logical. If \code{TRUE}, include text size options.
#' @param geom Character string or vector of default geom(s) to display.
#'   If \code{NULL}, inferred from \code{input[ns("display_geom")]}.
#' @param options_list List of shiny tags of additional options to include.
#'
#' @return List of shiny tags.
#'
#' @export
plotGeomOptionsUI <- function(id,
                              subsample = FALSE, text_size = FALSE,
                              geom = NULL, options_list = NULL) {
  ns <- shiny::NS(id)
  plot_geom <- ns("display_geom")

  subsample_opts <- shiny::sliderInput(
    ns("display_subsample"), "Subsample Data", min = 0, max = 1, value = 1
  )
  text_size_opts <- shiny::numericInput(
    ns("display_text_size"), label = "Text Size", value = 3, min = 0
  )
  bin_opts <- shiny::numericInput(
    ns("display_bins"), "Number of Bins", value = 15, min = 1, max = NA
  )
  transparency_opts <- shiny::sliderInput(
    ns("display_transparency"), "Transparency", value = 0.7, min = 0, max = 1
  )
  size_opts <- shiny::numericInput(
    ns("display_point_size"), "Point Size", value = 1, min = 0
  )
  lsize_opts <- shiny::numericInput(
    ns("display_line_size"), "Line Size", value = 1, min = 0
  )
  errbar_size_opts <- shiny::tagList(
    shiny::numericInput(
      ns("display_bar_width"), "Bar Width", value = 0.5, min = 0
    ),
    shiny::numericInput(
      ns("display_bar_thickness"), "Bar Thickness", value = 0.5, min = 0
    )
  )
  bw_opts <- shiny::numericInput(
    ns("display_bw"), "Bandwidth Multiplier", value = 1, min = 0
  )

  if (!subsample) {
    subsample_opts <- NULL
  }
  if (!text_size) {
    text_size_opts <- NULL
  }

  if (is.null(geom)) {
    shiny::tagList(
      shiny::conditionalPanel(
        sprintf(
          "input['%1$s'] != 'Please select variable(s) first' && %2$s",
          plot_geom, is_null_js(plot_geom)
        ),
        subsample_opts,
        shiny::conditionalPanel(
          sprintf("input['%1$s'] == 'histogram'", plot_geom),
          bin_opts,
        ),
        shiny::conditionalPanel(
          sprintf(
            "input['%1$s'] == 'density' | input['%1$s'] == 'histogram' | input['%1$s'] == 'point' | input['%1$s'] == 'line'",
            plot_geom
          ),
          transparency_opts
        ),
        shiny::conditionalPanel(
          sprintf("input['%1$s'] == 'point'", plot_geom),
          size_opts
        ),
        shiny::conditionalPanel(
          sprintf("input['%1$s'] == 'line'", plot_geom),
          lsize_opts
        ),
        shiny::conditionalPanel(
          sprintf("input['%1$s'] == 'errorbar'", plot_geom),
          errbar_size_opts
        ),
        shiny::conditionalPanel(
          sprintf("input['%1$s'] == 'density'", plot_geom),
          bw_opts
        ),
        text_size_opts,
        options_list
      )
    )
  } else {
    if (all(geom != "histogram")) {
      bin_opts <- NULL
    }
    if (all(!(geom %in% c("density", "histogram", "point", "line")))) {
      transparency_opts <- NULL
    }
    if (all(geom != "point")) {
      size_opts <- NULL
    }
    if (all(geom != "line")) {
      lsize_opts <- NULL
    }
    if (all(geom != "errorbar")) {
      errbar_size_opts <- NULL
    }
    if (all(geom != "density")) {
      bw_opts <- NULL
    }
    shiny::tagList(
      subsample_opts, bin_opts, transparency_opts, size_opts, lsize_opts,
      errbar_size_opts, bw_opts, text_size_opts, options_list
    )
  }
}


#' Radio group button with icons
#'
#' @description Wrapper around \code{radio_group_buttons} with options to
#'   display icons and float the buttons to the right.
#'
#' @inheritParams shinyWidgets::radioGroupButtons
#' @param id Unique identifier.
#' @param choices Choices must be some subset of "ggplot", "plotly", "table"
#' @param float_right Logical. Whether or not to float the buttons to the right.
#' @param ... Additional arguments to pass to radio_group_buttons
#'
#' @keywords internal
iconRadioGroupUI <- function(id, ns_id, choices,
                             selected = NULL, individual = FALSE,
                             size = "normal", justified = FALSE,
                             float_right = FALSE, ...) {
  ns <- shiny::NS(id)

  if (float_right) {
    style <- css_styler(
      float = "right",
      `z-index` = 99,
      position = "relative",
      `margin-right` = "15px"
    )
  } else {
    style <- css_styler(
      position = "relative"
    )
  }

  out <- radio_group_buttons(
    inputId = ns(ns_id),
    label = NULL,
    choices = get_icons(choices),
    selected = selected,
    individual = individual,
    size = size,
    justified = justified,
    ...
  ) %>%
    htmltools::tagAppendAttributes(class = "btn-margin", style = style) %>%
    display_inline()

  if (float_right) {
    # hack so that plotly plot tools don't overlap with buttons
    hidden_out <- iconRadioGroupUI(
      id = paste0(id, "_hidden"),
      ns_id = ns_id,
      choices = choices,
      selected = selected,
      individual = individual,
      size = size,
      justified = justified,
      float_right = FALSE,
      ...
    ) %>%
      htmltools::tagAppendAttributes(style = css_styler(visibility = "hidden"))
    out <- shiny::tagList(out, hidden_out)
  }

  return(out)
}


#' Select visualization type (table, ggplot, plotly)
#'
#' @inheritParams iconRadioGroupUI
#'
#' @export
selectVizUI <- function(id,
                        choices = c("ggplot", "plotly", "table"),
                        selected = NULL, individual = FALSE, size = "normal",
                        justified = FALSE, float_right = FALSE, ...) {
  iconRadioGroupUI(
    id = id,
    ns_id = "display_viz",
    choices = choices,
    selected = selected,
    individual = individual,
    size = size,
    justified = justified,
    float_right = float_right,
    ...
  )
}


#' Select geom type (point, line, etc.)
#'
#' @inheritParams iconRadioGroupUI
#'
#' @export
selectGeomUI <- function(id,
                         choices = "Please select variable(s) first",
                         selected = NULL, individual = TRUE, size = "normal",
                         justified = FALSE, float_right = FALSE, ...) {
  iconRadioGroupUI(
    id = id,
    ns_id = "display_geom",
    choices = choices,
    selected = selected,
    individual = individual,
    size = size,
    justified = justified,
    float_right = float_right,
    ...
  )
}
