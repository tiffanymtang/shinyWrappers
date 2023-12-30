#' File input module
#'
#' @name fileInputModule
#' @description Shiny module (UI and server-side functions) to upload a file.
#'
#' @inheritParams shiny::fileInput
#' @param id Unique identifier.
#' @param tooltip Tooltip to display.
#'
#' @keywords internal
NULL


#' File input module - UI
#' @rdname fileInputModule
#' @export
fileInputUI <- function(id,
                        label = "File Upload",
                        accept = c(".csv", ".txt", ".rds"),
                        show_accept = TRUE,
                        tooltip = NULL) {
  ns <- shiny::NS(id)
  indent_size <- "1.25em"

  if (show_accept) {
    label <- sprintf("%s (%s)", label, paste(accept, collapse = ", "))
  }
  if (!is.null(tooltip)) {
    label <- label %>% add_tooltip_icon(id = ns("file"))
  }

  shiny::tagList(
    # file input
    shiny::fileInput(
      inputId = ns("file"),
      label = label,
      multiple = FALSE,
      accept = accept
    ),
    add_tooltip(ns("file"), tooltip),
    # file input options
    shiny::conditionalPanel(
      condition = sprintf(
        "output['%1$s'] == true && output['%2$s'] !== '.rds'",
        ns("fileuploaded"), ns("filetype")
      ),
      # options for .txt file
      shiny::conditionalPanel(
        condition = sprintf("output['%1$s'] == '.txt'", ns("filetype")),
        radio_buttons(
          inputId = ns("sep"),
          label = htmltools::tags$i("Separator"),
          choices = c("Comma" = ",", "Semicolon" = ";", "Tab" = "\t"),
          inline = TRUE
        )
      ),
      # options for .csv/.txt file
      shiny::conditionalPanel(
        condition = sprintf(
          "output['%1$s'] == '.csv' | output['%1$s'] == '.txt'", ns("filetype")
        ),
        material_switch(
          inputId = ns("header"),
          label = htmltools::tags$i("Header"),
          value = TRUE,
          status = "primary"
        )
      )
    ) %>%
      htmltools::tagAppendAttributes(
        style = css_styler(
          `margin-left` = indent_size,
          `margin-top` = "-0.75em"
        )
      )
  )
}

#' File input module - Server
#' @rdname fileInputModule
#' @export
fileInputServer <- function(id, default_data = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    output$filetype <- shiny::reactive({
      stringr::str_extract(input$file$datapath, "(\\.[^.]*)$")
    })
    shiny::outputOptions(output, "filetype", suspendWhenHidden = FALSE)

    output$fileuploaded <- reactive({
      length(stringr::str_extract(input$file$datapath, "(\\.[^.]*)$")) > 0
    })
    outputOptions(output, "fileuploaded", suspendWhenHidden = FALSE)

    reactive({
      file_type <- stringr::str_extract(input$file$datapath, "(\\.[^.]*)$")
      if (identical(file_type, character(0))) {
        file_type <- "default"
      }
      switch(
        file_type,
        "default" = default_data,
        ".rds" = readRDS(input$file$datapath),
        ".csv" = read.csv(input$file$datapath, header = input$header),
        ".txt" = read.table(input$file$datapath, header = input$header,
                            sep = input$sep)
      )
    })
  })
}
