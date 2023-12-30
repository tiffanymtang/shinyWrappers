#' Wrapper around `prettyRadioButtons()`
#'
#' @description Wrapper around `prettyRadioButtons()` but with different
#'   default settings.
#'
#' @inheritParams shinyWidgets::prettyRadioButtons
#' @param ... Additional arguments to pass to
#'   `shinyWidgets::prettyRadioButtons()`.
#'
#' @export
radio_buttons <- function(inputId, label, choices, selected = NULL,
                          status = "primary", animation = "jelly",
                          icon = shiny::icon("check"), bigger = TRUE, ...) {
  shinyWidgets::prettyRadioButtons(
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected,
    status = status,
    animation = animation,
    icon = icon,
    bigger = bigger,
    ...
  )
}


#' Wrapper around `radioGroupButtons()`
#'
#' @description Wrapper around `radioGroupButtons()` but with different
#'   default settings.
#'
#' @inheritParams shinyWidgets::radioGroupButtons
#' @param ... Additional arguments to pass to
#'   `shinyWidgets::radioGroupButtons()`.
#'
#' @export
radio_group_buttons <- function(inputId, label, choices, selected = NULL,
                                status = "default", size = "sm",
                                justified = FALSE, ...) {
  shinyWidgets::radioGroupButtons(
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected,
    status = status,
    size = size,
    justified = justified,
    ...
  )
}


#' Wrapper around `pickerInput()` to select one choice
#'
#' @description Wrapper around `pickerInput()` to select one choice but with
#'   different default settings.
#'
#' @inheritParams shinyWidgets::pickerInput
#' @param ... Additional arguments to pass to `shinyWidgets::pickerInput()`.
#'
#' @export
picker_input <- function(inputId, label, choices, selected = NULL,
                         options = NULL, ...) {
  if (is.null(options)) {
    options <- list(
      `live-search` = TRUE,
      size = 5,
      title = "Nothing selected"
    )
  }
  shinyWidgets::pickerInput(
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected,
    options = options,
    ...
  )
}


#' Wrapper around `pickerInput()` to select multiple choices
#'
#' @description Wrapper around `pickerInput()` to select multiple choices but
#'   with different default settings.
#'
#' @inheritParams shinyWidgets::pickerInput
#' @param actionsBox Logical. Whether or not to include actions box.
#' @param maxOptions Numeric. Max number of options that can be selected.
#' @param ... Additional arguments to pass to `shinyWidgets::pickerInput()`.
#'
#' @export
picker_multiple_input <- function(inputId, label, choices, selected = NULL,
                                  actionsBox = TRUE, maxOptions = NULL,
                                  options = NULL, ...) {

  if (is.null(options)) {
    options <- list(
      `live-search` = TRUE,
      size = 5,
      `selected-text-format` = "count > 3",
      title = "Nothing selected",
      multipleSeparator = ", ",
      `actions-box` = actionsBox,
      `max-options` = maxOptions
    )
  }

  shinyWidgets::pickerInput(
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected,
    multiple = TRUE,
    options = options,
    ...
  )
}


#' Wrapper around `prettyCheckbox()`
#'
#' @description Wrapper around `prettyCheckbox()` with different default
#'   settings.
#'
#' @inheritParams shinyWidgets::prettyCheckbox
#' @param ... Additional arguments to pass to `shinyWidgets::prettyCheckbox()`.
#'
#' @export
checkbox <- function(inputId, label, value = FALSE,
                     status = "primary", animation = "jelly",
                     icon = shiny::icon("check"), bigger = TRUE, ...) {
  shinyWidgets::prettyCheckbox(
    inputId = inputId,
    label = label,
    value = value,
    status = status,
    animation = animation,
    icon = icon,
    bigger = bigger,
    ...
  )
}


#' Wrapper around `prettyCheckboxGroup()`
#'
#' @description Wrapper around `prettyCheckboxGroup()` with different default
#'   settings.
#'
#' @inheritParams shinyWidgets::prettyCheckboxGroup
#' @param ... Additional arguments to pass to
#'   `shinyWidgets::prettyCheckboxGroup()`.
#'
#' @export
checkbox_group <- function(inputId, label, choices, selected = NULL,
                           status = "primary", animation = "jelly",
                           icon = shiny::icon("check"), bigger = TRUE, ...) {
  shinyWidgets::prettyCheckboxGroup(
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected,
    status = status,
    animation = animation,
    icon = icon,
    bigger = bigger,
    ...
  )
}


#' Wrapper around `materialSwitch()`
#'
#' @description Wrapper around `materialSwitch()` with different default
#'   settings.
#'
#' @inheritParams shinyWidgets::materialSwitch
#' @param ... Additional arguments to pass to `shinyWidgets::materialSwitch()`.
#'
#' @export
material_switch <- function(inputId, label, value = TRUE, status = "primary",
                            right = FALSE, inline = FALSE, ...) {
  shinyWidgets::materialSwitch(
    inputId = inputId,
    label = htmltools::tags$b(label),
    value = value,
    status = status,
    inline = inline,
    right = right,
    ...
  )
}


#' Wrapper around `dropdown()`
#'
#' @description Wrapper around `dropdown()` with different default settings
#'
#' @inheritParams shinyWidgets::dropdown
#'
#' @export
options_dropdown <- function(...,
                             status = "primary", size = "sm",
                             icon = shiny::icon("cog"), width = "300px",
                             right = TRUE, style = "material-circle",
                             tooltip = FALSE) {
  shinyWidgets::dropdown(
    ...,
    status = status, size = size, icon = icon, width = width, style = style,
    tooltip = tooltip, right = right
  ) %>%
    htmltools::tagAppendAttributes(class = "options-dropdown")
}


#' Action button
#'
#' @description Wrapper around `actionButton` with different defaults
#'
#' @inheritParams shinyWidgets::actionBttn
#' @param br Logical indicating whether or not to add breaks above and below
#'    button.
#' @param color Color of button.
#' @param ... Additional arguments to pass to `shinyWidgets::actionBttn(()`.
#'
#' @export
action_button <- function(inputId, label, br = TRUE,
                          style = "material-circle", color = "danger", ...) {
  btn <- shinyWidgets::actionBttn(
    inputId = inputId, label = label, style = style, ...
  ) %>%
    htmltools::tagAppendAttributes(
      style = sprintf("background-color: %s;", color)
    ) %>%
    display_inline()

  if (br) {
    return(shiny::tagList(htmltools::br(), btn, htmltools::br()))
  } else {
    return(btn)
  }
}


#' Reset button
#'
#' @description Customized action button using `actionButton()`.
#'
#' @inheritParams shiny::actionButton
#' @param ... Additional arguments to pass to `shiny::actionButton()`.
#'
#' @export
reset_button <- function(inputId = "reset_input", label = "Reset all inputs",
                         ...) {
  htmltools::div(
    style = "display: inline-block; width: 48%; text-align: center;",
    shiny::actionButton(inputId = inputId, label = label, ...)
  )
}


#' Continue button
#'
#' @description Customized action button using `actionButton()`.
#'
#' @inheritParams shiny::actionButton
#' @param ... Additional arguments to pass to `shiny::actionButton()`.
#'
#' @export
continue_button <- function(inputId = "continue", label = "Next stage", ...) {
  htmltools::div(
    style = "display: inline-block; width: 48%; text-align: center;",
    shiny::actionButton(inputId = inputId, label = label, ...)
  )
}

