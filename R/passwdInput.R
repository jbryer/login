#' Password input textbox
#'
#' @param inputId ID for the input.
#' @param label label for the textbox.
#' @param value default value.
#' @export
passwdInput <- function(inputId, label, value) {
	tagList(
		tags$label(label),
		tags$input(id = inputId, type = "password", value = value, class = 'form-control')
	)
}
