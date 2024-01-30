#' Password input textbox.
#'
#' This is an extension to Shiny's built in passwordInput by encrpting the
#' password client side before sending it to the server. Although it is encrypted
#' in the client using JavaScript it highly recommend that you also use an SSL
#' certificate (for https) as well.
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
