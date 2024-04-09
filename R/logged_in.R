#' Display Shiny elements only if the user is logged in.
#'
#' This function can be used on the Shiny UI side. It will check to see if the user
#' is logged in, if so the other Shiny elements will be displayed.
#'
#' @param id id unique ID for the Shiny Login module.
#' @param ... Shiny UI elements.
#' @return a [@shiny::conditionalPanel()]
#' @export
is_logged_in <- function(id, ...) {
	# # This is a hack to ensure that this is re-evaluated when the logged_in variable changes
	div(textOutput(NS(id, 'logged_in')), style = 'visibility: hidden;')
	conditionalPanel(
		condition = paste0("output['", NS(id, 'logged_in'), "'] == 'TRUE'"),
		...
	)
}

#' Display Shiny elements only if the user is not logged in.
#'
#' This function can be used on teh Shiny UI side. It will. check to see if the
#' user is not logged in, if so the other Shiny elements will be displayed.
#'
#' @param id id unique ID for the Shiny Login module.
#' @param ... Shiny UI elements.
#' @return a [@shiny::conditionalPanel()]
#' @export
is_not_logged_in <- function(id, ...) {
	# # This is a hack to ensure that this is re-evaluated when the logged_in variable changes
	div(textOutput(NS(id, 'logged_in')), style = 'visibility: hidden;')
	conditionalPanel(
		condition = paste0("output['", NS(id, 'logged_in'), "'] == 'FALSE'"),
		...
	)
}
