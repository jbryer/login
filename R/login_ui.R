#' Login UI elements.
#'
#' @param id id unique ID for the Shiny Login module.
#' @export
login_ui <- function(id) {
	div(
		# This is a hack to ensure that this is re-evaluated when the logged_in variable changes
		div(textOutput(NS(id, 'logged_in')), style = 'visibility: hidden;'),
		conditionalPanel(
			condition = paste0("output['", NS(id, 'logged_in'), "'] != 'TRUE'"),
			htmltools::attachDependencies(
				x = wellPanel(
					div(textOutput(NS(id, 'login_message')), style = 'color:red;'),
					textInput(NS(id, 'username'), label = 'Email:', value = ''),
					passwdInput(NS(id, 'password'), label = 'Password: ', value = ''),
					actionButton(NS(id, "Login"), "Login")
				),
				value = login_dependencies(),
				append = TRUE
			)
		)
	)
}
