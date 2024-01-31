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
				x = uiOutput(NS(id, 'login_ui')),
				value = login_dependencies(),
				append = TRUE
			)
		)
	)
}

#' Alternate login interface. This does not check to see if the user is already
#' logged in. This is suitable for putting the UI in the server.
#'
# login_ui_output <- function(id) {
# 	uiOutput(NS(id, 'login_ui'))
# }

# login_ui <- function(id,
# 					 label = 'Login',
# 					 icon = shiny::icon('right-to-bracket')) {
# 	is_not_logged_in(
# 		id = id,
# 		wellPanel(
# 			div(textOutput(NS(id, 'login_message')), style = 'color:red;'),
# 			textInput(NS(id, 'username'), label = 'Email:', value = ''),
# 			passwdInput(NS(id, 'password'), label = 'Password: ', value = ''),
# 			actionButton(NS(id, "Login"),
# 						 label = label,
# 						 icon = icon)
# 		)
# 	)
# }
