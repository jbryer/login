#' UI for creating a new user account.
#'
#' @param id id unique ID for the Shiny Login module.
#' @export
new_user_ui <- function(id) {
	is_not_logged_in(
		id = id,
		htmltools::attachDependencies(
			x = wellPanel(
				div(textOutput(NS(id, 'new_user_message')), style = 'color:red;'),
				textInput(NS(id, 'new_username'), label = 'Email:', value = ''),
				passwdInput(NS(id, 'new_password1'), label = 'Password: ', value = ''),
				passwdInput(NS(id, 'new_password2'), label = 'Reenter password: ', value = ''),
				actionButton(NS(id, "new_user"), "Create Account")
			),
			value = login_dependencies(),
			append = TRUE
		)
	)
}
