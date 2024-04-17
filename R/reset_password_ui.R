#' UI for resetting password.
#'
#' Displays UI for users to reset their password. In order for the password
#' reset feature to work credentials to a SMTP server must be passed to the
#' [login::login_server()] function.
#'
#' @param id id unique ID for the Shiny Login module.
#' @export
reset_password_ui <- function(id) {
	# is_not_logged_in(
	# 	id = id,
		htmltools::attachDependencies(
			x = uiOutput(NS(id, 'reset_password_ui')),
			value = login_dependencies(),
			append = TRUE
		)
	# )
}
