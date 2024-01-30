#' UI for resetting password.
#'
#' @param id id unique ID for the Shiny Login module.
#' @export
reset_password_ui <- function(id) {
	is_not_logged_in(
		id = id,
		htmltools::attachDependencies(
			x = wellPanel(
				uiOutput(NS(id, 'reset_password_ui'))
			),
			value = login_dependencies(),
			append = TRUE
		)
	)
}
