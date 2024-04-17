#' UI for creating a new user account.
#'
#' This will render the UI for users to create an account.
#'
#' @param id id unique ID for the Shiny Login module.
#' @export
new_user_ui <- function(id) {
	is_not_logged_in(
		id = id,
		htmltools::attachDependencies(
			x = uiOutput(NS(id, 'new_user_ui')),
			value = use_login(),
			append = TRUE
		)
	)
}
