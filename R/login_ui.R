#' Login UI elements.
#'
#' @param id id unique ID for the Shiny Login module.
#' @export
login_ui <- function(id) {
	dependencies <- cookies::cookie_dependency()
	dependencies[[length(dependencies) + 1]] <- login_dependencies()
	div(
		# This is a hack to ensure that this is re-evaluated when the logged_in variable changes
		div(textOutput(NS(id, 'logged_in')), style = 'visibility: hidden;'),
		conditionalPanel(
			condition = paste0("output['", NS(id, 'logged_in'), "'] != 'TRUE'"),
			htmltools::attachDependencies(
				x = uiOutput(NS(id, 'login_ui')),
				value = dependencies,
				append = TRUE
			)
		)
	)
}
