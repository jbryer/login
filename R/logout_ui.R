#' Logout button.
#'
#' Render a button for the user to logout.
#'
#' @param id id unique ID for the Shiny Login module.
#' @param label label of the logout button.
#' @param icon icon for the logout button.
#' @param style CSS styles for the logout button.
#' @param check_login if TRUE this will call [login::is_logged_in()].
#' @return a [shiny::actionButton()] if the user is logged in.
#' @export
logout_button <- function(id,
					  label = 'Logout',
					  icon = shiny::icon('right-from-bracket'),
					  style = "", #"position: absolute; right: 20px; top: 10px",
					  check_login = TRUE
) {
	# dependencies <- cookies::cookie_dependency()
	# dependencies[[length(dependencies) + 1]] <- use_login()
	# div(
	# 	# This is a hack to ensure that this is re-evaluated when the logged_in variable changes
	# 	div(textOutput(NS(id, 'logged_in')), style = 'visibility: hidden;'),
	# 	conditionalPanel(
	# 		condition = paste0("output['", NS(id, 'logged_in'), "'] != 'TRUE'"),
	# 		htmltools::attachDependencies(
	# 			x = actionButton(NS(id, 'logout'),
	# 							 label = label,
	# 							 icon = icon,
	# 							 style = style),
	# 			value = dependencies,
	# 			append = FALSE
	# 		)
	# 	)
	# )

	if(check_login) {
		is_logged_in(
			id = id,
			actionButton(NS(id, 'logout'),
						 label = label,
						 icon = icon,
						 style = style)
		)
	} else {
		actionButton(NS(id, 'logout'),
					 label = label,
					 icon = icon,
					 style = style)
	}
}
