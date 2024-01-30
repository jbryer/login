#' Logout button.
#'
#' @param id id unique ID for the Shiny Login module.
#' @export
logout_ui <- function(id,
					  label = 'Logout',
					  icon = shiny::icon('right-from-bracket'),
					  style = "" #"position: absolute; right: 20px; top: 10px"
) {
	is_logged_in(
		id = id,
		actionButton(NS(id, 'logout'),
					 label = label,
					 icon = icon,
					 style = style)
	)
}
