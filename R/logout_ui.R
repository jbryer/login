#' Logout button.
#'
#' @param id id unique ID for the Shiny Login module.
#' @param label label of the logout button.
#' @param icon icon for the logout button.
#' @param style CSS styles for the logout button.
#' @export
logout_button <- function(id,
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
