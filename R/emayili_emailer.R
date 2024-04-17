#' Returns a function to send emails using the `emayili` package.
#'
#' This returns a function that can be used with the [login_server()]. Specifically,
#' the function takes two parameters, `to_email` and `message`.
#'
#' @seealso [login_server()]
#' @param from_email the from email address sent from [login_server()].
#' @param email_host SMTP email host.
#' @param email_port SMPT email port.
#' @param email_username username for the SMTP server.
#' @param email_password password for the SMTP server.
#' @return returns a function to send an email using the `emayili` package.
#' @importFrom emayili envelope from to subject text server
#' @export
emayili_emailer <- function(
		email_host = NULL,
		email_port = NULL,
		email_username = NULL,
		email_password = NULL,
		from_email = NULL
) {
	function(to_email, subject, message) {
		email <- emayili::envelope() |>
			emayili::from(from_email) |>
			emayili::to(to_email) |>
			emayili::subject(subject) |>
			emayili::text(message)
		smtp <- emayili::server(
			email_host,
			email_port,
			email_username,
			email_password
		)
		smtp(email, verbose = FALSE)
	}
}
