#' JavaScript and CSS dependencies.
#'
#' @return a `htmlDependcies` object defining the JavaScript and CSS files.
#' @importFrom htmltools htmlDependencies
#' @export
login_dependencies <- function() {
	htmltools::htmlDependency(
		name = 'login-assets',
		version = packageVersion('login'),
		package = 'login',
		src = 'assets',
		script = c('md5.js', 'passwdInputBinding.js'),
		stylesheet = c('style.css')
	)
}
