#' JavaScript and CSS dependencies.
#'
#' @return a [htmltools::htmlDependency()] object defining the JavaScript and CSS files.
#' @importFrom htmltools htmlDependencies
#' @importFrom utils packageVersion
#' @export
login_dependencies <- function() {
	htmltools::htmlDependency(
		name = 'login-assets',
		version = utils::packageVersion('login'),
		package = 'login',
		src = 'assets',
		script = c('md5.js', 'passwdInputBinding.js'),
		stylesheet = c('style.css')
	)
}
