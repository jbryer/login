library(shiny)
library(login)
library(cookies)

# source('../../R/user_param_module.R')

#' Simple email validator.
#' @param x string to test.
#' @return TRUE if the string is a valid email address.
is_valid_email <- function(x) {
	grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)
}

#' Custom validator function that also checks if the `email` field is a valid email address.
my_validator <- function(values, types) {
	spv <- simple_parameter_validator(values)
	if(!is.logical(spv)) {
		return(spv)
	} else {
		if(is_valid_email(values[['email']])) {
			return(TRUE)
		} else {
			return(paste0(values[['email']], ' is not a valid email address.'))
		}
	}
	return(TRUE)
}

ui <- shiny::fluidPage(
	cookies::cookie_dependency(),  # Necessary to save/get cookies
	shiny::titlePanel('Parameter Example'),
	shiny::verbatimTextOutput('param_values'),
	showParamButton('example'),
	clearParamButton('example')
)

server <- function(input, output) {
	params <- userParamServer(
		id = 'example',
		validator = my_validator,
		params = c('name', 'email'),
		param_labels = c('Your Name:', 'Email Address:'),
		param_types = c('character', 'character'),
		intro_message = 'This is an example application that asks the user for two parameters.')

	output$param_values <- shiny::renderText({
		txt <- character()
		for(i in names(params)) {
			txt <- paste0(txt, i, ' = ', params[[i]], '\n')
		}
		return(txt)
	})
}

shiny::shinyApp(ui = ui, server = server, options = list(port = 2112))
