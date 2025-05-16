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
		params = c('characterType', 'numericType', 'dateType', 'logicalType', 'passwordType', 'selectType'),
		param_types = c('character', 'numeric', 'date', 'logical', 'password', 'select'),
		intro_message = 'This is an example application that shows each of the parameter types.',
		input_params = list(selectType = list(choices = c('Option A', 'Option B')))
	)

	output$param_values <- shiny::renderText({
		txt <- character()
		for(i in names(params)) {
			txt <- paste0(txt, i, ' = ', params[[i]], '\n')
		}
		return(txt)
	})
}

shiny::shinyApp(ui = ui, server = server, options = list(port = 2112))
