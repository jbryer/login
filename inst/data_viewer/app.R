library(shiny)
library(login)
library(cookies)

# source('../../R/user_param_module.R')

#' Custom validator function that also checks if the `file` field is a valid CSV file.
my_validator <- function(values, types) {
	spv <- simple_parameter_validator(values)
	if(!is.logical(spv)) {
		return(spv)
	} else {
		if(!file.exists(values$file)) {
			return('File does not exists. Try uploading again.')
		} else if(!tools::file_ext(values$file) %in% c('csv', 'xslx')) {
			return('Only CSV and XLSX files supported.')
		}
	}
	return(TRUE)
}

ui <- shiny::fluidPage(
	cookies::cookie_dependency(),  # Necessary to save/get cookies
	shiny::titlePanel('Data Viewer'),
	showParamButton('csvviewer'),
	clearParamButton('csvviewer'),
	DT::DTOutput('data_table')
)

server <- function(input, output) {
	params <- userParamServer(
		id = 'csvviewer',
		validator = my_validator,
		params = c('filetype', 'file'),
		param_labels = c('File type', 'URL to a CSV file:'),
		input_params = list("filetype" = list("choices" = c("", "CSV" = "csv", "Excel" = "xlsx"))),
		param_types = c('select', 'file'),
		intro_message = 'This application will view a spreadsheet as a data table.')

	output$data_table <- DT::renderDT({
		df <- data.frame()
		if(file.exists(params$file)) {
			if(params$filetype == 'csv') {
				df <- read.csv(params$file)
			} else if(params$filetype == 'xlsx') {
				df <- readxl::read_excel(params$file)
			}
		}
		return(df)
	})
}

shiny::shinyApp(ui = ui, server = server, options = list(port = 2112))
