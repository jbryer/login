if(FALSE) {
	params <- c('Name', 'Email', 'Date_of_Birth')
}

#' Button to show the parameter modal dialog box.
#'
#' @rdname userParamServerButtons
#' @param id unique ID for the Shiny Login module.
#' @param label The contents of the button or link–usually a text label, but you could also use any
#'        other HTML, like an image.
#' @param icon An optional [shiny::icon()] to appear on the button.
#' @export
showParamButton <- function(id, label = 'Edit Parameters', icon = shiny::icon('gear')) {
	shiny::actionButton(inputId = NS(id, 'show_modal'),
						label = label,
						icon = icon)
}

#' Action button that will clear the parameters and remove cookies.
#'
#' @rdname userParamServerButtons
#' @param id unique ID for the Shiny Login module.
#' @param label The contents of the button or link–usually a text label, but you could also use any
#'        other HTML, like an image.
#' @param icon An optional [shiny::icon()] to appear on the button.
#' @export
clearParamButton <- function(id, label = 'Clear Parameters', icon = shiny::icon('eraser')) {
	shiny::actionButton(inputId = NS(id, 'clear_params'),
						label = label,
						icon = icon)
}

#' A simple validation check to ensure the parameters are not blank.
#'
#' @param values a named list of values to check.
#' @param types a character vector of the input types.
#' @return either `TRUE` if the validation passes or a character string indicating why the
#'         validation failed.
#' @export
simple_parameter_validator <- function(values, types) {
	for(i in seq_len(length(values))) {
		if(types[i] == 'date') { next } # TODO: Not sure why dates fail the if statement
		if(is.null(values[[i]]) | is.na(values[[i]]) | values[[i]] == '') {
			return(paste0('Parameter "', names(values[i]), '" is required.'))
		}
	}
	return(TRUE)
}

#' Server module for user parameter input
#'
#' The primary purpose of this Shiny module is to provide a framework to get parameters from a user
#' required for the Shiny application to run.
#'
#' See the `parameters` vignette for more information.
#'
#' @rdname userParamServer
#' @param id unique ID for the Shiny Login module.
#' @param params a character vector with the name of the parameters. This should be valid R names
#'        (i.e. start with a letter, do not contain any spaces, etc.).
#' @param param_labels labels used for the user inputs.
#' @param param_types the type of value. Valid types include `character`, `numeric`,
#'        `date`, `file`, `logical`, `password`, or `select`.
#'        Note that for `select` you must specify the `choices` parameter in the `input_params`.
#'         For example, `input_params <- list('param_name' = c('Option A', 'Option B'))`
#' @param param_defaults default values for the params.
#' @param modal_title title for the modal dialog.
#' @param intro_message A message that is displayed at the top of the modal dialog. This can
#'        be a character string or any valid Shiny container (e.g. `shiny::div`).
#' @param modal_size One of "s" for small, "m" (the default) for medium, "l" for large, or "xl" for
#'        extra large. See [shiny::modalDialog()] for more info.
#' @param save_label label for the save button in the modal dialog.
#' @param cancel_label label for the cancel button in the modal dialog, or `NULL` to exclude.
#'        If available a user clicking the cancel button will bypass any validation checks.
#' @param allow_cookies if `TRUE` the user can opt to save the values as cookies. The user will
#'        still be presented with a checkbox to not save parameters as cookies.
#' @param save_cookie_label label for the check box where the user can opt to save the parameter
#'        values as cookies.
#' @param validator a function to validate the user inputs. See [simple_parameter_validator()] for
#'        an example. The function should take two parameters: a named list of values to check
#'        and a vector of value types (see `param_types`). The function should return either `TRUE`
#'        if the validation passes or a character string indicating why the validation failed. Set
#'        to `NULL` to disable validation.
#' @param open_on_startup if `TRUE` the modal dialog will be shown if the parameters could not be
#'        read from cookies (i.e. they have not been set yet).
#' @param cookie_password key used to encrypt/decrypt cookies. See [encrypt_cookie()] and [decrypt_cookie()].
#' @param cookie_expiration the number of days after which the cookie will expire.
#' @param input_params additional parameters passed to the Shiny input. This should be a named list
#'        where names correspond to `params`.
#' @export
#' @return a [shiny::reactiveValues] object.
#' @importFrom cookies get_cookie set_cookie
#' @importFrom stats var
userParamServer <- function(
		id,
		params,
		param_labels = params,
		param_types = rep('character', length(params)),
		param_defaults = rep('', length(params)),
		modal_title = 'Application Settings',
		intro_message = '',
		modal_size = 'l',
		save_label = 'Save',
		cancel_label = NULL,
		allow_cookies = TRUE,
		save_cookie_label = 'Save parameters as cookies.',
		validator = simple_parameter_validator,
		open_on_startup = TRUE,
		cookie_password = NULL,
		cookie_expiration = 30,
		input_params = list()
) {
	if(missing(params)) {
		stop("The 'params' parameter is required with at least one value.")
	}
	if(var(c(length(params), length(param_labels), length(param_types))) != 0) { #,  length(param_defaults)
		stop('Parameters labels, types, and defaults must all be of the same length.')
	}
	if(is.null(cookie_password)) {
		warning("cookie_password not specified. Not specifying a key means cookies will be stored unencrypted in the user's browsers")
	} else {
		cookie_password <- sodium::sha256(charToRaw(cookie_password))
	}

	names(param_labels) <- params
	names(param_types) <- params
	names(param_defaults) <- params

	moduleServer(id, function(input, output, session) {
		values <- reactiveValues()
		all_set <- reactiveVal(FALSE) # Are all the values stored as cookies
		startup <- reactiveVal(TRUE)
		validation_message <- reactiveVal('')

		for(i in params) {
			values[[ i ]] <- param_defaults[i]
		}

		shiny::observeEvent(cookies::get_cookie(cookie_name = params[1], session = session), {
			cookies_set <- TRUE
			for(i in names(values)) {
				val <- cookies::get_cookie(cookie_name = i, missing = '', session = session)
				if(val != '' & !is.null(cookie_password)) {
					val <- decrypt_cookie(cookie_password, val)
				}
				if(param_types[i] == 'file') {
					if(!file.exists(val)) { # If the file no longer exists, set to blank.
						# warning('File previously uploaded no longer available.')
						cookies::remove_cookie(cookie_name = i, session = session)
						val <- ''
					}
				}
				cookies_set <- cookies_set & val != ''
				values[[i]] <- val
			}
			all_set(cookies_set)
		})

		param_modal <- reactive({
			ns <- shiny::NS(id)
			inputs <- list()
			inputs[[ length(inputs) + 1 ]] <- shiny::div(intro_message)
			inputs[[ length(inputs) + 1 ]] <- shiny::strong(validation_message(), style = 'color:#FF0000')
			for(i in seq_len(length(names(values)))) {
				val <- values[[ params[i] ]]
				param_list <- list()
				if(params[i] %in% names(input_params)) {
					param_list <- input_params[[ params[i] ]]
				}
				param_list$inputId <- ns(params[i])
				param_list$label <- param_labels[i]
				param_list$width <- '100%'
				input_fun <- NULL

				if(param_types[i] == 'date') {
					input_fun <- shiny::dateInput
					param_list$value <- as.Date(val)
				} else if(param_types[i] == 'numeric') {
					input_fun <- shiny::numericInput
					param_list$value <- as.numeric(val)
				} else if(param_types[i] == 'file') {
					input_fun <- shiny::fileInput
					param_list$multiple <- FALSE
				} else if(param_types[i] == 'select') {
					if(!('choices' %in% names(param_list))) {
						stop(paste0('The choices parameter must be set. Try:\n',
									'input_params = list("', params[i],
									'" = list(choices = c("Option A", "Option B")))'))
					}
					input_fun <- shiny::selectInput
				} else if(param_types[i] == 'password') {
					input_fun <- shiny::passwordInput
					param_list$value <- val
				} else if(param_types[i] == 'logical') {
					input_fun <- shiny::checkboxInput
					param_list$value <- as.logical(val)
					if(is.na(param_list$value)) {
						param_list$value <- FALSE
					}
				} else if(param_types[i] == 'character') {
					input_fun <- shiny::textInput
					param_list$value <- val
				} else {
					stop(paste0('Unknown parameter type: ', param_types[i]))
				}
				inputs[[ length(inputs) + 1 ]] <- do.call(input_fun, param_list)
			}

			if(allow_cookies) {
				inputs[[ length(inputs) + 1 ]] <- shiny::checkboxInput(
					inputId = ns('save-cookies'),
					label = save_cookie_label,
					value = TRUE,
					width = '100%'
				)
			}

			footer <- tagList()
			if(!is.null(cancel_label)) {
				footer[[length(footer) + 1]] <- shiny::actionButton(inputId = NS(id, 'cancel_params'),
																	label = cancel_label)
			}
			footer[[length(footer) + 1]] <- shiny::actionButton(inputId = NS(id, "save_params"),
																label = save_label)

			shiny::modalDialog(
				title = modal_title,
				do.call(shiny::div, inputs),
				easyClose = FALSE,
				footer = footer,
				size = modal_size
			)
		})

		observeEvent(input$show_modal, {
			shiny::showModal(param_modal())
		})

		observeEvent(input$cancel_params, {
			shiny::removeModal()
		})

		observeEvent(input$save_params, {
			for(i in names(values)) {
				if(param_types[i] == 'file') {
					val <- input[[i]]$datapath[1]
				# } else if(param_types[i] == 'logical') {
				# 	val <- input[i]
				} else {
					val <- input[[i]]
				}
				values[[i]] <- val
				if(!is.null(cookie_password)) {
					val <- encrypt_cookie(cookie_password, val)
				}
				if(allow_cookies & input[['save-cookies']]) {
					cookies::set_cookie(cookie_name = i,
										cookie_value = val,
										session = session,
										expiration = cookie_expiration)
				}
			}
			if(validate()) {
				shiny::removeModal()
			}
		})

		validate <- reactive({
			if(!is.null(validator) & !shiny::isolate(startup())) {
				msg <- validator(shiny::reactiveValuesToList(values), param_types)
				if(!is.logical(msg)) {
					validation_message(msg)
					shiny::showModal(param_modal())
					return(FALSE)
				} else if(!msg) {
					warning('Validation message returned FALSE. Should return a message instead.')
					validation_message('Validation failed.')
					shiny::showModal(param_modal())
					return(FALSE)
				} else {
					validation_message('')
				}
			}
			return(TRUE)
		})

		observeEvent(input$clear_params, {
			for(i in names(values)) {
				values[[i]] <- ''
				cookies::remove_cookie(cookie_name = i, session = session)
				validate()
			}
		})

		observe({
			if(startup()) {
				if(!all_set()) {
					showModal(param_modal())
				}
				startup(FALSE)
			}
		})

		return(values)
	})
}
