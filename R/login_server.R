#' Login server module.
#'
#' @param id unique ID for the Shiny Login module.
#' @param db_conn a DBI database connection.
#' @param users_table the name of the table in the database to store credentials.
#' @param reset_password_from_email the from email address for password resets.
#' @param reset_password_subject the subject of password reset emails.
#' @param email_host SMTP email host.
#' @param email_port SMPT email port.
#' @param email_username username for the SMTP server.
#' @param email_password password for the SMTP server.
#'
#' @import shiny
#' @importFrom emayili envelope from to subject text server
#' @importFrom DBI dbListTables dbWriteTable dbReadTable dbSendQuery
#' @importFrom cookies get_cookie set_cookie
#' @importFrom stringr str_pad
#' @export
login_server <- function(
		id,
		db_conn = NULL,
		users_table = 'users',
		reset_password_from_email = NULL,
		reset_password_subject = 'Reset password',
		email_host = NULL,
		email_port = NULL,
		email_username = NULL,
		email_password = NULL

) {
	moduleServer(id, function(input, output, session) {
		# cookie_username <- paste0(id, 'username')

		# Check to see if the users_table is already in the database, if not
		# create the table.
		if(!users_table %in% DBI::dbListTables(db_conn)) {
			users <- data.frame(username = character(),
								password = character(),
								created_date = numeric(),
								stringsAsFactors = FALSE)
			DBI::dbWriteTable(db_conn, users_table, users)
		}

		get_users <- function() {
			# TODO: allow for non SQL user management
			DBI::dbReadTable(db_conn, users_table)
		}

		USER <- reactiveValues(logged_in = FALSE,
							   unique = format(Sys.time(), '%Y%m%d%H%M%S'),
							   username = NA)

		output$logged_in <- renderText({
			USER$logged_in
		})

		# observeEvent(cookies::get_cookie(cookie_username), {
		# 	username <- cookies::get_cookie(cookie_username)
		# 	if(!is.null(username)) {
		# 		USER$username <- username
		# 		USER$logged_in <- TRUE
		# 	}
		# }, once = TRUE)

		##### User Login #######################################################
		login_message <- reactiveVal('')

		output$login_message <- renderText({
			login_message()
		})

		output$login_ui <- renderUI({
			wellPanel(
				div(textOutput(NS(id, 'login_message')), style = 'color:red;'),
				textInput(NS(id, 'username'), label = 'Email:', value = ''),
				passwdInput(NS(id, 'password'), label = 'Password: ', value = ''),
				actionButton(NS(id, "Login"), label = 'Login')
			)
		})

		observeEvent(input$Login, {
			users <- get_users()
			username <- input$username
			password <- input$password
			Id.username <- which(tolower(users$username) == tolower(username))
			if(is.null(Id.username) | length(Id.username) != 1) {
				login_message('Username not found.')
			} else if(password != users[Id.username,]$password) {
				login_message('Incorrect password.')
			} else {
				login_message('')
				USER$logged_in <- TRUE
				USER$username <- username
				# cookies::set_cookie(cookie_username, username)
			}
		})

		##### User logout ######################################################
		observeEvent(input$logout, {
			USER$logged_in <- FALSE
			USER$username <- ''
			USER$unique <- format(Sys.time(), '%Y%m%d%H%M%S')
			# remove_cookie(cookie_username)
			# remove_cookie(cookie_email)
			# remove_cookie(cookie_group)
		})

		##### Create new user ##################################################
		new_user_message <- reactiveVal('')

		output$new_user_message <- renderText({
			new_user_message()
		})

		observeEvent(input$new_user, {
			users <- get_users()
			username <- input$new_username
			password1 <- input$new_password1
			password2 <- input$new_password2

			id.username <- which(tolower(users$username) == tolower(username))
			if(length(id.username) > 0) {
				new_user_message(paste0('Account already exists for ', username))
			} else if(password1 != password2) {
				new_user_message('Passwords to not match.')
			} else if(password1 == 'd41d8cd98f00b204e9800998ecf8427e') {
				new_user_message('Please enter a valid password.')
			} else {
				newuser <- data.frame(
					username = username,
					password = password1,
					created_date = Sys.time(),
					stringsAsFactors = FALSE
				)
				for(i in names(users)[(!names(users) %in% names(newuser))]) {
					newuser[,i] <- NA # Make sure the data.frames line up
				}

				DBI::dbWriteTable(db_conn, users_table, newuser, append = TRUE)

				new_user_message(paste0('New account created for ', username,
										'. You can now login.'))
			}

		})

		##### Reset password ###################################################
		reset_code <- reactiveVal('')
		reset_code_verify <- reactiveVal('')
		reset_message <- reactiveVal('')
		reset_username <- reactiveVal('')

		output$reset_password_ui <- renderUI({
			if(is.null(reset_password_from_email) |
			   is.null(email_host) |
			   is.null(email_port) |
			   is.null(email_username) |
			   is.null(email_password)) {
				return(div('Email server has not been configured.'))
			}

			code <- isolate(input$reset_password_code)
			reset_password <- FALSE
			if(nchar(reset_code_verify()) == 6) {
				if(code == reset_code()) {
					reset_password <- TRUE
				}
			}
			if(reset_code() == '') {
				wellPanel(
					div(reset_message(), style = 'color:red'),
					div(
						textInput(inputId = NS(id, 'forgot_password_email'),
								  label = 'Email address: ',
								  value = '')),
					# br(),
					actionButton(inputId = NS(id, 'send_reset_password_code'),
								 label = 'Send reset code')
				)
			} else if(reset_password) {
				wellPanel(
					div(reset_message(), style = 'color:red'),
					div(
						passwdInput(inputId = NS(id, 'reset_password1'),
									label = 'Enter new password:',
									value = ''),
						passwdInput(inputId = NS(id, 'reset_password2'),
									label = 'Confirm new password:',
									value = '')
					),
					# br(),
					actionButton(inputId = NS(id, 'reset_new_password'),
								 label = 'Reset Password')
				)
			} else {
				wellPanel(
					div(reset_message(), style = 'color:red'),
					div(
						textInput(inputId = NS(id, 'reset_password_code'),
								  label = 'Enter the code from the email:',
								  value = '')
					),
					# br(),
					actionButton(inputId = NS(id, 'send_reset_password_code'),
								 label = 'Resend Code'),
					actionButton(inputId = NS(id, 'submit_reset_password_code'),
								 label = 'Submit')
				)
			}
		})

		observeEvent(input$submit_reset_password_code, {
			if(input$submit_reset_password_code == 1) {
				code <- isolate(input$reset_password_code)
				reset_code_verify(code)
				if(nchar(code) != 6 & reset_code() == code) {
					reset_message('Code is not correct')
				}
			}
		})

		observeEvent(input$reset_new_password, {
			if(input$reset_password1 == input$reset_password2) {
				query <- paste0(
					"UPDATE users SET password = '",
					input$reset_password1,
					"' WHERE username = '", reset_username(), "'"
				)
				DBI::dbSendQuery(db_conn, query)
				reset_message('Password updated successfully. Please go to the login tab.')
				reset_code('')
			} else {
				reset_message('Passwords do not match.')
			}
		})

		observeEvent(input$send_reset_password_code, {
			PASSWORD <- DBI::dbReadTable(db_conn, 'users')
			email_address <- isolate(input$forgot_password_email) |> tolower()
			if(!email_address %in% PASSWORD$username) {
				reset_message(paste0(email_address, ' not found.'))
			} else {
				code <- sample(099999, size = 1) |>
					as.character() |>
					stringr::str_pad(width = 6, pad = '0')
				tryCatch({
					username <- PASSWORD[PASSWORD$username == email_address,]$username[1]
					reset_username(username)
					email <- emayili::envelope() |>
						emayili::from(reset_password_from_email) |>
						emayili::to(email_address) |>
						emayili::subject(reset_password_subject) |>
						emayili::text(paste0('Your password reset code is: ', code,
									' \nIf you did not request to reset your password you can ignore this email.'))
					smtp <- emayili::server(
						email_host,
						email_port,
						email_username,
						email_password
					)
					smtp(email, verbose = FALSE)
					reset_code(code)
				}, error = function(e) {
					reset_message(paste0('Error sending email: ', as.character(e)))
				})
			}
		})

		return(USER)
	})
}
