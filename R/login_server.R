#' Login server module.
#'
#' @param id unique ID for the Shiny Login module.
#' @import shiny
#' @importFrom cookies get_cookie set_cookie
#' @export
login_server <- function(
		id,
		db_conn = NULL,
		users_table = 'users',
		reset_password_from_email,
		reset_password_subject = 'Reset password',
		email_host,
		email_port,
		email_username,
		email_password

) {
	moduleServer(id, function(input, output, session) {
		# cookie_username <- paste0(id, 'username')

		# Check to see if the users_table is already in the database, if not
		# create the table.
		if(!users_table %in% DBI::dbListTables(db_conn)) {
			users <- data.frame(username = character(),
								password = character(),
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

		# observeEvent(USER, {
		# 	session$userData$logged_in <- USER$logged_in
		# })

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

		observeEvent(input$new_user, {
			users <- get_users()
			username <- input$new_username
			password1 <- input$new_password1
			passowrd2 <- input$new_passwrod2

			id.username <- which(tolower(users$username) == tolower(username))
			if(length(id.username) > 0) {
				new_user_message(paste0('Account already exists for ', username))
			} else if(password1 != password2) {
				new_user_message('Passwords to not match.')
			} else if(password1 == 'd41d8cd98f00b204e9800998ecf8427e') {
				new_user_message('Please enter a valid password.')
			} else {
				newuser <- data.frame(
					username = newusername,
					password = newpassword1,
					stringsAsFactors = FALSE
				)
				for(i in names(users)[(!names(users) %in% names(newuser))]) {
					newuser[,i] <- NA # Make sure the data.frames line up
				}

				DBI::dbWriteTable(login_db_conn, users_table, newuser, append = TRUE)

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
			if(missing(reset_password_from_email) |
			   missing(email_host) |
			   missing(email_port) |
			   missing(email_username) |
			   missing(email_password)) {
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
						textInput('forgot_password_email', 'Email address: ', value = '')),
					br(),
					actionButton('send_reset_password_code', 'Send reset code')
				)
			} else if(reset_password) {
				wellPanel(
					div(reset_message(), style = 'color:red'),
					div(
						passwordInput('reset_password1', label = 'Enter new password:', value = ''),
						passwordInput('reset_password2', label = 'Confirm new password:', value = '')
					),
					br(),
					actionButton('reset_new_password', 'Reset Password')
				)
			} else {
				wellPanel(
					div(reset_message(), style = 'color:red'),
					div(
						textInput('reset_password_code', 'Enter the code from the email:', value = '')
					),
					br(),
					actionButton('send_reset_password_code', 'Resend Code'),
					actionButton('submit_reset_password_code', 'Submit')
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
			PASSWORD <- dbReadTable(db_conn, 'users')
			email_address <- isolate(input$forgot_password_email) |> tolower()
			if(!email_address %in% PASSWORD$username) {
				reset_message(paste0(email_address, ' not found.'))
			} else {
				code <- sample(099999, size = 1) |> as.character() |> str_pad(width = 6, pad = '0')
				tryCatch({
					username <- PASSWORD[PASSWORD$username == email_address,]$username[1]
					reset_username(username)
					email <- envelope() %>%
						from(reset_password_from_email) |>
						to(email_address) |>
						subject(reset_password_subject) |>
						text(paste0('Your password reset code is: ',
									code,
									' \nIf you did not request to reset your password you can ignore this email.'))
					smtp <- server(
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
