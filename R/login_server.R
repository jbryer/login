#' Login server module.
#'
#' This is the main server logic for the `login` Shiny module to be included
#' in server.R side,.
#'
#' @param id unique ID for the Shiny Login module.
#' @param db_conn a DBI database connection.
#' @param users_table the name of the table in the database to store credentials.
#' @param activity_table the name of the table in the database to log login and#' Login server module.
#'
#' This is the main server logic for the `login` Shiny module to be included
#' in server.R side,.
#'
#' @param id unique ID for the Shiny Login module.
#' @param db_conn a DBI database connection.
#' @param users_table the name of the table in the database to store credentials.
#' @param activity_table the name of the table in the database to log login and
#'        logout activity.
#' @param emailer function used to send email messages. The function should have
#'        have three parameters: `to_email` for the address to send the email,
#'        `subject` for the subject of the email and `message` for the contents
#'        of the email address. See [emayili_emailer()] for an example.
#' @param reset_password_subject the subject of password reset emails.
#' @param new_account_subject the subject used for verifying new accounts.
#' @param verify_email if true new accounts will need to verify their email
#'        address before the account is crated. This is done by sending a six
#'        digit code to the email address.
#' @param additional_fields a character vector of additional fields the user is
#'        asked to fill in at the when creating a new account. The names of the
#'        vector correspond to the variable names and the values will be used
#'        as the input labels.
#' @param username_label label used for text inputs of username.
#' @param password_label label used for text inputs of password.
#' @param create_account_label label for the create account button.
#' @param cookie_name the name of the cookie saved. Set to `NULL` to disable cookies.
#' @param cookie_expiration the number of days after which the cookie will expire.
#' @param cookie_password password used to encrypt cookies saved in the browser.
#' @param create_account_message Email message sent to confirm email when creating
#'        a new account. Include `\%s` somewhere in the message to include the code.
#' @param reset_email_message Email message sent to reset password. Include `\%s`
#'        somewhere in the message to include the code.
#' @param enclosing_panel the Shiny element that contains all the UI elements.
#'        The default is [shiny::wellPanel()]. If you wish a more subtle appearance
#'        [htmltools::div()] is a reasonable choice.
#' @param code_length the number of digits of codes emailed for creating accounts
#'        (if `verify_email == TRUE`) or resetting passwords.
#' @param salt a salt to use to encrypt the password before storing it in the database.
#' @param salt_algo the algorithm used to encrypt the password. See
#'        [digest::digest()] for more details.
#' @param shinybusy_position Position of the spinner when sending emails.
#'        See [shinybusy::use_busy_spinner()] for more information.
#' @param shinybusy_spin Style of the spinner when sending emails.
#'        See [shinybusy::use_busy_spinner()] for more information.
#' @return a [shiny::reactiveValues()] object that includes two values: `logged_in`
#'        (this is TRUE if the user is logged in) and `username` which has the
#'        user's login username if logged in.
#' @import shiny
#' @importFrom DBI dbListTables dbWriteTable dbReadTable dbSendQuery dbFetch
#' @importFrom cookies get_cookie set_cookie
#' @importFrom stringr str_pad
#' @importFrom shinybusy use_busy_spinner show_spinner hide_spinner
#' @importFrom digest digest
#' @importFrom shinyjs hide show
#' @importFrom sodium data_decrypt data_encrypt sha256 bin2hex hex2bin
#' @export
#' @example inst/login_demo_simple/app.R
login_server <- function(
		id,
		db_conn = NULL,
		users_table = 'users',
		activity_table = 'users_activity',
		emailer = NULL,
		new_account_subject = 'Verifizieren Sie ihren neuen Account',
		reset_password_subject = 'Passwort zurücksetzen',
		verify_email = !is.null(emailer),
		additional_fields = NULL,
		cookie_name = 'loginusername',
		cookie_expiration = 30,
		cookie_password = NULL,
		username_label = 'E-Mail:',
		password_label = 'Passwort',
		create_account_label = "Account erstellen",
		create_account_message = NULL,
		reset_email_message = NULL,
		enclosing_panel = shiny::wellPanel,
		code_length = 6,
		salt = NULL,
		salt_algo = "sha512",
		shinybusy_spin = "fading-circle",
		shinybusy_position = "full-page"
) {
	# Default messages
	if(is.null(create_account_message)) {
		create_account_message <- 'Ihr Bestätigungscode zur Erstellung eines neuen Kontos lautet: %s\n
Wenn Sie nicht angefordert haben, ein neues Konto zu erstellen, können Sie diese E-Mail ignorieren.'
	}
	if(is.null(reset_email_message)) {
		reset_email_message <- 'Ihr Code zum Zurücksetzen des Passworts lautet: %s\n
Wenn Sie nicht angefordert haben, Ihr Passwort zurückzusetzen, können Sie diese E-Mail ignorieren..'
	}

	# Handle additional fields
	if(!is.null(additional_fields)) {
		if(is.null(names(additional_fields))) {
			names(additional_fields) <- additional_fields
		}
	}

	# Cookie encryption setup
	cookie_key <- NULL
	cookie_nonce <- rep(as.raw(42), 24)

	encrypt_cookie <- function(message) {
		message |>
			charToRaw() |>
			sodium::data_encrypt(key = cookie_key, nonce = cookie_nonce) |>
			sodium::bin2hex()
	}

	decrypt_cookie <- function(message) {
		message |>
			sodium::hex2bin() |>
			sodium::data_decrypt(key = cookie_key, nonce = cookie_nonce) |>
			rawToChar()
	}

	# Email encryption function
	encrypt_email <- function(email) {
		if(is.null(salt)) return(tolower(email))
		digest::digest(paste0(salt, tolower(email)), algo = salt_algo, serialize = FALSE)
	}

	# Password encryption function
	encrypt_password <- function(password) {
		if(is.null(salt)) return(password)
		digest::digest(paste0(salt, password), algo = salt_algo, serialize = FALSE)
	}

	if(!is.null(cookie_name) && is.null(cookie_password)) {
		warning("cookie_password not specified. Cookies will be stored unencrypted in the user's browsers")
	} else if(!is.null(cookie_name)) {
		cookie_key <- sodium::sha256(charToRaw(cookie_password))
	}

	moduleServer(id, function(input, output, session) {
		# Initialize tables if they don't exist
		if(!users_table %in% DBI::dbListTables(db_conn)) {
			users <- data.frame(
				username = character(),  # This will store encrypted email
				password = character(),
				created_date = numeric(),
				stringsAsFactors = FALSE
			)
			if(!is.null(additional_fields)) {
				for(i in seq_len(length(additional_fields))) {
					users[,names(additional_fields[i])] <- character()
				}
			}
			DBI::dbWriteTable(db_conn, users_table, users)
		}

		if(!activity_table %in% DBI::dbListTables(db_conn)) {
			activity <- data.frame(
				username = character(),  # This will store encrypted email
				action = character(),
				timestamp = numeric(),
				stringsAsFactors = FALSE
			)
			DBI::dbWriteTable(db_conn, activity_table, activity)
		}

		# Helper functions
		add_activity <- function(encrypted_username, activity) {
			if(!is.null(activity_table)) {
				new_activity <- data.frame(
					username = encrypted_username,
					action = activity,
					timestamp = Sys.time(),
					stringsAsFactors = FALSE
				)
				DBI::dbWriteTable(db_conn, activity_table, new_activity, append = TRUE)
			}
		}

		get_users <- function() {
			DBI::dbReadTable(db_conn, users_table)
		}

		get_user <- function(encrypted_username) {
			user <- DBI::dbSendQuery(
				db_conn,
				paste0("SELECT * FROM ", users_table, " WHERE username='", encrypted_username, "'")
			) |> DBI::dbFetch()
			return(user)
		}

		# Create temporary storage for verification process
		USER <- reactiveValues()
		USER$logged_in <- FALSE
		USER$unique <- format(Sys.time(), '%Y%m%d%H%M%S')
		USER$username <- NA  # Holds current session's plain email (only in memory)
		USER$encrypted_username <- NA  # Holds encrypted version for DB operations
		for(i in additional_fields) {
			USER[[i]] <- NA
		}

		# Handle cookie-based login
		observeEvent(cookies::get_cookie(cookie_name = cookie_name, session = session), {
			cookie_value <- cookies::get_cookie(cookie_name = cookie_name, session = session)
			tryCatch({
				if(!is.null(cookie_key)) {
					plain_email <- decrypt_cookie(cookie_value)
					encrypted_email <- encrypt_email(plain_email)
					user <- get_user(encrypted_email)
					if(nrow(user) > 0) {
						USER$username <- plain_email  # Only in memory
						USER$encrypted_username <- encrypted_email
						USER$logged_in <- TRUE
						for(i in names(additional_fields)) {
							USER[[i]] <- user[1,i]
						}
						add_activity(encrypted_email, 'login_cookie')
					}
				}
			}, error = function(e) {
				warning('Error retrieving cookie value.')
				cookies::remove_cookie(cookie_name = cookie_name)
			})
		}, once = TRUE)

		# Handle login
		observeEvent(input$Login, {
			plain_email <- tolower(input$username)
			encrypted_email <- encrypt_email(plain_email)
			encrypted_password <- encrypt_password(input$password)

			user <- get_user(encrypted_email)

			if(nrow(user) == 0) {
				login_message('Username nicht gefunden.')
			} else if(encrypted_password != user$password) {
				login_message('Inkorrektes Passwort')
			} else {
				if(!is.null(input$remember_me) && input$remember_me) {
					cookie_value <- plain_email
					if(!is.null(cookie_key)) {
						cookie_value <- encrypt_cookie(plain_email)
					}
					tryCatch({
						cookies::set_cookie(
							cookie_name = cookie_name,
							cookie_value = cookie_value,
							session = session,
							expiration = cookie_expiration
						)
					}, error = function(e) {
						message(e)
					})
				}

				login_message('')
				USER$logged_in <- TRUE
				USER$username <- plain_email  # Only in memory
				USER$encrypted_username <- encrypted_email
				for(i in names(additional_fields)) {
					USER[[i]] <- user[i]
				}
				add_activity(encrypted_email, 'login')
			}
		})

		# Handle new user creation
		observeEvent(input$new_user, {
			plain_email <- tolower(input$new_username)
			encrypted_email <- encrypt_email(plain_email)
			encrypted_password1 <- encrypt_password(input$new_password1)
			encrypted_password2 <- encrypt_password(input$new_password2)

			user <- get_user(encrypted_email)

			if(nrow(user) > 0) {
				new_user_message(paste0('Account existiert bereits für ', plain_email))
			} else if(encrypted_password1 != encrypted_password2) {
				new_user_message('Passwörter stimmen nicht überein.')
			} else if(input$new_password1 == '') {
				new_user_message('Bitte geben Sie ein korrektes Passwort ein.')
			} else {
				newuser <- data.frame(
					username = encrypted_email,
					password = encrypted_password1,
					created_date = Sys.time(),
					stringsAsFactors = FALSE
				)

				if(!is.null(additional_fields)) {
					for(i in seq_len(length(additional_fields))) {
						newuser[1,names(additional_fields)[i]] <- input[[names(additional_fields)[i]]]
					}
				}

				if(verify_email) {
					shinybusy::show_spinner()
					new_user_values(newuser)
					code <- generate_code()
					tryCatch({
						emailer(
							to_email = plain_email,  # Use plain email only for sending
							subject = new_account_subject,
							message = sprintf(create_account_message, code)
						)
						new_user_code_verify(code)
					}, error = function(e) {
						message(e)
						reset_message(paste0('Error sending email: ', as.character(e)))
					})
					shinybusy::hide_spinner()
				} else {
					add_user(newuser)
					add_activity(encrypted_email, 'create_account')
					new_user_message(paste0('Neuer Account wurde erstellt für: ', plain_email,
											'. Sie können sich nun einloggen.'))
				}
			}
		})

		# Handle password reset
		observeEvent(input$send_reset_password_code, {
			plain_email <- tolower(isolate(input$forgot_password_email))
			encrypted_email <- encrypt_email(plain_email)
			user <- get_user(encrypted_email)

			if(nrow(user) == 0) {
				reset_message(paste0(plain_email, ' nicht gefunden.'))
			} else {
				code <- generate_code()
				shinybusy::show_spinner()
				tryCatch({
					emailer(
						to_email = plain_email,  # Use plain email only for sending
						subject = reset_password_subject,
						message = sprintf(reset_email_message, code)
					)
					reset_username(encrypted_email)  # Store encrypted version
					reset_code(code)
				}, error = function(e) {
					reset_message(paste0('Error sending email: ', as.character(e)))
				})
				shinybusy::hide_spinner()
			}
		})

		return(USER)
	})
}
