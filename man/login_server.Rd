% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/login_server.R
\name{login_server}
\alias{login_server}
\title{Login server module.}
\usage{
login_server(
  id,
  db_conn = NULL,
  users_table = "users",
  activity_table = "users_activity",
  emailer = NULL,
  new_account_subject = "Verify your new account",
  reset_password_subject = "Reset password",
  verify_email = !is.null(emailer),
  additional_fields = NULL,
  cookie_name = "loginusername",
  cookie_expiration = 30,
  username_label = "Email:",
  password_label = "Password:",
  create_account_label = "Create Account",
  create_account_message = NULL,
  reset_email_message = NULL,
  enclosing_panel = shiny::wellPanel,
  code_length = 6,
  salt = NULL,
  salt_algo = "sha512"
)
}
\arguments{
\item{id}{unique ID for the Shiny Login module.}

\item{db_conn}{a DBI database connection.}

\item{users_table}{the name of the table in the database to store credentials.}

\item{activity_table}{the name of the table in the database to log login and
logout activity.}

\item{emailer}{function used to send email messages. The function should have
have three parameters: `to_email` for the address to send the email,
`subject` for the subject of the email and `message` for the contents
of the email address. See [emayili_emailer()] for an example.}

\item{new_account_subject}{the subject used for verifying new accounts.}

\item{reset_password_subject}{the subject of password reset emails.}

\item{verify_email}{if true new accounts will need to verify their email
address before the account is crated. This is done by sending a six
digit code to the email address.}

\item{additional_fields}{a character vector fo additional fields the user is
asked to fill in at the when creating a new account. The names of the
vector correspond to the variable names and the values will be used
as the input labels.}

\item{cookie_name}{the name of the cookie saved. Set to `NULL` to disable cookies.}

\item{cookie_expiration}{the number of days after which the cookie will expire.}

\item{username_label}{label used for text inputs of username.}

\item{password_label}{label used for text inputs of password.}

\item{create_account_label}{label for the create account button.}

\item{create_account_message}{Email message sent to confirm email when creating
a new account. Include `\%s` somewhere in the message to include the code.}

\item{reset_email_message}{Email message sent to reset password. Include `\%s`
somewhere in the message to include the code.}

\item{enclosing_panel}{the Shiny element that contains all the UI elements.
The default is [shiny::wellPanel()]. If you wish a more subtle appearance
[htmltools::div()] is a reasonable choice.}

\item{code_length}{the number of digits of codes emailed for creating accounts
(if `verify_email == TRUE`) or resetting passwords.}

\item{salt}{a salt to use to encrypt the password before storing it in the database.}

\item{salt_algo}{the algorithm used to encrypt the password. See
[digest::digest()] for more details.}
}
\value{
a [shiny::reactiveValues()] object that includes two values: `logged_in`
       (this is TRUE if the user is logged in) and `username` which has the
       user's login username if logged in.
}
\description{
This is the main server logic for the `login` Shiny module.
}
