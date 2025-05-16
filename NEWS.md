# login 1.0.0

* First stable release of login.
* This version adds parameter module. This is a mechanism to get parameters from users when Shiny applications startup. Optionally values are stored as cookies.

# login 0.9.4

* Added feature to encrypt cookie values using sodium. See the cookie_password parameter on the the login_server function for more details.

# login 0.9.3

* First version on CRAN.

# login 0.9.2

* Fixed an issue where additional_fields were not saved in the returned reactiveValues object.
* Used different shinybusy method so it doesn't interfere with modal dialog boxes.
* Added new demo to use modal dialogs for logging in.

# login 0.9.1

* Initial version on Github.
