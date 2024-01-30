library(shiny)
library(login)
library(shinyjs)

if(file.exists('config.R')) {
    source('config.R')
} else {
    warning('No configuration file found. Email will not work.')
}

##### Define Callback Functions ################################################


###### User Interface ##########################################################
ui <- fluidPage(
    useShinyjs(),
    titlePanel("Shiny Login Template"),
    div(id = 'login_box',
        tabsetPanel(
            id = 'login_panel',
            tabPanel('Login',
                     login::login_ui(id = 'login_demo') ),
            tabPanel('Create Account',
                     login::new_user_ui(id = 'login_demo') ),
            tabPanel('Reset Password',
                     login::reset_password_ui(id = 'login_demo'))
        )
    ),
    logout_ui('login_demo'),
    hr(),
    div('Are you logged in? ', textOutput('is_logged_in')),
    div('Username: ', textOutput('username')),
    is_logged_in(
        id = 'login_demo',
        div("This only shows when you are logged in!")
    ),
    is_not_logged_in(
        id = 'login_demo',
        div("This only shows when you are NOT logged in!")
    )
)

##### Server ###################################################################
server <- function(input, output, session) {
    USER <- login::login_server(
        id = 'login_demo',
        db_conn = RSQLite::dbConnect(RSQLite::SQLite(), 'users.sqlite'),
        reset_password_from_email = reset_password_from_email,
        reset_password_subject = 'Reset password',
        email_host = email_host,
        email_port = email_port,
        email_username = email_username,
        email_password = email_password
    )

    observeEvent(USER$logged_in, {
        if(USER$logged_in) {
            shinyjs::hide(id = 'login_box')
        } else {
            shinyjs::show(id = "login_box")
        }
    })

    output$is_logged_in <- renderText({
        USER$logged_in
    })

    output$username <- renderText({
        USER$username
    })
}

##### Run the application ######################################################
shinyApp(ui = ui, server = server)
