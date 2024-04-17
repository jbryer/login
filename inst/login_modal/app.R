library(shiny)
library(login)
library(shinyjs)

if(file.exists('config.R')) {
    source('config.R')
} else {
    warning('No configuration file found. Email will not work.')
}

APP_ID <- 'login_modal'

###### User Interface ##########################################################
ui <- fluidPage(
    useShinyjs(),
    titlePanel("Shiny Login Modal Example"),
    uiOutput('login_button'),
    hr(),
    div('Are you logged in? ', textOutput('is_logged_in')),
    div('Username: ', textOutput('username')),
    div('Name: ', textOutput('name')),
    use_login()
    # is_logged_in(
    #     id = APP_ID,
    #     div("This only shows when you are logged in!")
    # ),
    # is_not_logged_in(
    #     id = APP_ID,
    #     div("This only shows when you are NOT logged in!")
    # )
)

##### Server ###################################################################
server <- function(input, output, session) {
    USER <- login::login_server(
        id = APP_ID,
        db_conn = DBI::dbConnect(RSQLite::SQLite(), 'users.sqlite'),
        emailer = emayili_emailer(
            email_host = email_host,
            email_port = email_port,
            email_username = email_username,
            email_password = email_password,
            from_email = reset_password_from_email
        ),
        additional_fields = c('first_name' = 'First Name',
                              'last_name' = 'Last Name'),
        cookie_name = NULL, # TODO: Cookies currently don't work with modal dialogs
        salt = 'login_demo'
    )

    output$login_button <- renderUI({
        if(USER$logged_in) {
            login::logout_button(APP_ID, style = "position: absolute; right: 20px; top: 10px")
        } else {
            shiny::actionButton(inputId = 'login_button',
                                label = 'Login',
                                style = "position: absolute; right: 20px; top: 10px")
        }
    })

    observeEvent(input$login_button, {
        showModal(modalDialog(
            footer = NULL,
            easyClose = TRUE,
            div(id = 'login_box',
                tabsetPanel(
                    id = 'login_panel',
                    tabPanel('Login',
                             login::login_ui(id = APP_ID) ),
                    tabPanel('Create Account',
                             login::new_user_ui(id = APP_ID) ),
                    tabPanel('Reset Password',
                             login::reset_password_ui(id = APP_ID))
                )
            )
        ))
    })

    observe({
        if(USER$logged_in) {
            removeModal()
        }
    })

    output$is_logged_in <- renderText({
        USER$logged_in
    })

    output$username <- renderText({
        USER$username
    })

    output$name <- renderText({
        paste0(USER$first_name, ' ', USER$last_name)
    })
}

##### Run the application ######################################################
shinyApp(ui = ui, server = server)
