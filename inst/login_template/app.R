library(shiny)
library(login)

library(cookies)
library(emayili)

if(file.exists('config.R')) {
    source('config.R')
} else {
    warning('No configuration file found. Email will not work.')
}

##### Define Callback Functions ################################################


###### User Interface ##########################################################
ui <- fluidPage(
    titlePanel("Shiny Login Template"),
    tabsetPanel(
        tabPanel('Login',
                 login::login_ui(id = 'login_demo') ),
        tabPanel('Create Accont',
                 login::new_user_ui(id = 'login_demo') ),
        tabPanel('Reset Password',
                 login::reset_password_ui(id = 'login_demo'))
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
        db_conn = RSQLite::dbConnect(RSQLite::SQLite(), 'users.sqlite')
    )

    output$is_logged_in <- renderText({
        USER$logged_in
    })

    output$username <- renderText({
        USER$username
    })
}

##### Run the application ######################################################
shinyApp(ui = ui, server = server)
