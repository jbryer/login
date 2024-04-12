library(shiny)
library(login)

###### User Interface ##########################################################
ui <- fluidPage(
    titlePanel("Shiny Login Simple Demo"),
    p("You can login with 'test/test'."),
    login::login_ui(id = 'login_demo'),
    login::logout_button('login_demo'),
    hr(),
    div('Are you logged in? ', textOutput('is_logged_in')),
    div('Username: ', textOutput('username')),
    login::is_logged_in(
        id = 'login_demo',
        div("This only shows when you are logged in!")
    ),
    login::is_not_logged_in(
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
if(interactive()) {
    shinyApp(ui = ui, server = server)
}
