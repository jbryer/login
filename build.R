
usethis::use_tidy_description()
devtools::document()
devtools::install(dependencies = FALSE)

shiny::runApp('inst/login_template/', port = 2112)



devtools::check()
