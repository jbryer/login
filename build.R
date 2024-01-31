# Build the package
usethis::use_tidy_description()
devtools::document()
devtools::install(dependencies = FALSE)

# Run the Shiny demo app
shiny::runApp('inst/login_template/', port = 2112)

# Run R CMD CHECK
devtools::check()
