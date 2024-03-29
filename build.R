# Build the package
usethis::use_tidy_description()
devtools::document()
devtools::install(dependencies = FALSE)

# Run the Shiny demo app
shiny::runApp('inst/login_template/', port = 2112)

# Run R CMD CHECK
devtools::check()

# usethis::use_pkgdown_github_pages()
# usethis::use_github_action("README.Rmd")



##### Hex Logo #################################################################
library(hexSticker)
library(showtext)
font_add_google("Gochi Hand", 'gochi')
p <- "man/figures/login2.png" # Generated from DALL-E
hexSticker::sticker(p,
					filename = 'man/figures/login_hex.png',
					p_size = 12,
					package = '',
					url = "jbryer.github.io/login/",
					# p_family = 'gochi',
					u_size = 5.5,
					s_width = 1, s_height = 1,
					s_x = 1, s_y = 0.9,
					p_x = .8, p_y = 0.1,
					# angle = 90,
					p_color = "#206AA5",
					h_fill = '#051E2D',
					h_color = '#113856',
					u_color = '#FEFFE8',
					white_around_sticker = TRUE)

