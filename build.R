# Build the package
usethis::use_tidy_description()
devtools::document()
devtools::build_vignettes()
devtools::install(build_vignettes = TRUE)
devtools::build()

# Run the Shiny demo app (this only works when working from source)
shiny::runApp('inst/login_demo_simple/', port = 2112)
shiny::runApp('inst/login_template/', port = 2112)
shiny::runApp('inst/login_modal/', port = 2112)
shiny::runApp('inst/user_params/', port = 2112)

devtools::spell_check()

# Run R CMD CHECK
devtools::check()

vignette('login', package = 'login')
vignette('parameters', package = 'login')

devtools::release()

# Build the slides
rmarkdown::render('inst/slides/login.Rmd')
renderthis::to_pdf('inst/slides/login.Rmd', complex_slides = TRUE, partial_slides = FALSE)


# Test the encryption of cookies
passkey <- sodium::sha256(charToRaw('my_secret_password'))
message <- 'sodium is cool!'
nonce <- rep(as.raw(42), 24)
message_encrypted <- message |> charToRaw() |> sodium::data_encrypt(key = passkey,  nonce = nonce)
attributes(message_encrypted)
message_to_send <- message_encrypted |> sodium::bin2hex()
message_received <- message_to_send |> sodium::hex2bin()
sodium::data_decrypt(message_received, key = passkey, nonce = nonce) |> rawToChar()


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

