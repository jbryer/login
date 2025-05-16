# TODO: This is a workaround to the decryption. I am not entirely sure if this
# compromises the encryption of the cookie value. See this issue:
# https://github.com/r-lib/sodium/issues/21
# Could make this a function parameter.
cookie_nonce <- rep(as.raw(42), 24)

#' Encrypt cookie.
#' @param cookie_key encryption key.
#' @param message string to encrypt.
#' @rdname cookie_encryption
#' @export
encrypt_cookie <- function(cookie_key, message) {
	message |>
		charToRaw() |>
		sodium::data_encrypt(key = cookie_key, nonce = cookie_nonce) |>
		sodium::bin2hex()
}

#' Decrypt cookie.
#' @param cookie_key encryption key.
#' @param message string to decrypt.
#' @rdname cookie_encryption
#' @export
decrypt_cookie <- function(cookie_key, message) {
	message |>
		sodium::hex2bin() |>
		sodium::data_decrypt(key = cookie_key, nonce = cookie_nonce) |>
		rawToChar()
}
