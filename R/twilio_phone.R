twilio_phone <- function(parsed_phone) {
  parsed_phone$account_sid <- NULL
  parsed_phone$uri <- NULL
  parsed_phone$subresource_uris <- NULL
  parsed_phone$url <- NULL

  structure(
    parsed_phone,
    class = "twilio_phone"
  )
}

#' @export
print.twilio_phone <- function(x, ...) {
  cat(
    "Phone Number: ", x$phone_number, "\n",
    sep = ""
  )
  invisible(x)
}

twilio_available_phone <- function(parsed_phone) {
  parsed_phone$account_sid <- NULL
  parsed_phone$uri <- NULL
  parsed_phone$subresource_uris <- NULL

  structure(
    parsed_phone,
    class = "twilio_available_phone"
  )
}

#' @export
print.twilio_available_phone <- function(x, ...) {
  cat(
    "Phone Number: ", x$phone_number, "\n",
    "Locality: ", x$locality, ", ", x$region, "\n",
    sep = ""
  )
  invisible(x)
}
