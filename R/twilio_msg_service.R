twilio_msg_service <- function(parsed_msg_service) {
  parsed_msg_service$account_sid <- NULL
  parsed_msg_service$url <- NULL
  parsed_msg_service$links <- NULL

  structure(
    parsed_msg_service,
    class = "twilio_msg_service"
  )
}

#' @export
print.twilio_msg_service <- function(x, ...) {
  cat(
    "Message Service: ", x$friendly_name, "\n",
    sep = ""
  )
  invisible(x)
}
