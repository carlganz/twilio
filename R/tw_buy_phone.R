#' Add a new phone number to your account
#'
#' To send messages with Twilio you must purchase phone numbers.
#' @param phone_number String representing phone number to purchase. See notes.
#' @note Phone numbers must be in E.164 format. '*' represents a wildcard digit.
#' @export
tw_buy_phone_number <- function(phone_number) {
  base_url <- "https://api.twilio.com/"
  ua <- user_agent("https://github.com/seankross/twilio")
  path <- paste("2010-04-01", "Accounts", get_sid(), "IncomingPhoneNumbers.json", sep = "/")
  url <- modify_url(base_url, path = path)
  resp <- POST(url,
               ua,
               authenticate(get_sid(), get_token()),
               body = list(PhoneNumber = phone_number))

  if (http_type(resp) != "application/json") {
    stop("Twilio API did not return JSON.", call. = FALSE)
  }

  parsed <-
    fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)

  check_status(resp)

  twilio_phone(parsed)
}
