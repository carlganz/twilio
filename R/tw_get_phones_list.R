#' Get a list of all phone numbers associated with Twilio account
#' @export
tw_get_phones_list <- function(phone_number = NULL, friendly_name = NULL, beta = NULL, origin = NULL) {
  base_url <- "https://api.twilio.com/"
  ua <- user_agent("https://github.com/seankross/twilio")
  path <- paste("2010-04-01", "Accounts", get_sid(), "IncomingPhoneNumbers.json", sep = "/")
  url <- modify_url(base_url,
    path = path,
    query = list(
      PhoneNumber = phone_number,
      FriendlyName = friendly_name,
      Beta = beta,
      Origin = origin
    )
  )
  resp <- GET(url, ua, authenticate(get_sid(), get_token()))

  if (http_type(resp) != "application/json") {
    stop("Twilio API did not return JSON.", call. = FALSE)
  }

  parsed <- fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)

  check_status(resp)

  structure(
    map(parsed$incoming_phone_numbers, twilio_phone),
    class = "twilio_phones_list"
  )
}
