#' Search Available Phones
#'
#' Search for phone numbers to purchase.
#'
#' @export
tw_available_phones <- function(area_code = NULL, contains = NULL, sms_enabled = NULL,
                                mms_enabled = NULL, voice_enabled = NULL, fax_enabled = NULL,
                                exclude_all_address_required = NULL, exclude_local_address_required = NULL,
                                exclude_foreign_address_required = NULL, beta = NULL) {
  base_url <- "https://api.twilio.com/"
  ua <- user_agent("https://github.com/seankross/twilio")
  path <- paste("2010-04-01", "Accounts", get_sid(), "AvailablePhoneNumbers", "US", "Local.json", sep = "/")
  url <- modify_url(base_url, path = path, query = list(
    AreaCode = area_code,
    Contains = contains,
    SmsEnabled = sms_enabled,
    MmsEnabled = mms_enabled,
    VoiceEnabled = voice_enabled,
    FaxEnabled = fax_enabled,
    ExcludeAllAddressRequired = exclude_all_address_required,
    ExcludeLocalAddressRequired = exclude_local_address_required,
    ExcludeForeignAddressRequired = exclude_foreign_address_required,
    Beta = beta
  ))
  resp <- GET(url, ua, authenticate(get_sid(), get_token()))

  if (http_type(resp) != "application/json") {
    stop("Twilio API did not return JSON.", call. = FALSE)
  }

  parsed <- fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)

  check_status(resp)

  structure(
    map(parsed$available_phone_numbers, twilio_available_phone),
    class = "twilio_messages_list"
  )
}
