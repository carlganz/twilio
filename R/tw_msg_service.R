#' @export
tw_create_msg_service <- function(friendly_name, status_callback) {
  url <- "https://messaging.twilio.com/v1/Services"
  ua <- user_agent("https://github.com/seankross/twilio")
  resp <- POST(url, ua, authenticate(get_sid(), get_token()),
    body = list(
      FriendlyName = friendly_name,
      StatusCallback = status_callback
    )
  )

  if (http_type(resp) != "application/json") {
    stop("Twilio API did not return JSON.", call. = FALSE)
  }

  parsed <- fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)

  check_status(resp)

  twilio_msg_service(parsed)
}

#' @export
tw_add_phone_msg_service <- function(msg_service_sid, phone_sid) {
  base_url <- "https://messaging.twilio.com/"
  ua <- user_agent("https://github.com/seankross/twilio")
  path <- paste("v1", "Services", msg_service_sid, "PhoneNumbers", sep = "/")
  url <- modify_url(base_url, path = path)
  resp <- POST(url, ua, authenticate(get_sid(), get_token()),
    body = list(
      PhoneNumberSid = phone_sid
    )
  )

  if (http_type(resp) != "application/json") {
    stop("Twilio API did not return JSON.", call. = FALSE)
  }

  parsed <- fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)

  check_status(resp)
  # was phone succesfully added
  invisible(!is.null(parsed$sid))
}

#' @export
tw_msg_service_phones_list <- function(msg_service_sid) {
  base_url <- "https://messaging.twilio.com/"
  ua <- user_agent("https://github.com/seankross/twilio")
  path <- paste("v1", "Services", msg_service_sid, "PhoneNumbers", sep = "/")
  url <- modify_url(base_url, path = path)
  resp <- GET(url, ua, authenticate(get_sid(), get_token()))

  if (http_type(resp) != "application/json") {
    stop("Twilio API did not return JSON.", call. = FALSE)
  }

  parsed <- fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)

  check_status(resp)

  structure(
    map(parsed$phone_numbers, twilio_phone)
  )
}

#' @importFrom httr DELETE
#' @export
tw_remove_phone_msg_service <- function(msg_service_sid, phone_sid) {
  base_url <- "https://messaging.twilio.com/"
  ua <- user_agent("https://github.com/seankross/twilio")
  path <- paste("v1", "Services", msg_service_sid, "PhoneNumbers", phone_sid, sep = "/")
  url <- modify_url(base_url, path = path)
  resp <- DELETE(url, ua, authenticate(get_sid(), get_token()))

  if (http_type(resp) != "application/json") {
    stop("Twilio API did not return JSON.", call. = FALSE)
  }

  parsed <- fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)

  check_status(resp)
  # was phone succesfully removed
  invisible(!is.null(parsed$sid))
}

#' @export
tw_msg_service_list <- function() {
  url <- "https://messaging.twilio.com/v1/Services"
  ua <- user_agent("https://github.com/seankross/twilio")

  resp <- GET(url, ua, authenticate(get_sid(), get_token()))

  if (http_type(resp) != "application/json") {
    stop("Twilio API did not return JSON.", call. = FALSE)
  }

  parsed <- fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)

  check_status(resp)

  structure(
    map(parsed$services, twilio_msg_service)
  )
}
