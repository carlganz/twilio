#' Get List of Messages Sent and Received from Your Account
#'
#' Retrieves a list of Twilio SMS and MMS messages sent and receieved from your
#' account.
#'
#' @param page The page number of the list you would like to retrieve. Starts at zero.
#' @param page_size The number of messages per page. The maximum number allowed is 1000.
#' @return A \code{twilio_messages_list} object.
#' @importFrom jsonlite fromJSON
#' @importFrom httr modify_url GET authenticate http_type content user_agent
#' @importFrom purrr map
#' @export
#' @examples
#' \dontrun{
#'
#' # Set API credentials
#' # You only need to do this once per R session
#' Sys.setenv(TWILIO_SID = "M9W4Ozq8BFX94w5St5hikg7UV0lPpH8e56")
#' Sys.setenv(TWILIO_TOKEN = "483H9lE05V0Jr362eq1814Li2N1I424t")
#'
#' # Get messages sent to your account
#' messages <- tw_get_messages_list()
#'
#' }
tw_get_messages_list <- function(page = 0, page_size = 50){
  base_url <- "https://api.twilio.com/"
  ua <- user_agent("https://github.com/seankross/twilio")
  path <- paste("2010-04-01", "Accounts", get_sid(), "Messages.json", sep = "/")
  url <- modify_url(base_url, path = path, query = list(page = page, pagesize = page_size))
  resp <- GET(url, ua, authenticate(get_sid(), get_token()))

  if(http_type(resp) != "application/json"){
    stop("Twilio API did not return JSON.", call. = FALSE)
  }

  parsed <- fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)

  check_status(resp)

  structure(
    map(parsed$messages, twilio_message),
    class = "twilio_messages_list"
  )
}

#' @importFrom dplyr bind_rows
#' @importFrom purrr map_chr
#' @export
tw_tidy_messages <- function(page = 0, page_size = 50, to = NULL, from = NULL) {
  base_url <- "https://api.twilio.com/"
  ua <- user_agent("https://github.com/seankross/twilio")
  path <- paste("2010-04-01", "Accounts", get_sid(), "Messages.json", sep = "/")
  url <- modify_url(base_url, path = path, query = list(page = page, pagesize = page_size, To = to, From = from))
  resp <- GET(url, ua, authenticate(get_sid(), get_token()))

  if(http_type(resp) != "application/json"){
    stop("Twilio API did not return JSON.", call. = FALSE)
  }

  parsed <- fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)

  check_status(resp)

  tibble(
    sid = parsed$messages %>% map_chr("sid", .default = NA_character_),
    txt = parsed$messages %>% map_chr("body", .default = NA_character_),
    to = parsed$messages %>% map_chr("to", .default = NA_character_),
    from = parsed$messages %>% map_chr("from", .default = NA_character_),
    sent = parsed$messages %>% map_chr("date_sent", .default = NA_character_),
    num_media = parsed$messages %>% map_chr("num_media", .default = NA_character_)
  ) %>%
    left_join(
      print(.) %>% filter(num_media > 0) %>% pull("sid") %>% {
       tibble(
        sid = .,
        media_url = map(., tw_get_message_media) %>% map(c(1,4))
       )
      }, by = "sid"
    )
}

