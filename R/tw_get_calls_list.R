#' Get List of Calls Sent and Received from Your Account
#'
#' Retrieves a list of Twilio calls sent and received from your
#' account.
#'
#' @param page The page number of the list you would like to retrieve. Starts at zero.
#' @param page_size The number of calls per page. The maximum number allowed is 1000.
#' @param status The status of the calls to include. Can be: "queued", "ringing", "in-progress",
#'   "canceled", "completed", "failed", "busy", or "no-answer".
#' @param start_time Only include calls that started on this date. Specify a date as YYYY-MM-DD in
#'   GMT, for example: `2009-07-06`, to read only calls that started on this date. You can also
#'   specify an inequality, such as `<=YYYY-MM-DD`, to read calls that started on or before
#'   midnight of this date, and `>=YYYY-MM-DD` to read calls that started on or after
#'   midnight of this date.
#' @param end_time Only include calls that ended on this date. Specify a date as YYYY-MM-DD in GMT,
#'   for example: `2009-07-06`, to read only calls that ended on this date. You can also specify an
#'   inequality, such as `<=YYYY-MM-DD`, to read calls that ended on or before midnight of
#'   this date, and `>=YYYY-MM-DD` to read calls that ended on or after midnight of this date.
#'
#' @return A \code{twilio_calls_list} object.
#'
#' @examples
#' \dontrun{
#' # Set API credentials
#' # You only need to do this once per R session
#' Sys.setenv(TWILIO_SID = "M9W4Ozq8BFX94w5St5hikg7UV0lPpH8e56")
#' Sys.setenv(TWILIO_TOKEN = "483H9lE05V0Jr362eq1814Li2N1I424t")
#'
#' # Get calls sent to your account
#' calls <- tw_get_calls_list()
#' }
#'
#' @importFrom jsonlite fromJSON
#' @importFrom httr modify_url GET authenticate http_type content user_agent
#' @importFrom purrr map
#'
#' @export
#'
tw_get_calls_list <- function(
                              page = 0, page_size = 50, status = "", start_time = "",
                              end_time = "") {
  base_url <- "https://api.twilio.com/"
  ua <- user_agent("https://github.com/seankross/twilio")
  path <- paste("2010-04-01", "Accounts", get_sid(), "Calls.json", sep = "/")
  url <- modify_url(base_url, path = path, query = list(page = page, pagesize = page_size))
  url <- paste0(
    url,
    ifelse(nchar(status) > 0, paste0("&Status=", status), ""),
    ifelse(nchar(start_time) > 0, paste0("&StartTime=", start_time), ""),
    ifelse(nchar(end_time) > 0, paste0("&EndTime=", end_time), "")
  )
  resp <- GET(url, ua, authenticate(get_sid(), get_token()))

  if (http_type(resp) != "application/json") {
    stop("Twilio API did not return JSON.", call. = FALSE)
  }

  parsed <- fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)

  check_status(resp)

  structure(
    map(parsed$calls, twilio_message),
    class = "twilio_calls_list"
  )
}
