#' Standardise date format
#'
#' This function standardise inconsistent date formats.
#'
#' @param dates_vector A character vector that is assumed to be dates.
#' @param input_format A character value to specify input date format.
#' @param chat A chat object defined by ellmer.
#'
#' @examples
#' x <- c("16/02/1997", "20 November 2024", "24 Mar 2022", "2000-01-01", "Jason", "Dec 25, 2030", "12/05/2024")
#' chat <- ellmer::chat_ollama(model = "llama3.1:8b", seed = 0, echo = "none")
#' emend__clean_date(x, chat = chat)
#'
#' # To specify the input date format:
#' x <- c("12/05/2024", "11/15/2024", "02/25/2024")
#' emend_clean_date(x, input_format = "MM/DD/YYYY")
#'
#' @export
emend_clean_date <- function(dates_vector, input_format = "DD-MM-YYYY", chat = NULL) {

  if (!is.character(dates_vector)) {rlang::abort("Input must be a character vector.")}
  if (is.null(chat)) {rlang::abort("Please provide the chat environment.")}

  chat$set_system_prompt(paste0(
    "You are an expert in recognizing and formatting dates. ",
    "Your task is to convert a given date into the standard format YYYY-MM-DD. ",
    "- Output only the converted date, with no extra text, explanations, or comments.",
    "- Output 'INVALID' for unrecognised date.",
    "- When the input date is in format XX-XX-YYYY, interpret it as ", input_format, "."
  ))

  chat$set_turns(list())

  converted <- lapply(dates_vector, function(x) {
    response <- chat$chat(paste0(
      "Now process: ", x
    ))
    return(response)
  })

  new_dates <- as.Date(unlist(converted), format = "%Y-%m-%d")
  return(new_dates)
}


