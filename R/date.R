#' Standardise date format
#'
#' This function standardise inconsistent date formats.
#'
#' @param dates_vector A character vector that is assumed to be dates.
#' @param chat A chat object defined by ellmer.
#' @return A vector of Date objects. 
#'
#' @examples
#' x <- c("16/02/1997", "20 November 2024", "24 Mar 2022", "2000-01-01", "Jason", 
#'        "Dec 25, 2030", "11/05/2024", "March 10, 1999")
#' chat <- ellmer::chat_ollama(model = "llama3.1:8b", seed = 0, echo = "none")
#' emend_clean_date(x, chat = chat)
#'
#' @export
emend_clean_date <- function(dates_vector, chat = get_default_chat()) {

  if (!is.character(dates_vector)) {rlang::abort("Input must be a character vector.")}
  if (is.null(chat)) {rlang::abort("Please provide the chat environment.")}

  chat_clone <- chat$clone(deep = TRUE)
  
  chat_clone$set_system_prompt(paste0(
    "You are a date formatting assistant. ",
    "Your task is to identify and parse dates in various formats, then convert them into the standard 'YYYY-MM-DD' format. ",
    "If a string is not a recognizable date, return 'INVALID'. ",
    "Always ensure correct parsing, considering different date formats such as 'DD/MM/YYYY', 'Month DD, YYYY', 'YYYY-MM-DD', 'DD Mon YYYY', and similar variations. ",
    "Output result only."
  ))

  converted <- lapply(dates_vector, function(x) {
    chat_clone2 <- chat_clone$clone(deep = TRUE)
    response <- chat_clone2$chat(paste0(
      "Standardize this date: ", x
    ))
    return(response)
  })

  new_dates <- as.Date(unlist(converted), format = "%Y-%m-%d")
  return(new_dates)
}