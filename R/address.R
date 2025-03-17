#' Standardise address format
#'
#' This function standardise inconsistent address formats to a standard format.
#'
#' @param address_vector A character vector that is assumed to be addresses.
#' @param chat A chat object defined by ellmer.
#'
#' @examples
#' # Convert a vector of inconsistent formatted address to a standard format
#' x <- c("68/150 Acton Road, Acton ACT 2601", "655 Jackson St, Dickson ACT 2602", "Unit 60 523 Joey Cct, Layton NSW 6500", "23/100 de burgh road, Southbank VIC 7800", "999 Lords pl, Sydney nsw 6600", "i don't know the address")
#' chat <- chat_ollama(model = "llama3.1:8b", seed = 0, echo = "none")
#' emend_clean_address(x, chat = chat)
#'
#' @export
emend_clean_address <- function(address_vector, chat = NULL){

  if (!is.character(address_vector)) {rlang::abort("Input must be a character vector.")}
  if (is.null(chat)) {rlang::abort("Please provide the chat environment.")}

  chat$set_system_prompt(paste0(
    "You are an expert in address formatting. \n",
    "Your task is to standardize addresses into the following format: \n",
    "**Unit/House Number, Street Name, Suburb, State Abbreviation, Postcode** \n",
    "### Examples: \n",
    "Input: 46 sullivan's creek road, acton act 2601 \n",
    "Output: 46 Sullivan's Creek Rd, Acton ACT 2601 \n",
    "\n ",
    "Input: 403/100 de Burgh Street, Lyneham ACT 2602 \n",
    "Output: 403/100 De Burgh St, Lyneham ACT 2602 \n",
    "\n ",
    "Input: Unit 1 20 Challis St, Dickson ACT 2602 \n",
    "Output: 1/20 Challis St, Dickson ACT 2602 \n",
    "\n",
    "Input: Shop 4/2 Frencham Pl, Downer ACT 2602 \n",
    "Output: 4/2 Frencham Pl, Downer ACT 2602 \n",
    "\n",
    "Input: 36 badham st, Dickson ACT 2602 \n",
    "Output: 36 Badham St, Dickson ACT 2602 \n",
    "### Output Rules: \n",
    "- Output 'INVALID ADDRESS' if not recognised. \n",
    "- Return output only, no explanation or comment. \n"
  ))

  chat$set_turns(list())

  converted <- lapply(address_vector, function(x) {
    response <- chat$chat(paste0(
      "Now process: ", x
    ))
    return(response)
  })

  return(unlist(converted))
}



