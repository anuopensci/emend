#' Standardise address format
#'
#' This function standardise inconsistent address formats to a standard format.
#'
#' @param address_vector A character vector that is assumed to be addresses.
#' @param chat A chat object defined by ellmer.
#' @return A character vector with converted addresses. 
#'
#' @examples
#' \donttest{
#' # Convert a vector of inconsistent formatted address to a standard format
#' options(ellmer_timeout_s = 3600) 
#' x <- c("154 university avenue, acton act 2601",
#'        "76/2 Cape Street, Dickson ACT 2602",
#'        "Shop 4/96 Bunda St, Canberra ACT 2601",
#'        "11 E Row, Canberra ACT 2601",
#'        "173/46 Macquarie St, Barton ACT 2600",
#'        "Unit 189/260 City walk, Canberra ACT 2601",
#'        "the kebab place",
#'        "i don't know the address")
#' chat <- ellmer::chat_ollama(model = "llama3.1:8b", seed = 0, echo = "none")
#' emend_clean_address(x, chat = chat)
#' } 
#' 
#' @export
emend_clean_address <- function(address_vector, chat = get_default_chat()){

  if (!is.character(address_vector)) {rlang::abort("Input must be a character vector.")}
  if (is.null(chat)) {rlang::abort("Please provide the chat environment.")}

  chat_clone <- chat$clone(deep = TRUE)
  
  chat_clone$set_system_prompt(paste0(
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

  converted <- lapply(address_vector, function(x) {
    chat_clone2 <- chat_clone$clone(deep = TRUE)
    response <- chat_clone2$chat(paste0(
      "Now standardize: ", x
    ))
    return(response)
  })

  return(unlist(converted))
}