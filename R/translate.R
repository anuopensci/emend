#' Translate text from one language to another.
#'
#' @param text The text to translate.
#' @param to The language to translate to. The default is "English".
#' @param chat An ellmer Chat object.
#'
#' @examples
#' chat <- ellmer::chat_ollama(model = "llama3.1:8b", seed = 0, echo = "none")
#' emend_translate(c("猿も木から落ちる", "你好", "bon appetit"), chat = chat)
#'
#' @export
emend_translate <- function(text, to = "English", chat = NULL) {

  if (!is.character(text)) {
    rlang::abort("Input must be a character vector.")
  }

  if (is.null(chat)) {
    rlang::abort("Please provide the chat environment.")
  }

  chat$clone()
  chat$set_turns(list())

  translated <- lapply(text, function(x){
    response <- chat$chat(paste0(
      "For '", x, "' translate it to ", to, ". ",
      "Return translated text only, no explanation or comment."
    ))
    return(response)
  })

  translated_text <- unlist(translated)
  return(translated_text)
}

#' Identify the language in the text.
#' @param text A string or a factor that contains text information.
#' @param chat A chat object defined by ellmer
#'
#' @examples
#' chat <- ellmer::chat_ollama(model = "llama3.1:8b", seed = 0, echo = "none")
#' emend_what_language(c("猿も木から落ちる", "你好", "bon appetit"), chat = chat)
#'
#' @export
emend_what_language <- function(text, chat = NULL) {

  if (!is.character(text)) {
    rlang::abort("Input must be a character vector.")
  }

  if (is.null(chat)) {
    rlang::abort("Please provide the chat environment.")
  }

  chat$clone()
  chat$set_turns(list())

  language_types <- lapply(text, function(x){
    response <- chat$chat(paste0(
      "For '", x, "' identify which language it is. ",
      "Return language type only, no explanation or comment."
    ))
    return(response)
  })

  types <- unlist(language_types)
  return(types)
}
