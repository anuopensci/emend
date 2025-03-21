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
emend_translate <- function(text, to = "English", chat = get_default_chat()) {
  if (!is.character(text)) {rlang::abort("Input must be a character vector.")}
  if (is.null(chat)) {rlang::abort("Please provide the chat environment.")}

  chat_clone <- chat$clone(deep = TRUE)
  
  chat_clone$set_system_prompt(paste0(
    "You are a translating assistant. ",
    "Your task is to translate the input to", to, ". ",
    "Output translated text only, no explanation or comment. "
  ))
  
  translated <- lapply(text, function(x){
    chat_clone2 <- chat_clone$clone(deep = TRUE)
    response <- chat_clone2$chat(paste0(
      "Input: ", x
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
emend_what_language <- function(text, chat = get_default_chat()) {
  if (!is.character(text)) {rlang::abort("Input must be a character vector.")}
  if (is.null(chat)) {rlang::abort("Please provide the chat environment.")}

  chat_clone <- chat$clone(deep = TRUE)
  
  chat_clone$set_system_prompt(paste0(
    "You are a translating assistant. ", 
    "Identify what language the input is. ",
    "Output language name only, no explanation or comment. "
  ))
  
  language_types <- lapply(text, function(x){
    chat_clone2 <- chat_clone$clone(deep = TRUE)
    
    response <- chat_clone2$chat(paste0(
      "Input: ", x
    ))
    return(response)
  })

  types <- unlist(language_types)
  return(types)
}
