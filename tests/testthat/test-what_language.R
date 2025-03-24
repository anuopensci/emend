test_that("emend_what_language identify language works", {
  skip_if_not(system2("which", "ollama", stdout = TRUE) != "")
  chat_mock <- ellmer::chat_ollama(model = "llama3.1:8b", seed = 0, echo = "none")

  input_1 <- c("猿も木から落ちる", "你好", "bon appetit")
  truth_1 <- c("Japanese", "Mandarin Chinese", "French")
  result_1 <- emend_what_language(input_1, chat = chat_mock)
  expect_equal(result_1, truth_1)
})
