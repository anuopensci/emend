test_that("date cleaning works", {
  chat_mock <- ellmer::chat_ollama(model = "llama3.1:8b", seed = 0, echo = "none")

  input_1 <- c("16/02/1997", "24 Mar 2022", "2000-01-01", "Jason", "Dec 25, 2006", "12/05/2024")
  truth_1 <- as.Date(c("1997-02-16", "2022-03-24", "2000-01-01", "Jason", "2006-12-25", "2024-05-12"), format = "%Y-%m-%d")
  result_1 <- emend_clean_date(input_1, chat = chat_mock)
  expect_equal(result_1, truth_1)
})
