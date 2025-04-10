test_that("emend_get_levels Extract unique levels of messy input", {
  skip_if_not(system2("which", "ollama", stdout = TRUE) != "")
  chat_mock <- ellmer::chat_ollama(model = "llama3.1:8b", seed = 0, echo = "none")

  truth_1 <- c("United Kingdom", "United States", "Canada", "New Zealand", "Australia")
  result_1 <- emend_lvl_unique(messy$country, chat = chat_mock)
  expect_equal(result_1, truth_1)
})
