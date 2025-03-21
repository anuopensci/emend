test_that("clean address works", {
  chat_mock <- ellmer::chat_ollama(model = "llama3.1:8b", seed = 0, echo = "none")

  input_1 <- c("154 university avenue, acton act 2601",
             "76/2 Cape Street, Dickson ACT 2602",
             "Shop 4/96 Bunda St, Canberra ACT 2601",
             "11 E Row, Canberra ACT 2601",
             "173/46 Macquarie St, Barton ACT 2600",
             "Unit 189/260 City walk, Canberra ACT 2601",
             "the kebab place",
             "i don't know the address")

  truth_1 <- c("154 University Ave, Acton ACT 2601",
               "76/2 Cape St, Dickson ACT 2602",
               "Shop 4/96 Bunda St, Canberra ACT 2601",
               "11 E Row, Canberra ACT 2601",
               "173/46 Macquarie St, Barton ACT 2600",
               "189/260 City Walk, Canberra ACT 2601",
               "INVALID ADDRESS",
               "INVALID ADDRESS")

  result_1 <- emend_clean_address(input_1, chat = chat_mock)
  expect_equal(result_1, truth_1)
})
