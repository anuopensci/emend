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
               "4/96 Bunda St, Canberra ACT 2601",
               "11 E Row, Canberra ACT 2601",
               "173/46 Macquarie St, Barton ACT 2600",
               "189/260 City Walk, Canberra ACT 2601",
               "INVALID ADDRESS",
               "INVALID ADDRESS")

  result_1 <- emend_clean_address(input_1, chat = chat_mock)
  expect_equal(result_1, truth_1)

  input_2 <- c("68/150 Acton Road, Acton ACT 2601",
               "655 Jackson St, Dickson ACT 2602",
               "Unit 60 523 Joey Cct, Layton NSW 6500",
               "23/100 de burgh road, Southbank VIC 7800",
               "999 Lords pl, Sydney nsw 6600",
               "i don't know the address")

  truth_2 <- c("68/150 Acton Rd, Acton ACT 2601",
               "655 Jackson St, Dickson ACT 2602",
               "INVALID ADDRESS",
               "23/100 De Burgh Rd, Southbank VIC 7800",
               "INVALID ADDRESS",
               "INVALID ADDRESS")
  result_2 <- emend_clean_address(input_2, chat = chat_mock)
  expect_equal(result_2, truth_2)
})
