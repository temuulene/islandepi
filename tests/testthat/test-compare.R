new_output <- data.frame(
  hsda = c("South", "Central", "North"),
  cases = c(412, 251, 127),
  rate = c(88.8, 78.4, 88.6)
)
reference <- data.frame(
  hsda = c("Central", "South", "North Island"),
  cases = c(250, 412, 127),
  rate = c(78.0, 88.8, 88.6)
)

test_that("identical tables give zero rows", {
  out <- islh_compare_outputs(new_output, new_output, by = "hsda")
  expect_equal(nrow(out), 0)
  expect_identical(
    names(out),
    c("hsda", "column", "x", "y", "difference", "status")
  )
})

test_that("row order does not matter", {
  shuffled <- new_output[c(3, 1, 2), ]
  expect_equal(nrow(islh_compare_outputs(new_output, shuffled, "hsda")), 0)
})

test_that("differences and one-sided rows are reported", {
  out <- islh_compare_outputs(new_output, reference, "hsda")
  central <- out[out$hsda == "Central", ]
  expect_equal(central$column, c("cases", "rate"))
  expect_equal(central$difference, c(1, 0.4))
  expect_equal(unique(central$status), "different")
  expect_equal(unique(out$status[out$hsda == "North"]), "only_in_x")
  expect_equal(unique(out$status[out$hsda == "North Island"]), "only_in_y")
  expect_false("South" %in% out$hsda)
})

test_that("tolerance absorbs rounding but not real differences", {
  rounded <- new_output
  rounded$rate <- rounded$rate + c(0.05, -0.04, 0)
  expect_equal(
    nrow(islh_compare_outputs(new_output, rounded, "hsda", tolerance = 0.05)),
    0
  )
  expect_equal(nrow(islh_compare_outputs(new_output, rounded, "hsda")), 2)
})

test_that("all = TRUE keeps matching cells", {
  out <- islh_compare_outputs(new_output, new_output, "hsda", all = TRUE)
  expect_equal(nrow(out), 6)
  expect_equal(unique(out$status), "match")
})

test_that("text columns compare exactly", {
  suppressed <- new_output
  suppressed$cases <- as.character(suppressed$cases)
  suppressed$cases[3] <- "Suppressed"
  out <- islh_compare_outputs(new_output, suppressed, "hsda", values = "cases")
  expect_equal(out$hsda, "North")
  expect_type(out$x, "character")
  expect_equal(out$y, "Suppressed")
})

test_that("missing values match each other but not a number", {
  a <- data.frame(k = 1:2, v = c(NA, NA))
  b <- data.frame(k = 1:2, v = c(NA, 5))
  out <- islh_compare_outputs(a, b, "k")
  expect_equal(out$k, 2L)
  expect_equal(out$status, "different")
})

test_that("columns only one table has are named, not compared", {
  wider <- new_output
  wider$note <- "x"
  expect_message(
    islh_compare_outputs(wider, new_output, "hsda"),
    "note",
    class = "islh_message"
  )
})

test_that("ambiguous comparisons are refused", {
  doubled <- rbind(new_output, new_output)
  expect_error(
    islh_compare_outputs(doubled, new_output, "hsda"),
    "more than one row",
    class = "islh_error"
  )
  coded <- data.frame(code = c("011", "012"), n = 1:2)
  numbers <- data.frame(code = c(11, 12), n = 1:2)
  expect_error(islh_compare_outputs(coded, numbers, "code"), "different types")
  expect_error(
    islh_compare_outputs(new_output, reference, "hsda", values = "missing"),
    "no column"
  )
  expect_error(
    islh_compare_outputs(new_output, reference, "hsda", tolerance = -1),
    "tolerance"
  )
  only_keys <- new_output["hsda"]
  expect_error(islh_compare_outputs(only_keys, only_keys, "hsda"), "share no")
})
