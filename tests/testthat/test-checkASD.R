ref_file <- test_path("aru.27.5b6a_2026_catch_scenarios.xml")

check_ref <- function(modify = function(x) x) {
  x <- modify(asd_inputs_from_xml(ref_file))
  checkASD(x$info, x$columns, x$rows, x$values, x$notes)
}

test_that("reference file has no errors, and unused notes are flagged", {
  local_mocked_bindings(asd_vocab_keys = mock_vocab)
  res <- check_ref()
  expect_length(res$errors, 0)
  expect_setequal(
    res$warnings,
    c("note '***' is not used in any label", "note '^^^' is not used in any label")
  )
})

test_that("invalid vocabulary codes are errors", {
  local_mocked_bindings(asd_vocab_keys = mock_vocab)
  expect_match(check_ref(function(x) { x$info$stockCode <- "aru.27.xx"; x })$errors, "ICES_StockCode", all = FALSE)
  expect_match(check_ref(function(x) { x$columns$Unit[1] <- "tonnes"; x })$errors, "MUNIT", all = FALSE)
  expect_match(check_ref(function(x) { x$rows$CS_Basis[2] <- "Maybe"; x })$errors, "CatchScenariosBasis", all = FALSE)
  expect_match(check_ref(function(x) { x$columns$CatchScenarioVariables[1] <- "Landings"; x })$errors, "StockDataType", all = FALSE)
})

test_that("vocabulary that cannot be downloaded is a warning", {
  local_mocked_bindings(asd_vocab_keys = function(code_type) NULL)
  res <- check_ref()
  expect_length(res$errors, 0)
  expect_match(res$warnings, "could not download", all = FALSE)
})

test_that("table layout is checked", {
  local_mocked_bindings(asd_vocab_keys = mock_vocab)
  expect_match(
    check_ref(function(x) { x$rows$CS_Purpose[1] <- "OtherScenarios"; x })$errors,
    "no basis of advice row", all = FALSE
  )
  expect_match(
    check_ref(function(x) { x$rows$CS_Purpose[1:4] <- "BasisAdvice"; x })$errors,
    "4 basis of advice rows", all = FALSE
  )
  expect_match(
    check_ref(function(x) { x$rows$CS_Purpose[2] <- "Scenario"; x })$errors,
    "purpose 'Scenario'", all = FALSE
  )
  expect_match(
    check_ref(function(x) { x$columns$AK_Label[3] <- ""; x })$errors,
    "column 3 has no label", all = FALSE
  )
  expect_match(
    check_ref(function(x) { x$values <- x$values[-1, ]; x })$errors,
    "values has 7 rows", all = FALSE
  )
})

test_that("stock information is checked", {
  local_mocked_bindings(asd_vocab_keys = mock_vocab)
  expect_match(check_ref(function(x) { x$info$contact <- ""; x })$errors, "contact is missing", all = FALSE)
  next_year <- as.numeric(format(Sys.Date(), "%Y")) + 1
  expect_match(
    check_ref(function(x) { x$info$AssessmentYear <- next_year; x })$errors,
    "later than the present year", all = FALSE
  )
})

test_that("footnote symbols without notes are flagged", {
  local_mocked_bindings(asd_vocab_keys = mock_vocab)
  res <- check_ref(function(x) { x$rows$CS_Label[2] <- "F = 0 ##"; x })
  expect_match(res$warnings, "symbol '##' is used in a label but has no note", all = FALSE)
})
