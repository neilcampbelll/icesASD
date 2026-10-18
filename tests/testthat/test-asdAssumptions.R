ref_file <- test_path("aru.27.5b6a_2026_catch_scenarios.xml")

check_assumptions <- function(modify = function(a) a) {
  x <- asd_inputs_from_xml(ref_file)
  res <- checkASD(x$info, x$columns, x$rows, x$values, x$notes, modify(aru_assumptions()))
  # leave out the two warnings the reference table already has
  res$warnings <- grep("^note ", res$warnings, value = TRUE, invert = TRUE)
  res
}

test_that("asdAssumptions recycles and orders its fields", {
  a <- asdAssumptions("Recruitment", year = 2026:2028, value = 107501, ages = "5")
  expect_equal(nrow(a), 3)
  expect_equal(
    names(a),
    c("Variable", "Ages", "Year", "Value", "Unit", "Basis", "YearFrom", "YearTo", "Notes")
  )
  expect_equal(a$Ages, rep("5", 3))
})

test_that("Table 1 for aru.27.5b6a passes the checks", {
  local_mocked_bindings(asd_vocab_keys = mock_vocab)
  res <- check_assumptions()
  expect_length(res$errors, 0)
  expect_length(res$warnings, 0)
})

test_that("codes outside the provisional lists are warnings", {
  local_mocked_bindings(asd_vocab_keys = mock_vocab)
  res <- check_assumptions(function(a) { a$Variable[3] <- "SSB"; a$Basis[1] <- "Guess"; a })
  expect_length(res$errors, 0)
  expect_match(res$warnings, "variable 'SSB' is not one of the provisional codes", all = FALSE)
  expect_match(res$warnings, "basis 'Guess' is not one of the provisional codes", all = FALSE)
})

test_that("values, years and units are checked", {
  local_mocked_bindings(asd_vocab_keys = mock_vocab)
  expect_match(check_assumptions(function(a) { a$Year[1] <- NA; a })$errors, "assumption 1 \\(FTotal\\) has no year", all = FALSE)
  expect_match(check_assumptions(function(a) { a$YearTo[4] <- NA; a })$errors, "needs both from and to", all = FALSE)
  expect_match(check_assumptions(function(a) { a$YearFrom[4] <- 2026; a })$errors, "from \\(2026\\) is after to \\(2025\\)", all = FALSE)
  expect_match(check_assumptions(function(a) { a$Unit[3] <- "tonnes"; a })$errors, "MUNIT", all = FALSE)
  expect_match(check_assumptions(function(a) { a$Year[5] <- 2026; a })$errors, "'Recruitment 5 2026' is given more than once", all = FALSE)
  expect_match(check_assumptions(function(a) { a$YearFrom[4:6] <- NA; a$YearTo[4:6] <- NA; a })$warnings, "basis 'Resampled' needs the years used", all = FALSE)
  expect_match(check_assumptions(function(a) { a$Notes[8] <- NA; a })$warnings, "assumption 8 \\(CatchWanted\\) has no value and no note", all = FALSE)
})

test_that("assumptions are written only when given, after the notes", {
  local_mocked_bindings(asd_vocab_keys = mock_vocab)
  x <- asd_inputs_from_xml(ref_file)
  out <- tempfile(fileext = ".xml")
  suppressWarnings(writeASDxml(x$info, x$columns, x$rows, x$values, x$notes, file = out))
  expect_equal(nrow(read_asd_xml(out)$assumptions), 0)

  suppressWarnings(writeASDxml(x$info, x$columns, x$rows, x$values, x$notes, aru_assumptions(), file = out))
  new <- read_asd_xml(out)

  a <- aru_assumptions()
  expect_equal(nrow(new$assumptions), nrow(a))
  expect_equal(new$assumptions$Variable, a$Variable)
  expect_equal(new$assumptions$Value, c("0.178", NA, "96380", "107501", "107501", "107501", "17601", NA, NA))
  expect_equal(new$assumptions$YearFrom, as.character(a$YearFrom))
  expect_equal(new$assumptions$Notes, a$Notes)

  kids <- xml2::xml_name(xml2::xml_children(xml2::read_xml(out)))
  expect_true(max(which(kids == "Catch_Scenario_Notes_Notes")) < min(which(kids == "Forecast_Assumptions")))
})
