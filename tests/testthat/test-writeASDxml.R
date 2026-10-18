ref_file <- test_path("aru.27.5b6a_2026_catch_scenarios.xml")

write_ref <- function(inputs = asd_inputs_from_xml(ref_file), ...) {
  out <- tempfile(fileext = ".xml")
  suppressWarnings(
    writeASDxml(inputs$info, inputs$columns, inputs$rows, inputs$values, inputs$notes, file = out, ...)
  )
  out
}

test_that("written file reproduces the Excel template export", {
  local_mocked_bindings(asd_vocab_keys = mock_vocab)
  ref <- read_asd_xml(ref_file)
  new <- read_asd_xml(write_ref())

  expect_equal(new$info, ref$info)
  expect_equal(new$columns, ref$columns)
  # the template writes CS_Label without trimming trailing spaces
  ref$rows$CS_Label <- trimws(ref$rows$CS_Label)
  expect_equal(new$rows, ref$rows)
  expect_equal(new$values, ref$values)
  expect_equal(new$notes, ref$notes)
})

test_that("root element and processing instructions match the template", {
  local_mocked_bindings(asd_vocab_keys = mock_vocab)
  txt <- readLines(write_ref())
  expect_match(txt[2], "^<\\?xml-stylesheet type=['\"]text/xsl['\"] href=['\"]CatchOptionsTable.xsl['\"]\\?>$")
  doc <- xml2::read_xml(write_ref())
  expect_equal(xml2::xml_name(doc), "Assessment")
  expect_equal(
    unname(xml2::xml_attr(doc, "xsi:noNamespaceSchemaLocation", ns = xml2::xml_ns(doc))),
    "CatchOptionsTable.xsd"
  )
})

test_that("IDs follow the template numbering", {
  rows <- asdRows(
    label = c("a", "b", "c", "d", "e"),
    purpose = c("OtherScenarios", "BasisAdvice", "OtherScenarios", "BasisAdvice", "OtherScenarios")
  )
  expect_equal(rows$AR_ID, c(16L, 12L, 17L, 13L, 18L))
  expect_equal(asdColumns(c("x", "y", "z"), 2027, "t")$AK_ID, 5:7)
  expect_equal(asdRows("a", NA, "BasisAdvice")$CS_Basis, "Other")
})

test_that("special characters are escaped once and kept in UTF-8", {
  local_mocked_bindings(asd_vocab_keys = mock_vocab)
  inputs <- asd_inputs_from_xml(ref_file)
  inputs$columns$AK_Label[4] <- "F 6–14"
  inputs$notes$Notes[5] <- "probability of SSB < Blim & more"
  inputs$values[2, 1] <- "<1"

  out <- write_ref(inputs)
  txt <- readLines(out, encoding = "UTF-8")
  expect_true(any(grepl("F 6–14", txt, fixed = TRUE)))
  expect_true(any(grepl("SSB &lt; Blim &amp; more", txt, fixed = TRUE)))

  new <- read_asd_xml(out)
  expect_equal(new$columns$AK_Label[4], "F 6–14")
  expect_equal(new$notes$Notes[5], "probability of SSB < Blim & more")
  expect_equal(new$values$Value[new$values$AR_ID == "16" & new$values$AK_ID == "5"], "<1")
})

test_that("empty cells are left out and numbers are written in full", {
  local_mocked_bindings(asd_vocab_keys = mock_vocab)
  info <- asdInfo("aru.27.5b6a", 2026, "Assessor Name")
  columns <- asdColumns(c("Total catch", "Projected landings"), 2027, "t")
  rows <- asdRows(c("MSY approach", "F = 0"), c("MSY", "Other"), c("BasisAdvice", "OtherScenarios"))
  values <- data.frame(catch = c(1234567890, 0), landings = c(NA, ""))

  new <- read_asd_xml(write_ref(list(info = info, columns = columns, rows = rows, values = values, notes = NULL)))
  expect_equal(nrow(new$values), 2)
  expect_equal(new$values$Value, c("1234567890", "0"))
  expect_equal(new$values$AK_ID, c("5", "5"))
})

test_that("CatchScenarioVariables is written only when given", {
  local_mocked_bindings(asd_vocab_keys = mock_vocab)
  inputs <- asd_inputs_from_xml(ref_file)
  expect_false("CatchScenarioVariables" %in% names(read_asd_xml(write_ref(inputs))$columns))

  inputs$columns$CatchScenarioVariables[1] <- "Catch"
  new <- read_asd_xml(write_ref(inputs))
  expect_equal(new$columns$CatchScenarioVariables[1], "Catch")
  expect_true(all(is.na(new$columns$CatchScenarioVariables[-1])))
})

test_that("errors stop the file being written", {
  local_mocked_bindings(asd_vocab_keys = mock_vocab)
  inputs <- asd_inputs_from_xml(ref_file)
  out <- tempfile(fileext = ".xml")
  expect_error(
    suppressWarnings(writeASDxml(inputs$info, inputs$columns, inputs$rows, inputs$values[, -1], inputs$notes, file = out)),
    "values has 8 rows and 10 columns"
  )
  expect_false(file.exists(out))
})
