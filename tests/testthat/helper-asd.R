# stand-in for the ICES vocabularies so tests run offline
mock_vocab <- function(code_type) {
  switch(code_type,
    ICES_StockCode = c("aru.27.5b6a", "her.27.3a47d"),
    MUNIT = c("NE9", "NE6", "NE3", "N", "t", "kg", "%", "ratio", "fd", "kWd", "nd"),
    StockDataType = c("Catch", "Effort", "FishingPressure", "RecreationalCatch", "RecrPressure", "Recruitment", "StockSize"),
    CatchScenariosBasis = c("FMSYranges", "MP", "MSY", "NoAdv", "Other", "PA"),
    NULL
  )
}

# read an ASD xml file into a list of tables
read_asd_xml <- function(file) {
  doc <- xml2::read_xml(file)
  section <- function(name) {
    nodes <- xml2::xml_find_all(doc, paste0("/Assessment/", name))
    out <- lapply(nodes, function(n) {
      kids <- xml2::xml_children(n)
      stats::setNames(as.list(xml2::xml_text(kids)), xml2::xml_name(kids))
    })
    cols <- unique(unlist(lapply(out, names)))
    df <- lapply(cols, function(cl) vapply(out, function(x) if (is.null(x[[cl]])) NA_character_ else x[[cl]], character(1)))
    df <- as.data.frame(stats::setNames(df, cols), stringsAsFactors = FALSE)
    df
  }
  top <- function(name) xml2::xml_text(xml2::xml_find_first(doc, paste0("/Assessment/", name)))
  list(
    info = list(stockCode = top("stockCode"), AssessmentYear = top("AssessmentYear"),
                Purpose = top("Purpose"), contact = top("contact")),
    columns = section("Scenarios_Columns"),
    rows = section("Scenarios_Rows"),
    values = section("Scenarios_Values"),
    notes = section("Catch_Scenario_Notes_Notes")
  )
}

# convert the parsed file into inputs for writeASDxml
asd_inputs_from_xml <- function(file) {
  x <- read_asd_xml(file)
  values <- matrix(NA_character_, nrow(x$rows), nrow(x$columns))
  for (k in seq_len(nrow(x$values))) {
    i <- match(x$values$AR_ID[k], x$rows$AR_ID)
    j <- match(x$values$AK_ID[k], x$columns$AK_ID)
    values[i, j] <- x$values$Value[k]
  }
  list(
    info = asdInfo(x$info$stockCode, as.numeric(x$info$AssessmentYear), x$info$contact),
    columns = asdColumns(x$columns$AK_Label, x$columns$YearLabel, x$columns$Unit),
    rows = asdRows(x$rows$CS_Label, x$rows$CS_Purpose, x$rows$CS_Basis),
    values = as.data.frame(values, stringsAsFactors = FALSE),
    notes = asdNotes(x$notes$Symbol, x$notes$Notes)
  )
}
