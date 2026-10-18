#' Write a catch scenarios upload file
#'
#' Write a catch scenarios table to the XML format uploaded to the ICES Advice
#' and Scenarios Database at \url{https://asd.ices.dk}.  The layout matches the
#' file produced by the ICES catch scenarios Excel template.
#'
#' By default the data are checked with \code{\link{checkASD}} before the file
#' is written.  Errors stop the file being written, warnings are reported.
#'
#' @param info stock information, see \code{\link{asdInfo}}.
#' @param columns column headers, see \code{\link{asdColumns}}.
#' @param rows row headers, see \code{\link{asdRows}}.
#' @param values the body of the table: a data frame or matrix with one row per
#'               scenario (in the order of \code{rows}) and one column per column
#'               header (in the order of \code{columns}).  Values can be numbers or
#'               text (e.g. "<1"); empty cells (\code{NA} or "") are left out.
#' @param notes footnotes, see \code{\link{asdNotes}}.
#' @param assumptions optional forecast assumptions, see \code{\link{asdAssumptions}}.
#'                    These are not yet part of the ASD upload format, so a file
#'                    that includes them is for testing the proposal only.
#' @param file the name of the xml file to write.  Defaults to
#'             \code{<stockCode>_<AssessmentYear>_catch_scenarios.xml}.
#' @param check should the data be checked before writing? Default is TRUE.
#'
#' @return the file name, invisibly.
#'
#' @seealso \code{\link{asdInfo}}, \code{\link{checkASD}}
#'
#' @examples
#' \dontrun{
#' info <- asdInfo("aru.27.5b6a", 2026, "Assessor Name")
#' columns <- asdColumns(c("Total catch", "Spawning-stock biomass"), c(2027, 2028), c("t", "t"))
#' rows <- asdRows(c("MSY approach: FMSY", "F = 0"), c("MSY", "Other"), c("BasisAdvice", "OtherScenarios"))
#' values <- data.frame(catch = c(22865, 0), ssb = c(91306, 111006))
#' writeASDxml(info, columns, rows, values)
#' }
#' @export
writeASDxml <- function(info, columns, rows, values, notes = NULL,
                        assumptions = NULL, file = NULL, check = TRUE) {
  x <- asd_prepare(info, columns, rows, values, notes, assumptions)

  if (check) {
    res <- checkASD(info, columns, rows, values, notes, assumptions)
    if (length(res$warnings)) {
      warning(paste0("\n  ", res$warnings, collapse = ""), call. = FALSE)
    }
    if (length(res$errors)) {
      stop(
        "The file was not written, please fix the following errors:",
        paste0("\n  ", res$errors, collapse = ""),
        call. = FALSE
      )
    }
  } else if (!identical(dim(x$values), c(nrow(x$rows), nrow(x$columns)))) {
    stop("values must have one row per scenario and one column per column header", call. = FALSE)
  }

  if (is.null(file)) {
    file <- paste0(x$info$stockCode, "_", x$info$AssessmentYear, "_catch_scenarios.xml")
  }

  doc <- xml2::read_xml(
    paste0(
      "<?xml version='1.0' encoding='utf-8' standalone='no'?>",
      "<?xml-stylesheet type='text/xsl' href='CatchOptionsTable.xsl'?>",
      "<Assessment xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' ",
      "xsi:noNamespaceSchemaLocation='CatchOptionsTable.xsd'/>"
    )
  )

  asd_add_values(doc, unclass(x$info))

  cols <- x$columns[order(x$columns$AK_ID), ]
  for (i in seq_len(nrow(cols))) {
    asd_add_values(xml2::xml_add_child(doc, "Scenarios_Columns"), as.list(cols[i, ]))
  }

  ord <- order(x$rows$AR_ID)
  for (i in ord) {
    asd_add_values(xml2::xml_add_child(doc, "Scenarios_Rows"), as.list(x$rows[i, ]))
  }

  for (i in ord) {
    for (j in order(x$columns$AK_ID)) {
      value <- x$values[i, j]
      if (is.na(value)) next
      asd_add_values(
        xml2::xml_add_child(doc, "Scenarios_Values"),
        list(AR_ID = x$rows$AR_ID[i], AK_ID = x$columns$AK_ID[j], Value = value)
      )
    }
  }

  if (!is.null(x$notes)) {
    for (i in seq_len(nrow(x$notes))) {
      asd_add_values(xml2::xml_add_child(doc, "Catch_Scenario_Notes_Notes"), as.list(x$notes[i, ]))
    }
  }

  if (!is.null(x$assumptions)) {
    for (i in seq_len(nrow(x$assumptions))) {
      asd_add_values(xml2::xml_add_child(doc, "Forecast_Assumptions"), as.list(x$assumptions[i, ]))
    }
  }

  xml2::write_xml(doc, file, encoding = "utf-8")

  invisible(file)
}

# standardise the inputs: rebuild headers (and IDs) with the constructors and
# convert values to a character matrix with NA for empty cells
asd_prepare <- function(info, columns, rows, values, notes, assumptions = NULL) {
  info <- unclass(info)
  info <- do.call(asdInfo, info[intersect(names(info), c("stockCode", "AssessmentYear", "contact"))])

  columns <- asd_rebuild(columns, asdColumns, c(AK_Label = "label", YearLabel = "year", Unit = "unit", CatchScenarioVariables = "type"))
  rows <- asd_rebuild(rows, asdRows, c(CS_Label = "label", CS_Basis = "basis", CS_Purpose = "purpose"))

  if (!is.null(notes)) {
    notes <- as.data.frame(notes, stringsAsFactors = FALSE)[c("Symbol", "Notes")]
  }

  if (!is.null(assumptions)) {
    assumptions <- asd_rebuild(assumptions, asdAssumptions, c(
      Variable = "variable", Ages = "ages", Year = "year", Value = "value", Unit = "unit",
      Basis = "basis", YearFrom = "from", YearTo = "to", Notes = "note"
    ))
  }

  values <- as.data.frame(values, stringsAsFactors = FALSE)
  m <- matrix(NA_character_, nrow = nrow(values), ncol = ncol(values))
  for (j in seq_along(values)) {
    m[, j] <- vapply(values[[j]], asd_format, character(1))
  }
  values <- m

  list(info = info, columns = columns, rows = rows, values = values, notes = notes,
       assumptions = assumptions)
}

# call a constructor on a data frame it made, mapping the file names back to
# the argument names
asd_rebuild <- function(x, fun, map) {
  x <- as.list(as.data.frame(x, stringsAsFactors = FALSE))
  x <- x[intersect(names(map), names(x))]
  names(x) <- map[names(x)]
  do.call(fun, x)
}

# one value as text, NA for empty; numbers written in full
asd_format <- function(x) {
  if (length(x) != 1 || is.na(x)) {
    return(NA_character_)
  }
  out <- if (is.numeric(x)) format(x, scientific = FALSE, digits = 15, trim = TRUE) else trimws(as.character(x))
  if (nzchar(out)) out else NA_character_
}

# add one child element per non-empty value in a named list
asd_add_values <- function(node, x) {
  for (nm in names(x)) {
    value <- asd_format(x[[nm]])
    if (is.na(value)) next
    xml2::xml_add_child(node, nm, value)
  }
  invisible(node)
}
