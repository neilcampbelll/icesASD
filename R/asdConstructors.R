#' Create the parts of a catch scenarios upload file
#'
#' Helper functions to create the stock information, column and row headers,
#' and notes of a catch scenarios table, for writing with \code{\link{writeASDxml}}.
#'
#' The arguments follow the labels of the ICES catch scenarios Excel template.
#' The data frames returned use the names of the XML file, so they can be
#' compared with an exported file.  The column and row IDs (\code{AK_ID},
#' \code{AR_ID}) are the column and row numbers of the template: columns are
#' numbered from 5 (column E), basis of advice rows 12 to 14, and other
#' scenario rows from 16.
#'
#' @param stockCode the ICES stock code, e.g. "aru.27.5b6a".
#' @param AssessmentYear the assessment year.
#' @param contact the name of the contact person.
#' @param label for columns, the table headers; for rows, the scenario labels.
#'              Footnote symbols are part of the label.
#' @param year the year each column refers to.  For \% change columns give the
#'             last year.
#' @param unit units of each column, as ICES MUNIT codes (e.g. "t", "ratio", "\%").
#' @param type optional type of each column, as codes from the ICES
#'             StockDataType vocabulary (e.g. "Catch", "FishingPressure",
#'             "StockSize").  Left out of the file when NA.
#' @param basis the basis of each scenario, as codes from the ICES
#'              CatchScenariosBasis vocabulary (e.g. "MSY", "MP", "PA", "Other").
#'              Empty entries are set to "Other".
#' @param purpose "BasisAdvice" for the basis of the advice (up to three rows),
#'                or "OtherScenarios".
#' @param symbol footnote symbols, e.g. "*", "**", "^".
#' @param note the footnote text for each symbol.
#'
#' @return \code{asdInfo} returns a list, the others return data frames.
#'
#' @seealso \code{\link{writeASDxml}}, \code{\link{checkASD}}
#'
#' @examples
#' info <- asdInfo("aru.27.5b6a", 2026, "Assessor Name")
#'
#' columns <- asdColumns(
#'   label = c("Total catch", "F 6–14", "Spawning-stock biomass"),
#'   year = c(2027, 2027, 2028),
#'   unit = c("t", "ratio", "t")
#' )
#' columns
#'
#' rows <- asdRows(
#'   label = c("MSY approach: FMSY", "F = 0", "F = Fpa"),
#'   basis = c("MSY", "Other", "Other"),
#'   purpose = c("BasisAdvice", "OtherScenarios", "OtherScenarios")
#' )
#' rows
#'
#' notes <- asdNotes(symbol = "*", note = "SSB 2028 relative to SSB 2027.")
#'
#' @rdname asdConstructors
#' @export
asdInfo <- function(stockCode, AssessmentYear, contact) {
  structure(
    list(
      stockCode = stockCode,
      AssessmentYear = AssessmentYear,
      Purpose = "Advice",
      contact = contact
    ),
    class = c("asd.info", "list")
  )
}

#' @rdname asdConstructors
#' @export
asdColumns <- function(label, year, unit, type = NA) {
  out <- data.frame(
    AK_Label = label,
    YearLabel = year,
    Unit = unit,
    CatchScenarioVariables = type,
    stringsAsFactors = FALSE
  )
  out <- cbind(AK_ID = 4L + seq_len(nrow(out)), out)
  out[c("AK_ID", "CatchScenarioVariables", "AK_Label", "YearLabel", "Unit")]
}

#' @rdname asdConstructors
#' @export
asdRows <- function(label, basis = "Other", purpose = "OtherScenarios") {
  out <- data.frame(
    CS_Purpose = purpose,
    CS_Basis = basis,
    CS_Label = label,
    stringsAsFactors = FALSE
  )
  blank <- is.na(out$CS_Basis) | !nzchar(trimws(out$CS_Basis))
  out$CS_Basis[blank] <- "Other"

  basis <- out$CS_Purpose %in% "BasisAdvice"
  AR_ID <- integer(nrow(out))
  AR_ID[basis] <- 11L + seq_len(sum(basis))
  AR_ID[!basis] <- 15L + seq_len(sum(!basis))

  cbind(AR_ID = AR_ID, out)
}

#' @rdname asdConstructors
#' @export
asdNotes <- function(symbol, note) {
  data.frame(Symbol = symbol, Notes = note, stringsAsFactors = FALSE)
}
