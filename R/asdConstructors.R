#' Create the parts of a catch scenarios upload file
#'
#' Helper functions to create the stock information, column and row headers,
#' and notes of a catch scenarios table, for writing with \code{\link{writeASDxml}}.
#'
#' Column and row IDs follow the numbering of the ICES catch scenarios Excel
#' template: columns (\code{AK_ID}) are numbered from 5, basis of advice rows
#' (\code{AR_ID}) are numbered 12 to 14, and other scenario rows from 16.
#'
#' @param stockCode the ICES stock code, e.g. "aru.27.5b6a".
#' @param AssessmentYear the assessment year.
#' @param contact the name of the contact person.
#' @param AK_Label column labels, including any footnote symbols.
#' @param YearLabel the year each column refers to.  For \% change columns
#'                  give the last year.
#' @param Unit units of each column, as ICES MUNIT codes (e.g. "t", "ratio", "\%").
#' @param CatchScenarioVariables optional type of each column, as codes from the
#'                               ICES StockDataType vocabulary (e.g. "Catch",
#'                               "FishingPressure", "StockSize").  Left out of the
#'                               file when NA.
#' @param CS_Label scenario labels, including any footnote symbols.
#' @param CS_Purpose "BasisAdvice" for the basis of the advice (up to three rows),
#'                   or "OtherScenarios".
#' @param CS_Basis the basis of each scenario, as codes from the ICES
#'                 CatchScenariosBasis vocabulary (e.g. "MSY", "MP", "PA", "Other").
#'                 Empty entries are set to "Other".
#' @param Symbol footnote symbols, e.g. "*", "**", "^".
#' @param Notes the footnote text for each symbol.
#'
#' @return \code{asdInfo} returns a list, the others return data frames.
#'
#' @seealso \code{\link{writeASDxml}}, \code{\link{checkASD}}
#'
#' @examples
#' info <- asdInfo("aru.27.5b6a", 2026, "Assessor Name")
#'
#' columns <- asdColumns(
#'   AK_Label = c("Total catch", "F 6–14", "Spawning-stock biomass"),
#'   YearLabel = c(2027, 2027, 2028),
#'   Unit = c("t", "ratio", "t")
#' )
#' columns
#'
#' rows <- asdRows(
#'   CS_Label = c("MSY approach: FMSY", "F = 0", "F = Fpa"),
#'   CS_Purpose = c("BasisAdvice", "OtherScenarios", "OtherScenarios"),
#'   CS_Basis = c("MSY", "Other", "Other")
#' )
#' rows
#'
#' notes <- asdNotes(Symbol = "*", Notes = "SSB 2028 relative to SSB 2027.")
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
asdColumns <- function(AK_Label, YearLabel, Unit, CatchScenarioVariables = NA) {
  out <- data.frame(
    AK_Label = AK_Label,
    YearLabel = YearLabel,
    Unit = Unit,
    CatchScenarioVariables = CatchScenarioVariables,
    stringsAsFactors = FALSE
  )
  out <- cbind(AK_ID = 4L + seq_len(nrow(out)), out)
  out[c("AK_ID", "CatchScenarioVariables", "AK_Label", "YearLabel", "Unit")]
}

#' @rdname asdConstructors
#' @export
asdRows <- function(CS_Label, CS_Purpose = "OtherScenarios", CS_Basis = "Other") {
  out <- data.frame(
    CS_Purpose = CS_Purpose,
    CS_Basis = CS_Basis,
    CS_Label = CS_Label,
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
asdNotes <- function(Symbol, Notes) {
  data.frame(Symbol = Symbol, Notes = Notes, stringsAsFactors = FALSE)
}
