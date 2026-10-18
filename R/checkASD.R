#' Check a catch scenarios table before writing the upload file
#'
#' Checks codes against the ICES vocabularies (using \pkg{icesVocab}), the
#' layout of the table against the limits of the ICES catch scenarios template,
#' and that footnote symbols used in the labels match the notes.  Forecast
#' assumptions, if given, are checked against the provisional codes listed in
#' \code{\link{asdAssumptions}}; codes not in those lists are warnings, not errors.
#'
#' Vocabulary checks need an internet connection.  If a vocabulary cannot be
#' downloaded, a warning is given and that check is skipped.
#'
#' @inheritParams writeASDxml
#'
#' @return a list with two character vectors: \code{errors} and \code{warnings}.
#'
#' @seealso \code{\link{writeASDxml}} runs these checks before writing the file.
#'
#' @export
checkASD <- function(info, columns, rows, values, notes = NULL, assumptions = NULL) {
  x <- asd_prepare(info, columns, rows, values, notes, assumptions)

  errors <- character(0)
  warnings <- character(0)
  addError <- function(...) errors <<- c(errors, paste0(...))
  addWarning <- function(...) warnings <<- c(warnings, paste0(...))
  isBlank <- function(v) is.na(v) | !nzchar(trimws(as.character(v)))

  # ---- vocabularies ----
  checkVocab <- function(value, code_type, field) {
    value <- unique(trimws(as.character(value[!isBlank(value)])))
    if (!length(value)) return(invisible())
    keys <- asd_vocab_keys(code_type)
    if (is.null(keys)) {
      addWarning("could not download the ", code_type, " vocabulary, ", field, " was not checked")
      return(invisible())
    }
    for (v in setdiff(value, keys)) {
      addError(field, " '", v, "' is not a valid code in the ICES ", code_type, " vocabulary")
    }
  }
  checkVocab(x$info$stockCode, "ICES_StockCode", "stockCode")
  checkVocab(x$columns$Unit, "MUNIT", "unit")
  checkVocab(x$columns$CatchScenarioVariables, "StockDataType", "column type")
  checkVocab(x$rows$CS_Basis, "CatchScenariosBasis", "basis")
  if (!is.null(x$assumptions)) checkVocab(x$assumptions$Unit, "MUNIT", "assumption unit")

  # ---- stock information ----
  if (isBlank(x$info$stockCode)) addError("stockCode is missing")
  if (isBlank(x$info$contact)) addError("contact is missing")
  if (isBlank(x$info$AssessmentYear)) {
    addError("AssessmentYear is missing")
  } else if (as.numeric(x$info$AssessmentYear) > as.numeric(format(Sys.Date(), "%Y"))) {
    addError("AssessmentYear (", x$info$AssessmentYear, ") is later than the present year")
  }

  # ---- columns ----
  if (nrow(x$columns) == 0) addError("there are no columns")
  if (nrow(x$columns) > 50) addError("there are ", nrow(x$columns), " columns, the maximum is 50")
  for (i in which(isBlank(x$columns$AK_Label))) addError("column ", i, " has no label")
  for (i in which(isBlank(x$columns$YearLabel))) addWarning("column ", i, " (", x$columns$AK_Label[i], ") has no year")
  for (i in which(isBlank(x$columns$Unit))) addWarning("column ", i, " (", x$columns$AK_Label[i], ") has no unit")

  # ---- rows ----
  nBasis <- sum(x$rows$CS_Purpose %in% "BasisAdvice")
  if (nBasis == 0) addError("there is no basis of advice row (purpose = 'BasisAdvice')")
  if (nBasis > 3) addError("there are ", nBasis, " basis of advice rows, the maximum is 3")
  for (p in setdiff(unique(x$rows$CS_Purpose), c("BasisAdvice", "OtherScenarios"))) {
    addError("purpose '", p, "' must be 'BasisAdvice' or 'OtherScenarios'")
  }
  for (i in which(isBlank(x$rows$CS_Label))) addError("row ", i, " has no label")

  # ---- values ----
  if (!identical(dim(x$values), c(nrow(x$rows), nrow(x$columns)))) {
    addError(
      "values has ", nrow(x$values), " rows and ", ncol(x$values), " columns, but there are ",
      nrow(x$rows), " row headers and ", nrow(x$columns), " column headers"
    )
  } else if (all(is.na(x$values))) {
    addWarning("the table has no values")
  }

  # ---- footnote symbols ----
  used <- unique(unlist(regmatches(
    c(x$columns$AK_Label, x$rows$CS_Label),
    gregexpr("[*^#+]+", c(x$columns$AK_Label, x$rows$CS_Label))
  )))
  defined <- if (is.null(x$notes)) character(0) else trimws(as.character(x$notes$Symbol))
  for (s in setdiff(used, defined)) addWarning("footnote symbol '", s, "' is used in a label but has no note")
  for (s in setdiff(defined, used)) addWarning("note '", s, "' is not used in any label")

  # ---- forecast assumptions ----
  a <- x$assumptions
  if (!is.null(a)) {
    for (i in seq_len(nrow(a))) {
      v <- a$Variable[i]
      what <- paste0("assumption ", i, if (!isBlank(v)) paste0(" (", v, ")"))
      if (isBlank(v)) {
        addError(what, " has no variable")
      } else if (!v %in% c(asd_assumption_values, asd_assumption_settings)) {
        addWarning(what, ": variable '", v, "' is not one of the provisional codes")
      }
      if (!isBlank(a$Basis[i]) && !a$Basis[i] %in% asd_assumption_basis) {
        addWarning(what, ": basis '", a$Basis[i], "' is not one of the provisional codes")
      }
      if (v %in% asd_assumption_values) {
        if (isBlank(a$Year[i])) addError(what, " has no year")
        if (isBlank(a$Value[i]) && isBlank(a$Notes[i])) addWarning(what, " has no value and no note")
      }
      from <- a$YearFrom[i]
      to <- a$YearTo[i]
      if (isBlank(from) != isBlank(to)) {
        addError(what, " needs both from and to")
      } else if (!isBlank(from) && as.numeric(from) > as.numeric(to)) {
        addError(what, ": from (", from, ") is after to (", to, ")")
      }
      if (a$Basis[i] %in% asd_assumption_averages && !v %in% "ForecastStatistic" && isBlank(from)) {
        addWarning(what, ": basis '", a$Basis[i], "' needs the years used (from and to)")
      }
    }
    key <- paste(a$Variable, a$Ages, a$Year)
    for (k in unique(key[duplicated(key)])) addError("assumption '", k, "' is given more than once")
  }

  list(errors = errors, warnings = warnings)
}

# valid codes for an ICES vocabulary code type, NULL if it cannot be downloaded
asd_vocab_keys <- function(code_type) {
  out <- tryCatch(
    suppressMessages(icesVocab::getCodeList(code_type)),
    error = function(e) NULL
  )
  if (is.null(out) || !is.data.frame(out) || !"Key" %in% names(out)) {
    return(NULL)
  }
  trimws(as.character(out$Key))
}
