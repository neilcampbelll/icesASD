#' Record the assumptions of the short-term forecast
#'
#' Create the assumptions behind a catch scenarios table: the values used in
#' the intermediate year (Table 1 of the advice sheet), and the settings of the
#' forecast. Each row records a value and its basis, so the forecast can be
#' repeated from the record.
#'
#' Forecast assumptions are not yet part of the ASD upload (see
#' \href{https://github.com/ices-tools-prod/icesSAG/issues/308}{icesSAG #308}).
#' This is a proposal for how they could be recorded, and the codes below are
#' provisional until an ICES vocabulary is agreed.
#'
#' @section Variables:
#' Values used in the forecast, which need a \code{year}:
#' \describe{
#'   \item{CatchTotal, CatchWanted, CatchOther}{total catch, projected landings
#'     and projected discards}
#'   \item{FTotal, Fwanted, FOther}{fishing mortality, give the ages in \code{ages}}
#'   \item{StockSize}{SSB}
#'   \item{Recruitment}{recruitment, give the age in \code{ages}}
#' }
#' Settings of the forecast, usually with no \code{year}:
#' \describe{
#'   \item{ForecastMethod}{the software in \code{value}, and a basis of
#'     "Deterministic" or "Stochastic"}
#'   \item{Iterations}{the number of simulations}
#'   \item{ForecastStatistic}{the statistic reported, a basis of "Median" or "Mean"}
#'   \item{StockWeights, CatchWeights, LandingsWeights, DiscardsWeights,
#'     Maturity, NaturalMortality, Selectivity, DiscardFraction}{how each was
#'     set for the forecast years, with the years used in \code{from} and
#'     \code{to}}
#'   \item{FBeforeSpawning, MBeforeSpawning}{the proportion of F and M before
#'     spawning}
#' }
#'
#' @section Basis:
#' \describe{
#'   \item{Estimate}{an estimate from the assessment}
#'   \item{Forecast}{a result of the forecast}
#'   \item{StatusQuo, TAC, CatchConstraint}{how the intermediate year was set}
#'   \item{Mean, GeometricMean, Median, Resampled, MeanRescaled}{taken from the
#'     years \code{from} to \code{to}; "MeanRescaled" is a mean
#'     selection pattern rescaled to the F of the last year}
#'   \item{Model}{modelled, e.g. stock weights from a model}
#'   \item{Fixed}{a fixed value}
#'   \item{Deterministic, Stochastic}{the type of forecast}
#' }
#'
#' @param variable what the row records, see Variables.
#' @param year the year the value applies to.
#' @param value the value used. Can be text, e.g. the name of the software.
#' @param unit unit of the value, as an ICES MUNIT code (e.g. "t", "NE3", "ratio").
#' @param basis how the value was set, see Basis.
#' @param from,to the years used, e.g. to average or resample.
#' @param ages the ages, e.g. "6-14" for F, or "5" for recruitment.
#' @param note text explaining the value, as in Table 1 of the advice sheet.
#'
#' @return a data frame, with the names used in the XML file (\code{Variable},
#'         \code{Ages}, \code{Year}, \code{Value}, \code{Unit}, \code{Basis},
#'         \code{YearFrom}, \code{YearTo}, \code{Notes}).
#'
#' @seealso \code{\link{writeASDxml}}, \code{\link{checkASD}}
#'
#' @examples
#' # from Table 1 of the 2026 advice for aru.27.5b6a
#' asdAssumptions(
#'   variable = c("FTotal", "StockSize", "Recruitment", "CatchTotal"),
#'   ages = c("6-14", NA, "5", NA),
#'   year = c(2026, 2027, 2026, 2026),
#'   value = c(0.178, 96380, 107501, 17601),
#'   unit = c("ratio", "t", "NE3", "t"),
#'   basis = c("CatchConstraint", "Forecast", "Resampled", "CatchConstraint"),
#'   from = c(NA, NA, 2023, NA),
#'   to = c(NA, NA, 2025, NA)
#' )
#'
#' @export
asdAssumptions <- function(variable, year = NA, value = NA, unit = NA, basis = NA,
                           from = NA, to = NA, ages = NA, note = NA) {
  data.frame(
    Variable = variable,
    Ages = ages,
    Year = year,
    Value = value,
    Unit = unit,
    Basis = basis,
    YearFrom = from,
    YearTo = to,
    Notes = note,
    stringsAsFactors = FALSE
  )
}

# provisional codes, until an ICES vocabulary is agreed
asd_assumption_values <- c(
  "CatchTotal", "CatchWanted", "CatchOther",
  "FTotal", "Fwanted", "FOther",
  "StockSize", "Recruitment"
)

asd_assumption_settings <- c(
  "ForecastMethod", "Iterations", "ForecastStatistic",
  "StockWeights", "CatchWeights", "LandingsWeights", "DiscardsWeights",
  "Maturity", "NaturalMortality", "Selectivity", "DiscardFraction",
  "FBeforeSpawning", "MBeforeSpawning"
)

asd_assumption_averages <- c("Mean", "GeometricMean", "Median", "Resampled", "MeanRescaled")

asd_assumption_basis <- c(
  "Estimate", "Forecast", "StatusQuo", "TAC", "CatchConstraint",
  asd_assumption_averages, "Model", "Fixed", "Deterministic", "Stochastic"
)
