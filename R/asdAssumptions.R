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
#' Values used in the forecast, which need a \code{Year}:
#' \describe{
#'   \item{CatchTotal, CatchWanted, CatchOther}{total catch, projected landings
#'     and projected discards}
#'   \item{FTotal, Fwanted, FOther}{fishing mortality, give the ages in \code{Ages}}
#'   \item{StockSize}{SSB}
#'   \item{Recruitment}{recruitment, give the age in \code{Ages}}
#' }
#' Settings of the forecast, usually with no \code{Year}:
#' \describe{
#'   \item{ForecastMethod}{the software in \code{Value}, and a basis of
#'     "Deterministic" or "Stochastic"}
#'   \item{Iterations}{the number of simulations}
#'   \item{ForecastStatistic}{the statistic reported, a basis of "Median" or "Mean"}
#'   \item{StockWeights, CatchWeights, LandingsWeights, DiscardsWeights,
#'     Maturity, NaturalMortality, Selectivity, DiscardFraction}{how each was
#'     set for the forecast years, with the years used in \code{YearFrom} and
#'     \code{YearTo}}
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
#'     years \code{YearFrom} to \code{YearTo}; "MeanRescaled" is a mean
#'     selection pattern rescaled to the F of the last year}
#'   \item{Model}{modelled, e.g. stock weights from a model}
#'   \item{Fixed}{a fixed value}
#'   \item{Deterministic, Stochastic}{the type of forecast}
#' }
#'
#' @param Variable what the row records, see Variables.
#' @param Year the year the value applies to.
#' @param Value the value used. Can be text, e.g. the name of the software.
#' @param Unit unit of the value, as an ICES MUNIT code (e.g. "t", "NE3", "ratio").
#' @param Basis how the value was set, see Basis.
#' @param YearFrom,YearTo the years used, e.g. to average or resample.
#' @param Ages the ages, e.g. "6-14" for F, or "5" for recruitment.
#' @param Notes text explaining the value, as in Table 1 of the advice sheet.
#'
#' @return a data frame.
#'
#' @seealso \code{\link{writeASDxml}}, \code{\link{checkASD}}
#'
#' @examples
#' # from Table 1 of the 2026 advice for aru.27.5b6a
#' asdAssumptions(
#'   Variable = c("FTotal", "StockSize", "Recruitment", "CatchTotal"),
#'   Ages = c("6-14", NA, "5", NA),
#'   Year = c(2026, 2027, 2026, 2026),
#'   Value = c(0.178, 96380, 107501, 17601),
#'   Unit = c("ratio", "t", "NE3", "t"),
#'   Basis = c("CatchConstraint", "Forecast", "Resampled", "CatchConstraint"),
#'   YearFrom = c(NA, NA, 2023, NA),
#'   YearTo = c(NA, NA, 2025, NA)
#' )
#'
#' @export
asdAssumptions <- function(Variable, Year = NA, Value = NA, Unit = NA, Basis = NA,
                           YearFrom = NA, YearTo = NA, Ages = NA, Notes = NA) {
  data.frame(
    Variable = Variable,
    Ages = Ages,
    Year = Year,
    Value = Value,
    Unit = Unit,
    Basis = Basis,
    YearFrom = YearFrom,
    YearTo = YearTo,
    Notes = Notes,
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
