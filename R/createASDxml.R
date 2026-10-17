
#' Create the XML format for catch scenario data
#'
#' Deprecated: use \code{\link{writeASDxml}} instead.
#'
#' @param info details
#' @param scenarioColumns details
#' @param scenarioRows details
#' @param scenarioValues details
#' @param scenarioNotes details
#'
#' @examples
#' \dontrun{
#' checkTable(col, rows, vals)
#' }
#'
#' @importFrom reactable reactable colDef
#' @export
createASDxml <-
  function(info, scenarioColumns, scenarioRows, scenarioValues, scenarioNotes) {
    .Deprecated("writeASDxml")
    if (is.na(info$Purpose)) {
      info$Purpose <- "Advice"
    }

    list2xml <- function(x, xnames = NULL, sep = "") {
      if (!is.null(xnames)) {
        names(x) <- rep(xnames, length = length(x))
      }
      out <- paste(paste0("<", names(x), ">"),
        sapply(x, formatC, format = "fg"), paste0(
          "</",
          names(x), ">"
        ),
        collapse = "\n",
        sep = sep
      )
      gsub(">NA</", "></", out)
    }

    header <- paste("<?xml version='1.0' encoding='utf-8' standalone='no'?>",
      "<?xml-stylesheet type='text/xsl' href='CatchOptionsTable.xsl'?>",
      "<Assessment xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xsi:noNamespaceSchemaLocation='CatchOptionsTable.xsd'>",
      sep = "\n"
    )

    info <- unclass(info)
    # info <- info[names(info) %in% validNames("stockInfo")]
    info <- info[!sapply(info, is.na)]

    scenarioColumns <- as.data.frame(scenarioColumns)
    # fishdata <- fishdata[names(fishdata) %in% validNames("stockFishdata")]
    # scenarioColumnsfishdata <- fishdata[sapply(fishdata, function(x) !all(is.na(x)))]

    paste0(
      header, "\n", list2xml(info), "\n",
      list2xml(apply(scenarioColumns, 1, list2xml),
        xnames = "Scenarios_Columns",
        sep = "\n"
      ),"\n",
      list2xml(apply(scenarioRows, 1, list2xml),
        xnames = "Scenarios_Rows",
        sep = "\n"
      ), "\n",
      list2xml(apply(scenarioValues, 1, list2xml),
        xnames = "Scenarios_Values",
        sep = "\n"
      ), "\n",
      list2xml(apply(scenarioNotes, 1, list2xml),
        xnames = "Catch_Scenario_Notes_Notes",
        sep = "\n"
      ), "\n",
      "</Assessment>\n"
    )
  }
