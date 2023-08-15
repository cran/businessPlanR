# Copyright 2021-2023 Meik Michalke <meik.michalke@c3s.cc>
#
# This file is part of the R package businessPlanR.
#
# businessPlanR is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# businessPlanR is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with businessPlanR.  If not, see <http://www.gnu.org/licenses/>.

#' Turn a Shiny permalink into a list
#'
#' The Shiny package can generate permalinks of its web apps, making it possible to share individual configurations
#' of the app with others. This function translated such a permalink into a named list, so you can use the configuration
#' also in R code.
#' 
#' When this package was written, we also wrote a Shiny web app for it but separated the actual calculations from the app's code.
#' This allowed us to use the same functions and objects in RMarkdown. We were discussing the numbers in the web tool
#' using permalinks, and finally transferred the calculations to the PDF version.
#' 
#' To transfer the configuration from the web app to the markdown document, this function discards the URL prefix and splits
#' the arguments into a named list that behaves like the input object commonly used in Shiny apps.
#'
#' @param permalink Character string, the actual URL with arguments copied from the Shiny app as-is.
#' @param prefix Character string or regular expression, should capture everything up to the first argument name.
#'    This is the part that will be discarded.
#' @return A named list with one element for each argument in \code{permalink}.
#' @rdname permalink2list
#' @export
#' @examples
#' permalink2list(
#'   paste0(
#'     "https://example.com/businessPlanR/?_inputs_&salary=50000",
#'     "&loan_interest=3.22&loan_period=7&loan_due=2&years=%5B%222022%22%2C%222026%22%5D"
#'   )
#' )
permalink2list <- function(
    permalink,
    prefix=".*\\?_inputs_&"
){
    result <- strsplit(gsub(prefix, "", permalink), "&")
    result <- lapply(result, function(x) {
        gsub("%22", "\"", 
            gsub("%5B", "",
                gsub("%5D", "",
                    gsub("%2C", ":", x)
                )
            )
        )
    })
    result <- eval(parse(text=paste0(
        "list(",
        paste0(unlist(result), collapse=","),
        ")"
    )))
    return(result)
}
