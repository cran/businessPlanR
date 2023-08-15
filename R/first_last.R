# Copyright 2021-2022 Meik Michalke <meik.michalke@c3s.cc>
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

#' Shortcut for lists with steady transactions.
#' 
#' Generates a list of two elements, first and last month of the full years range, both
#' with the same value specified.
#'
#' You can use this in combination with the \code{.list} argument of
#' \code{\link[businessPlanR:expense]{expense}}, \code{\link[businessPlanR:revenue]{revenue}}, and
#' \code{\link[businessPlanR:transaction]{transaction}}.
#'
#' @param years Integer vector, at least two elements, the range of years to cover.
#' @param value The transaction amount that is assumed to remain unchanged over all \code{years}.
#' @return A list with two elements named after the first and last month of the \code{years}' range
#'    in \code{YYYY.MM} format.
#' @export
#' @examples
#' expense(
#'     type="Operation",
#'     category="Bank",
#'     name="Accounting",
#'     missing="rep",
#'     .list=first_last(2022:2025, value=20)
#' )
first_last <- function(years, value){
    results <- list(value, value)
    names(results) <- c(
        paste0(years[1], ".01"),
        paste0(years[length(years)], ".12")
    )
    return(results)
}



