# Copyright 2021 Meik Michalke <meik.michalke@c3s.cc>
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

#' Generate list of repeating financial transactions
#'
#' For all years defined, generates a list of values as defined by \code{pa}
#' and due at the given month. The result can be used as input for the
#' \code{.list} argument of \code{\link[businessPlanR:expense]{expense}},
#' \code{\link[businessPlanR:revenue]{revenue}}, and \code{\link[businessPlanR:transaction]{transaction}}.
#'
#' @param years Integer vector, the range of years to cover.
#' @param pa A vector with values for each year. This amounts to the total sum for the respective year.
#' @param month Character, but numeric description of a month in "MM" format when to account the values of \code{pa}.
#'    If you provide more than a single month here, e.g., quarterly payments, the amounts defined by \code{pa} are
#'    divided the number of months.
#' @param last Defines the final entry, last month of the last year. It can be either
#'    a numeric value (taken as-is), \code{"rep"} (repeats the last value of pa),
#'    or \code{"none"} to omit adding a last month (e.g., to later merge with results of another call to this function).
#'    Only used if month is not "12".
#' @param first Defines how to treat years if January was included in in \code{month}. This could be desired for merging,
#'    but problematic if you want to create a new transaction object. Valid values are the same as for \code{last} except
#'    \code{"rep"}.
#' @param merge Another list of values to be merged with the results, can be used for nested calls of this function to
#'    generate more complex patterns.
#' @param digits Number of digits used for rounding when \code{month} is more than one entry.
#' @return A list of monthly transactions named in "YYYY.MM" scheme (\code{regularly_delayed}).
#' @rdname regularly
#' @export
#' @examples
#' expense(
#'     type="Operation",
#'     category="Insurance",
#'     name="Electronics",
#'     missing="0",
#'     .list=regularly(
#'         years=2021:2025,
#'         pa=rep(111.11, 5),
#'         month="01",
#'         last=0
#'     )
#' )
regularly <- function(years, pa, month="01", last=0, first=0, merge=list(), digits=2){
    month <- coerce_month(month=month, call="regularly")
    if(!length(years) == length(pa)){
        stop(simpleError("unequal number of elements in 'years' and 'pa'!"))
    } else {}
    n_months <- length(month)
    if(n_months > 1){
        pa <- round(rep(pa/n_months, each=n_months), digits=digits)
        years <- rep(years, each=n_months)
    } else {}
    names(pa) <- paste0(years, ".", month)
    if(all(!"01" %in% month, !identical("none", first))){
        if(is.numeric(first)){
            pa[paste0(years, ".01")] <- first
            pa <- pa[sort(names(pa))]
        } else {
            stop(simpleError("unsupported value for 'first'!"))
        } 
    }
    if(all(!"12" %in% month, !identical("none", last))){
        if(is.numeric(last)){
            pa[paste0(years[length(years)], ".12")] <- last
        } else if(identical("rep", last)){
            pa[paste0(years[length(years)], ".12")] <- pa[length(pa)]
        } else {
            stop(simpleError("unsupported value for 'last'!"))
        }
    }
    if(length(merge) > 0){
        dupes_months <- names(merge) %in% names(pa)
        for (dupes in names(merge)[dupes_months]){
            pa[[dupes]] <- pa[[dupes]] + merge[[dupes]]
        }
        pa <- append(pa, merge[!dupes_months])
        pa <- pa[sort(names(pa))]
    }

    return(as.list(pa))
}
