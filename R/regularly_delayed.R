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

#' Generate list of repeating financial transactions with delayed starting month
#'
#' In case you only know the annual sum of transactions for given years but also that they
#' don't begin in January of the first year, you can use the function \code{regularly_delayed} to split the sums
#' to be used in revenue or expense objects that acknowledge the delay.
#' It extends \code{\link[businessPlanR:regularly]{regularly}}.
#' 
#' The \code{delayed} function assumes \code{pa} to be a total value for a full year, but does not distribute it evenly
#' over the active months, but rather subtracts any amount that would have been due before \code{start_month}.
#'
#' @param years See \code{\link[businessPlanR:regularly]{regularly}}.
#' @param pa See \code{\link[businessPlanR:regularly]{regularly}}.
#' @param start_month Integer number, the month of the first revenue/expense. All earlier monthly transactions will be 0
#'    and the sum for the respective year divided by the number months left for that year.
#' @return Either a list of monthly transactions named in "YYYY.MM" scheme (\code{regularly_delayed}), or
#'    vector of the same length as \code{pa} (\code{delayed}).
#' @rdname regularly_delayed
#' @export
#' @examples
#' # say you earn 3000 each year, but payment starts in September
#' # calculate payment sums
#' delayed_2019_2021 <- delayed(
#'    pa=rep(3000, 3),
#'    start_month=9
#' )
#' 
#' # now use the result to caclulate monthly amounts
#' delayed_monthly_2019_2021 <- regularly_delayed(
#'    years=2019:2021,
#'    pa=delayed_2019_2021,
#'    start_month=9
#' )

regularly_delayed <- function(
    years,
    pa,
    start_month=1
){
    if(start_month > 1){
        missing_payment <- pa
        # this is going to be the year that needs an adjustment...
        year <- (start_month %/% 12) + 1
        # ... in this particular month
        rest <- start_month %% 12
        if(isTRUE(rest == 0)){
            rest <- 12
        } else {}
        adjusted_rates <- missing_payment[year] / length(rest:12)
        missing_payment[1:year] <- 0
        result <- regularly(
            years=years,
            pa=missing_payment/12,
            month="01",
            last="rep",
            merge=regularly(
                years=years[year],
                pa=adjusted_rates,
                month=sprintf("%02i", rest),
                last="rep",
                first="none"
            )
        )
    } else {
        result <- regularly(
            years=years,
            pa=pa/12,
            month="01",
            last="rep"
        )
    }
    return(result)
}

#' @rdname regularly_delayed
#' @export
delayed <- function(
    pa,
    start_month=1
){
    results <- pa
    years <- start_month %/% 12
    rest <- start_month %% 12
    if(rest > 0){
        rest <- (13 - rest) / 12
    } else {
        rest <- 1/12
    }
    if(years > 0){
        results[1:years] <- 0
    } else {}
    if(start_month %% 12 > 0){
        results[years+1] <- pa[years+1] * rest
    } else {
        results[years] <- pa[years] * rest
    }
    return(results)
}

