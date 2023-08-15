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

#' Calculate the number of staff persons necessary to complete a task
#'
#' Calculates two values (split by 'boom_months') and returns both in a vector,
#' so that there's never a shortage of staff.
#'
#' Set \code{boom_months=6} and \code{boom_pct=.5} to get all hours spread evenly across the year.
#'
#' @param task The total number of hours to get done in one year.
#' @param workdays Numeric, average total working days for a staff person. 205 is the 
#'    conservative lower end for Germany, see \url{https://www.deutschlandinzahlen.de/tab/deutschland/arbeitsmarkt/arbeitszeit/arbeitstage}.
#' @param hours Number of hours per working day.
#' @param rnd Round numbers up to this next fraction of a part-time job.
#' @param boom_months Number of months with highest workload, e.g., festival summer
#' @param boom_pct Total fraction of \code{task} that needs to be done during \code{boom_months}.
#' @return A named vector with two elements, \code{high} (number of staff needed for months with higher workload)
#' and \code{low} (number of staff needed for months with lower workload).
#' @rdname calc_staff
#' @export
#' @examples
#' calc_staff(12328)
calc_staff <- function(
    task,          # the total number of hours to get done in one year
    workdays=205,  # conservative lower end, see https://www.deutschlandinzahlen.de/tab/deutschland/arbeitsmarkt/arbeitszeit/arbeitstage
    hours=8,       # staff person working 8 hours a day
    rnd=.25,       # round numbers up to the next 25% of a part-time job
    boom_months=6, # number of months with highest workload, e.g., festival summer
    boom_pct=.5    # fraction of 'task' that happens in 'boom_months'
){
    boom_y <- boom_months / 12
    workdays <- c(high=workdays * boom_y, low=workdays * (1 - boom_y))
    task <- c(high=task * boom_pct, low=task * (1 - boom_pct))

    tasks_daily <- task / workdays
    staff_n <- tasks_daily / hours

    rnd_fct <- 1 / rnd
    staff_n_rnd <- ceiling(staff_n * rnd_fct) / rnd_fct

    return(staff_n_rnd)
}
