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

#' Estimate capital requirement from cash flow
#'
#' To avoid cash flow issues, this function takes a data frame as returned by
#' \code{\link[businessPlanR:condense]{condense}} with \code{cashflow=TRUE} to calculate
#' the amount of financial needs per time resolution.
#'
#' Only negative values are returned, so the row sum can be used as an estimate
#' of the overall financial demand for the given period of time.
#'
#' @param cashflow_df Data frame as returned by \code{\link[businessPlanR:condense-methods]{condense}} with \code{cashflow=TRUE}.
#' @param resolution  One of \code{"month"}, \code{"quarter"}, or \code{"year"}. Must be identical
#'    to the value used with the call to \code{condense}!
#' @param row_names Character vector of two, names for the rows of the resulting data frame.
#'    The first represents financial need per time period (column), the second is cumulated over all columns.
#' @return A data frame with two rows and columns depending on \code{resolution} and period covered by \code{cashflow_df}.
#' @rdname fin_needs
#' @export
fin_needs <- function(
    cashflow_df,
    resolution=c("year", "quarter", "month"),
    row_names=c(
        "Financial needs",
        "Cumulative"
    )
){
    resolution <- match.arg(resolution)
    year_cols <- df_date_cols(cashflow_df, scheme=resolution)
    cash_demand <- cashflow_df[c(1, nrow(cashflow_df)), year_cols]
    cash_demand[1,] <- cash_demand[1,] * -1
    missing_funds <- colSums(cash_demand)
    missing_funds[missing_funds > 0] <- 0 
    cash_demand[3,] <- missing_funds
    cash_demand[4,] <- cumsum(missing_funds)
    cash_demand <- cash_demand[3:4,]
    row.names(cash_demand) <- row_names
    return(cash_demand)
}
