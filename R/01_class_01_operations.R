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

#' S4 Class operations
#'
#' This class is used for objects that contain all transactions of the business plan.
#'
#' @section Constructor function:
#' Should you need to manually generate objects of this class, the constructor function 
#' \code{operations(...)} can be used instead of
#' \code{new("operations", ...)}.
#'
#' @slot period A character vector defining beginning and end of the time period covered by the business plan.
#'    Values can either be a vector of two in \code{YYYY.MM} format, or a numeric vector of full fiscal years which will automatically be
#'    transformed into character.
#' @slot revenue Data frame containing \code{type}, \code{category}, \code{name}, and all revenues, each month in a column named \code{YYYY.MM}. If these are not covering \code{period} exactly, missing values will be set to zero.
#' @slot expense Data frame containing all expenses, data structure like the \code{revenue} slot.
#' @slot loan Data frame, basically the \code{plan} slot as in \code{\link[businessPlanR:transaction_plan-class]{transaction_plan}} with
#'    \code{plan_type="loan"}.
#' @slot depreciation Data frame, like \code{loan}, but with \code{plan_type="depreciation"}, respectively.
#' @slot misc A list to keep miscellaneous data or information for documentation or re-use.
#' @name operations,-class
#' @aliases operations-class
#' @import methods
#' @keywords classes
#' @export operations
#' @exportClass operations
#' @rdname operations-class
#' @examples
#' rev_2019_2021_merch <- revenue(
#'    type="Sale",
#'    category="Merch",
#'    name="T-Shirts",
#'    "2019.01"=100,
#'    "2019.08"=267,
#'    "2020.03"=344,
#'    "2020.09"=549,
#'    "2021.02"=770,
#'    "2021.07"=1022,
#'    "2021.12"=1263
#' )
#' rev_2019_2021_rec <- revenue(
#'    type="Sale",
#'    category="Records",
#'    name="Albums",
#'    "2019.01"=220,
#'    "2019.08"=234,
#'    "2020.03"=221,
#'    "2020.09"=354,
#'    "2021.02"=276,
#'    "2021.07"=285,
#'    "2021.12"=311
#' )
#' rev_2019_2021_inv <- revenue(
#'    type="Invest income",
#'    category="Rent",
#'    name="Studio",
#'    "2019.01"=120,
#'    "2019.08"=234,
#'    "2020.03"=321,
#'    "2020.09"=454,
#'    "2021.02"=376,
#'    "2021.07"=385,
#'    "2021.12"=211
#' )
#' exp_2019_2021_merch <- expense(
#'    type="Goods",
#'    category="Merch",
#'    name="T-Shirts",
#'    "2019.01"=65,
#'    "2019.07"=170,
#'    "2020.02"=210,
#'    "2020.08"=312,
#'    "2021.01"=450,
#'    "2021.06"=600,
#'    "2021.12"=720
#' )
#' exp_2019_2021_rec <- expense(
#'    type="Goods",
#'    category="Records",
#'    name="Pressing",
#'    "2019.01"=1860,
#'    "2019.02"=0,
#'    "2020.08"=600,
#'    "2020.09"=0,
#'    "2021.12"=0
#' )
#'
#' op_2019_2021 <- operations(
#'     period=c("2019.01", "2021.12") # alternative: 2019:2021
#' ) 
#' update_operations(op_2019_2021) <- rev_2019_2021_merch
#' update_operations(op_2019_2021) <- exp_2019_2021_merch
#' update_operations(op_2019_2021) <- rev_2019_2021_rec
#' update_operations(op_2019_2021) <- exp_2019_2021_rec
#' update_operations(op_2019_2021) <- rev_2019_2021_inv
operations <- setClass("operations",
    representation=representation(
        period="character", # e.g., c("2020.06", "2022.12") or c("2020", "2021", "2022")
        revenue="data.frame",
        expense="data.frame",
        loan="data.frame",
        depreciation="data.frame",
        misc="list"
    )
)

setMethod("initialize", "operations",
    function(
        .Object,
        period,
        revenue,
        expense,
        loan,
        depreciation,
        misc
    ){
        if(!missing(period)){
            if(is.numeric(period)){
                period <- as.character(period)
            } else {}
            period <- c(check_YYYY_MM(period[1], month="01"), check_YYYY_MM(period[length(period)], month="12"))
            slot(.Object, "period") <- period
            empty_df <- empty_df(period)
        } else {
            empty_df <- empty_df()
        }
        if(!missing(revenue)){
            slot(.Object, "revenue") <- revenue
        } else {
            slot(.Object, "revenue") <- empty_df
        }
        if(!missing(expense)){
            slot(.Object, "expense") <- expense
        } else {
            slot(.Object, "expense") <- empty_df
        }
        if(!missing(loan)){
            slot(.Object, "loan") <- loan
        } else {
            slot(.Object, "loan") <- empty_df(first_cols=c("type", "category", "name", "part"))
        }
        if(!missing(depreciation)){
            slot(.Object, "depreciation") <- depreciation
        } else {
            slot(.Object, "depreciation") <- empty_df(first_cols=c("type", "category", "name", "part"))
        }
        if(!missing(misc)){
            slot(.Object, "misc") <- misc
        } else {
            slot(.Object, "misc") <- list()
        }
        validObject(.Object)
        return(.Object)
    }
)


setValidity("operations", function(object){
    period <- slot(object, "period")
    revenue <- slot(object, "revenue")
    expense <- slot(object, "expense")
    loan <- slot(object, "loan")
    depreciation <- slot(object, "depreciation")

    if(!all(c(colnames(revenue), colnames(expense)) %in% c(default_cols, YYYY_MM(period)))){
        stop(simpleError("The period doesn't add up, please fix!"))
    } else {}

    if(!all(c("type", "category", "name", "part") %in% colnames(loan))){
        stop(simpleError("Invalid 'loan' slot!"))
    } else {}

    if(!all(c("type", "category", "name", "part") %in% colnames(depreciation))){
        stop(simpleError("Invalid 'depreciation' slot!"))
    } else {}

    return(TRUE)
})
