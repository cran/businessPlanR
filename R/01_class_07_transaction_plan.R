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

#' S4 Class transaction_plan
#'
#' This is a container class for multiple objects of either class \code{\link[businessPlanR:depreciation-class]{depreciation}} or
#' \code{\link[businessPlanR:loan-class]{loan}}, similar to \code{\link[businessPlanR:operations-class]{operations}} for revenues and expenses.
#' Its main data frame stores each transaction object in multiple rows.
#' Investment have three rows, \code{investment}, \code{depreciation}, and remaining \code{value}, while loans have six named
#' \code{balance_start}, \code{interest}, \code{principal}, \code{total}, \code{cumsum}, and \code{balance_remain}, repectively.
#' This makes it easier to create nice overview tables via \code{\link[businessPlanR:kable_bpR]{kable_bpR}}.
#' 
#' The data frame has four meta data columns, \code{type}, \code{category}, \code{name}, and \code{part}, followed by a column for each month
#' covered by any of the contained \code{transaction} objects. The first three columns take their values from the respective object, while
#' the fourth, \code{part}, defines the rows as explained earlier.
#' 
#' @section Constructor function:
#' Should you need to manually generate objects of this class, the constructor function 
#' \code{transaction_plan(...)} can be used instead of
#' \code{new("transaction_plan", ...)}.
#'
#' @slot plan_type One of \code{"depreciation"} or \code{"loan"}, defining which type of transactions are accumulated in the object.
#' @slot plan A data frame with three rows for each \code{\link[businessPlanR:depreciation-class]{depreciation}} or
#'    six for each \code{\link[businessPlanR:loan-class]{loan}} class object added to it, e.g., via \code{\link[businessPlanR:update_plan]{update_plan}}.
#' @name transaction_plan,-class
#' @aliases transaction_plan-class
#' @import methods
#' @keywords classes
#' @export transaction_plan
#' @exportClass transaction_plan
#' @rdname transaction_plan-class
#' @examples
#' depreciation_printer <- depreciation(
#'     type="Depreciation",
#'     category="Office",
#'     name="Printer",
#'     amount=100,
#'     obsolete=36,
#'     invest_month="2019.04"
#' )
#' depreciation_laptop <- depreciation(
#'     type="Depreciation",
#'     category="Office",
#'     name="Laptop",
#'     amount=1200,
#'     obsolete=36,
#'     invest_month="2019.02"
#' )
#' # initialize an empty plan
#' dep_plan <- transaction_plan()
#' # add your assets to the plan
#' update_plan(dep_plan) <- depreciation_printer
#' update_plan(dep_plan) <- depreciation_laptop
transaction_plan <- setClass("transaction_plan",
    representation=representation(
        plan_type="character",
        plan="data.frame"
    )
)

setMethod("initialize", "transaction_plan",
    function(
        .Object,
        plan_type=c("depreciation", "loan"),
        plan
    ){
        slot(.Object, "plan_type") <- match.arg(plan_type)
        if(!missing(plan)){
            slot(.Object, "plan") <- plan
        } else {
            slot(.Object, "plan") <- empty_df(first_cols=c("type", "category", "name", "part"))
        }
        validObject(.Object)
        return(.Object)
    }
)

setValidity("transaction_plan", function(object){
    plan_type <- slot(object, "plan_type")
    plan <- slot(object, "plan")

    if(!plan_type %in% c("depreciation", "loan")){
        stop(simpleError("Invalid 'plan_type'!"))
    } else {}
    if(!all(c("type", "category", "name", "part") %in% names(plan))){
        stop(simpleError("Invalid data frame!"))
    } else {}
    if(nrow(plan) > 0){
        if(!all(plan[["part"]] %in% valid_parts(plan_type=plan_type))){
            stop(simpleError("Values of 'part' don't match 'part_type', did you mix up \"depreciation\" and \"loan\"?"))
        } else {}
    } else {}

    return(TRUE)
})
