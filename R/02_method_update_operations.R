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

#' Update operations objects
#' 
#' You can use this method to add or replace transactions to an existing object of class
#' \code{\link[businessPlanR:operations-class]{operations}}.
#' 
#' @docType methods
#' @param obj An object of class \code{\link[businessPlanR:operations-class]{operations}}.
#' @param cut_to_period Logical, whether to adjust the data of \code{value} to the period covered by \code{obj}. This means that missing months
#'    will be added with zero values, and months that lie beyond the covered period will be dropped.
#'    This only affects objects of class \code{revenue} and \code{expense}.
#' @param warning Logical, if \code{TRUE} shows a warning when \code{cut_to_period=TRUE} and months are adjusted.
#' @param as_transaction Optional list of vectors of arguments for \code{value} of class \code{\link[businessPlanR:loan-class]{loan}} or
#'    \code{\link[businessPlanR:depreciation-class]{depreciation}}, as used by \code{\link[businessPlanR:as_transaction]{as_transaction}}.
#'    If given, the object provided as \code{value} will also be processed as if \code{as_transaction} was also called. This is repeated
#'    for each vector of arguments.
#' @param value An object of either class \code{\link[businessPlanR:revenue-class]{revenue}}, \code{\link[businessPlanR:expense-class]{expense}},
#'    \code{\link[businessPlanR:loan-class]{loan}}, \code{\link[businessPlanR:depreciation-class]{depreciation}}, or
#'    \code{\link[businessPlanR:transaction_plan-class]{transaction_plan}}.
#' @return An updated object of class \code{operations}.
#' @rdname update_operations-methods
#' @export
setGeneric("update_operations<-", function(obj, cut_to_period=TRUE, warning=FALSE, as_transaction, value) standardGeneric("update_operations<-"))

#' @rdname update_operations-methods
#' @export
#' @docType methods
#' @aliases
#'    update_operations<-,-methods
#'    update_operations<-,operations-method
setMethod("update_operations<-",
    signature=signature(obj="operations"),
    function (obj, cut_to_period=TRUE, warning=FALSE, as_transaction, value){
        if(!any(is(value, "revenue"), is(value, "expense"), is(value, "loan"), is(value, "depreciation"), is(value, "transaction_plan"))){
            stop(simpleError("The value must be an object of class \"revenue\", \"expense\", \"loan\", \"depreciation\", or \"transaction_plan\"!"))
        } else {}
        if(is(value, "loan")){
            ## TODO: replace updated loan objects
            # update the loan plan
            loan_plan <- get_loans(obj, as_data_frame=FALSE)
            update_plan(loan_plan) <- value
            slot(obj, "loan") <- get_value(loan_plan, drop_nonyear_cols=FALSE, resolution="month")
            if(!missing(as_transaction)){
                obj <- do_as_transaction_call(obj=obj, value=value, args_list=as_transaction)
            } else {}
        } else if(is(value, "depreciation")){
            # update the depreciation plan
            dep_plan <- get_depreciation_plan(obj, as_data_frame=FALSE)
            update_plan(dep_plan) <- value
            slot(obj, "depreciation") <- get_value(dep_plan, drop_nonyear_cols=FALSE, resolution="month")
            if(!missing(as_transaction)){
                obj <- do_as_transaction_call(obj=obj, value=value, args_list=as_transaction)
            } else {}
        } else if(is(value, "transaction_plan")){
            slot(obj, get_plan_type(value)) <- slot(value, "plan")
        } else {
            # this throws an error if periods don't match and 'cut_to_period=FALSE'
            new_data <- tailor_value_data(
                value=slot(value, "value"),
                period=slot(obj, "period"),
                cut_to_period=cut_to_period,
                warning=warning
            )
            val_type <- slot(value, "type")
            val_cat <- slot(value, "category")
            val_name <- slot(value, "name")
            if(is(value, "revenue")){
                slot(obj, name="revenue") <- update_operations_data(
                    type=val_type,
                    category=val_cat,
                    name=val_name,
                    old_data=get_revenue(obj),
                    new_data=new_data
                )
            } else {
                slot(obj, name="expense") <- update_operations_data(
                    type=val_type,
                    category=val_cat,
                    name=val_name,
                    old_data=get_expense(obj),
                    new_data=new_data
                )
            }
        }
        validObject(obj)
        return(obj)
    }
)


update_operations_data <- function(type, category, name, old_data, new_data){
    row_in_old_data <- old_data[["type"]] %in% type & old_data[["category"]] %in% category & old_data[["name"]] %in% name
    if(any(row_in_old_data)){
        old_data[row_in_old_data, ] <- new_data
    } else {
        old_data[nrow(old_data) + 1, ] <- new_data
    }
    return(old_data)
}
