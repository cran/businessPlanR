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

#' Update transaction_plan objects
#' 
#' You can use this method to add or replace \code{\link[businessPlanR:depreciation-class]{depreciation}} or
#' \code{\link[businessPlanR:loan-class]{loan}} class objects to/in an existing object of class
#' \code{\link[businessPlanR:transaction_plan-class]{transaction_plan}}.
#' 
#' @docType methods
#' @param obj An object of class \code{\link[businessPlanR:transaction_plan-class]{transaction_plan}}.
#' @param value An object of class \code{\link[businessPlanR:depreciation-class]{depreciation}} or
#'    \code{\link[businessPlanR:loan-class]{loan}}.
#' @return An updated object of class \code{transaction_plan}.
#' @rdname update_plan-methods
#' @aliases update_plan
#' @export
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
setGeneric("update_plan<-", function(obj, value) standardGeneric("update_plan<-"))

#' @rdname update_plan-methods
#' @export
#' @docType methods
#' @aliases
#'    update_plan<-,-methods
#'    update_plan<-,transaction_plan-method
setMethod("update_plan<-",
    signature=signature(obj="transaction_plan"),
    function (obj, value){
        if(!is(value, get_plan_type(obj))){
            stop(simpleError("The value must match 'plan_type' of the object to be updated!"))
        } else {}
        obj_df <- slot(obj, "plan")
        val_type <- slot(value, "type")
        val_cat <- slot(value, "category")
        val_name <- slot(value, "name")
        val_df <- get_value(value)
        # check if there's a mismatch in covered months
        obj_period <- df_date_cols(obj_df, scheme="month")
        val_period <- df_date_cols(val_df, scheme="month")
        if(identical(obj_period, val_period)){
            # easypeasy
            obj_df <- update_plan_data(
                type=val_type,
                category=val_cat,
                name=val_name,
                old_data=obj_df,
                new_data=val_df
            )
        } else {
            if(length(obj_period) > 0){
                all_period <- c(min(obj_period, val_period), max(obj_period, val_period))
                obj_df <- cbind(
                    obj_df[,!names(obj_df) %in% obj_period],
                    tailor_value_data(
                        value=obj_df[, obj_period],
                        period=all_period,
                        cut_to_period=TRUE,
                        warning=FALSE
                    )
                )
                val_df <-  cbind(
                    val_df[,!names(val_df) %in% val_period],
                    tailor_value_data(
                        value=val_df[, val_period],
                        period=all_period,
                        cut_to_period=TRUE,
                        warning=FALSE
                    )
                )
                obj_df <- update_plan_data(
                    type=val_type,
                    category=val_cat,
                    name=val_name,
                    old_data=obj_df,
                    new_data=val_df
                )
                # look for columns full of zeroes at start and end of period
                obj_df <- unzero_cols(df=obj_df)
                # finally, check if this is a loan plan, because we probably have at least one cumsum row
                # that is filled with 0 values at its end now, but it needs to remain at its last value
                if(is(value, "loan")){
                    for(cs_row in which(obj_df[["part"]] %in% "cumsum")){
                        max_cumsum <- max(obj_df[cs_row, df_date_cols(obj_df, scheme="month")], na.rm=TRUE)
                        zero_values <- which(obj_df[cs_row, ] == 0)
                        zero_values_at_end <- zero_values > max(which(obj_df[cs_row, ] == max_cumsum))
                        if(any(zero_values_at_end)){
                            obj_df[cs_row, zero_values[zero_values_at_end]] <- obj_df[cs_row, min(zero_values[zero_values_at_end]) - 1]
                        } else {}
                    }
                }
            } else {
                # obj is empty, replace with value
                obj_df <- val_df
            }
        }
        rownames(obj_df) <- NULL
        slot(obj, name="plan") <- obj_df
        validObject(obj)
        return(obj)
    }
)


update_plan_data <- function(type, category, name, old_data, new_data){
    row_in_old_data <- old_data[["type"]] %in% type & old_data[["category"]] %in% category & old_data[["name"]] %in% name
    if(any(row_in_old_data)){
        old_data[which(row_in_old_data), ] <- new_data
        return(old_data)
    } else {
        return(rbind(
            old_data,
            new_data
        ))
    }
}
