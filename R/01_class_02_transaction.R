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

#' S4 Class transaction
#'
#' This is a generic class used by subclasses \code{\link[businessPlanR:revenue-class]{revenue}} and \code{\link[businessPlanR:expense-class]{expense}}.
#'
#' @section Constructor function:
#' Should you need to manually generate objects of this class, the constructor function 
#' \code{transaction(...)} can be used instead of \code{new("transaction", ...)}. It
#' uses the same arguments like the \code{initialize()} method.
#'
#' You should either provide exactly one named value for each month of the full scope
#' of the respective business plan, or at least two, representing the first and last
#' value.
#' 
#' @section Missing values:
#' How missing values are dealt with depends on the value of the \code{missing} parameter.
#' By default (\code{missing="rep"}) a given value will be repeated until a later value comes,
#' which will then be repeated further on. That is, you can define a staring value and only have to
#' provide updated values for months that differ from the previous value.
#' Alternatively, \code{missing="interpol"} will interpolate missing values linearly, and
#' \code{missing="0"} fills missing values with zeroes.
#'
#' @slot type A character string, valid values are defined by the subclasses.
#' @slot category A character string, custom category for this transaction.
#' @slot name A character string, custom name or ID for this transaction.
#' @slot value Data frame containing all transactions, each month of each year in a column named \code{YYYY.MM}.
#' @slot valid_types A character string, the model types defined by
#'    \code{\link[businessPlanR:set_types]{set_types}} to be used for validation.
#' @name transaction,-class
#' @aliases transaction-class
#' @import methods
#' @keywords classes
#' @export transaction
#' @exportClass transaction
#' @rdname transaction-class
transaction <- setClass("transaction",
    representation=representation(
        type="character", # one of the valid types
        category="character", # custom category
        name="character", # custom name/reference
        value="data.frame", # one column for each month of each year, colnames be YYYY.MM
        valid_types="character" # name for set of types
    )
)

#' @param .Object The object to initialize.
#' @param type A character string defining the type of transaction as defined by \code{valid_types}.
#' @param category A character string, custom category for this transaction.
#' @param name A character string, custom name or ID for this transaction.
#' @param per_use If given, the numbers provided via \code{...} (or \code{.list}) are
#'    not interpreted as the monetary value, but as number of transactions in that
#'    month, and the actual fiscal value is calculated by multiplying it with the value
#'    given here.
#' @param missing One of \code{"rep"}, \code{"interpol"}, or \code{"0"}.
#'    This defines how gaps are filled: If \code{"rep"}, present values are repeated
#'    until the next valid value; if \code{"interpol"}, missing values are interpolated
#'    using \code{approx}; if \code{"0"}, missing values are set to zero.
#' @param due_month Character vector to define months where transactions are due. This argument
#'    causes previous amounts to be cumulated and thereby postponed to the given month of a year.
#'    Combined with e.g. \code{.list} this makes it easier to turn monthly amounts into quarterly
#'    ones.
#' @param valid_types A character string, the model types defined by
#'    \code{\link[businessPlanR:set_types]{set_types}} to be used for validation.
#'    If \code{"default"}, pre-defined example types are used.
#' @param ... Numeric values named in \code{YYYY.MM} format, defining the transaction amount for a particular month.
#'    The resulting object will automatically cover all months from the earliest to the latest among all given values.
#' @param .list An alternative to \code{...} if the values are already present as a list. If both are given, their values
#'    will be merged into one list.
#' @rdname transaction-class
setMethod("initialize", "transaction",
    function(
        .Object,
        type,
        category,
        name,
        per_use,
        missing=c("rep", "interpol", "0"),
        due_month=NA,
        valid_types="default",
        ...,
        .list=list() # will be appended to "..." if present
    ){
        slot(.Object, "type") <- type
        slot(.Object, "category") <- category
        slot(.Object, "name") <- name
        value_list <- list(...)
        if(is.list(.list) & length(.list) > 0){
            value_list <- append(value_list, .list)
        } else {}
        if(!missing(per_use)){
            value_list <- lapply(value_list, "*", per_use)
        }
        slot(.Object, "value") <- value_list2month_df(
            value_list=value_list,
            type=type,
            category=category,
            name=name,
            missing=missing,
            due_month=due_month
        )
        slot(.Object, "valid_types") <- valid_types
        validObject(.Object)
        return(.Object)
    }
)

setValidity("transaction", function(object){
    type <- slot(object, "type")
    name <- slot(object, "name")
    category <- slot(object, "category")
    value <- slot(object, "value")
    valid_types <- slot(object, "valid_types")

    if(length(name) > 1){
        stop(simpleError("Slot \"name\" must have one value only!"))
    } else {}

    if(length(category) > 1){
        stop(simpleError("Slot \"category\" must have one value only!"))
    } else {}

    all_valid_types <- c(
        get_types(name=valid_types, class="revenue", names_only=TRUE),
        get_types(name=valid_types, class="expense", names_only=TRUE)
    )
    if(!type %in% all_valid_types){
        stop(simpleError(paste0("Slot \"type\" must be one of:\n  \"", paste0(all_valid_types, collapse="\", \""), "\"")))
    } else {}
    
    return(TRUE)
})
