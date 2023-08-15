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

#' S4 Class revenue
#'
#' This is a special case of the generic class \code{\link[businessPlanR:transaction-class]{transaction}}.
#'
#' @section Constructor function:
#' Should you need to manually generate objects of this class, the constructor function 
#' \code{revenue(...)} can be used instead of
#' \code{new("revenue", ...)}.
#' 
#' @slot type A character string, for valid values see \code{valid_types}.
#' @slot category A character string, custom category for this revenue.
#' @slot name A character string, custom name or ID for this revenue.
#' @slot value Data frame containing all revenues, each month in a column named \code{YYYY.MM}.
#' @slot valid_types A character string, the model types defined by
#'    \code{\link[businessPlanR:set_types]{set_types}} to be used for validation.
#' @name revenue,-class
#' @aliases revenue-class
#' @import methods
#' @keywords classes
#' @export revenue
#' @exportClass revenue
#' @rdname revenue-class
#' @examples
#' rev_2019_2021 <- revenue(
#'    type="Sale",
#'    category="Merch",
#'    name="T-Shirts",
#'    "2019.03"=100,
#'    "2019.08"=267,
#'    "2020.03"=344,
#'    "2020.09"=549,
#'    "2021.02"=770,
#'    "2021.07"=1022,
#'    "2021.10"=1263
#' )
revenue <- setClass("revenue",
    contains="transaction"
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
#' @rdname revenue-class
setMethod("initialize", "revenue",
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
        .Object <- callNextMethod()
        validObject(.Object)
        return(.Object)
    }
)

setValidity("revenue", function(object){
    type <- slot(object, "type")
    value <- slot(object, "value")
    valid_types <- slot(object, "valid_types")

    validTypes_revenue <- get_types(name=valid_types, class="revenue", names_only=TRUE)
    if(!type %in% validTypes_revenue){
        stop(simpleError(paste0("Slot \"type\" must be one of:\n  \"", paste0(validTypes_revenue, collapse="\", \""), "\"")))
    } else {}

    if(nrow(value) > 1){
        stop(simpleError("Slot \"value\" must have one row!"))
    } else {}

    return(TRUE)
})
