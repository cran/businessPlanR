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

#' S4 Class depreciation
#'
#' This is a special case of the generic class \code{\link[businessPlanR:transaction-class]{transaction}}.
#' 
#' In contrast to \code{\link[businessPlanR:revenue-class]{revenue}} or \code{\link[businessPlanR:expense-class]{expense}},
#' the time range of this class of objects is defined by details of the investment as specified. Only when used as
#' an aspect of an \code{\link[businessPlanR:operations-class]{operations}} class object, this range is adjusted
#' to fit that particular object.
#'
#' @section Constructor function:
#' Should you need to manually generate objects of this class, the constructor function 
#' \code{depreciation(...)} can be used instead of
#' \code{new("depreciation", ...)}.
#'
#' @section: Use in operations:
#' \code{operations} class objects only accept \code{revenue} or \code{expense} objects. Therefore, you might use a respective
#' \code{\link[businessPlanR:as_transaction,-methods]{as_transaction}} method to coerce the \code{depreciation} object into what you need.
#'
#' @slot type A character string, for valid values see \code{valid_types}. You might use all valid types pre-defined for either
#'    \code{revenue} or \code{expense}, considering that you might be the depreciation giver or receiver.
#' @slot category A character string, custom category for this depreciation.
#' @slot name A character string, custom name or ID for this depreciation.
#' @slot value Data frame containing an investment plan and allowance for depreciation balance, each month in a row named \code{YYYY.MM}.
#'    The columns are \code{investment}, \code{depreciation}, and remaining \code{value}.
#' @slot valid_types A character string, the model types defined by
#'    \code{\link[businessPlanR:set_types]{set_types}} to be used for validation.
#' @name depreciation,-class
#' @aliases depreciation-class
#' @import methods
#' @keywords classes
#' @export depreciation
#' @exportClass depreciation
#' @rdname depreciation-class
#' @examples
#' depreciation_printer <- depreciation(
#'     type="Depreciation",
#'     category="Office",
#'     name="Printer",
#'     amount=100,
#'     obsolete=36,
#'     invest_month="2019.04"
#' )
#' 
#' # turn depreciation object into an expense
#' depreciation_as_expense_printer <- as_transaction(
#'     depreciation_printer,
#'     to="expense",
#'     aspect="depreciation"
#' )
depreciation <- setClass("depreciation",
    contains="transaction"
)

#' @param .Object The object to initialize.
#' @param type A character string defining the type of transaction as defined by \code{valid_types}.
#' @param category A character string, custom category for this transaction.
#' @param name A character string, custom name or ID for this transaction (i.e., a particular asset that was purchased).
#' @param amount Numeric, the amount of money invested into the asset.
#' @param obsolete Integer value defining the period (in months) over which the value of the asset diminishes to zero.
#' @param invest_month Character string in \code{YYYY.MM} format, the month of the investment/purchase.
#' @param method One of the following, defining the depreciation method:
#'    \itemize{
#'        \item{\code{"linear"}: }{The straight line depreciation. This is currently the only implemented option.}
#'        \item{\code{"writedown"}: }{The written-down value depreciation, not yet implemented.}
#'        \item{\code{"sumofyears"}: }{The sum-of-years depreciation, not yet implemented.}
#'        \item{\code{"doubledecline"}: }{The double-declining depreciation, not yet implemented.}
#'    }
#' @param valid_types A character string, the model types defined by \code{\link[businessPlanR:set_types]{set_types}} to be used for validation.
#'    If \code{"default"}, pre-defined example types are used.
#' @param value A valid data frame to be used as the value slot directly, omitting calculation via \code{amount}, \code{obsolete}, \code{invest_month}, etc.
#' @rdname depreciation-class
setMethod("initialize", "depreciation",
    function(
        .Object,
        type,
        category,
        name,
        amount,
        obsolete,
        invest_month=format(Sys.Date(), "%Y.%m"),
        method=c("linear", "writedown", "sumofyears", "doubledecline"),
        valid_types="default",
        value
    ){
        slot(.Object, "type") <- type
        slot(.Object, "category") <- category
        slot(.Object, "name") <- name
        if(missing(value)){
            slot(.Object, "value") <- obsolescence(
                amount=amount,
                obsolete=obsolete,
                invest_month=invest_month,
                method=method
            )
        } else {
            slot(.Object, "value") <- value
        }
        slot(.Object, "valid_types") <- valid_types
        validObject(.Object)
        return(.Object)
    }
)

setValidity("depreciation", function(object){
    type <- slot(object, "type")
    valid_types <- slot(object, "valid_types")

    validTypes_depreciations <- c(
      get_types(name=valid_types, class="revenue", names_only=TRUE),
      get_types(name=valid_types, class="expense", names_only=TRUE)
    )
    if(!type %in% validTypes_depreciations){
        stop(simpleError(paste0("Slot \"type\" must be one of:\n  \"", paste0(validTypes_depreciations, collapse="\", \""), "\"")))
    } else {}

    return(TRUE)
})
