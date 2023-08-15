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

#' S4 Class loan
#'
#' This is a special case of the generic class \code{\link[businessPlanR:transaction-class]{transaction}}.
#' 
#' In contrast to \code{\link[businessPlanR:revenue-class]{revenue}} or \code{\link[businessPlanR:expense-class]{expense}},
#' the time range of this class of objects is defined by details of the loan as specified. Only when used as
#' an aspect of an \code{\link[businessPlanR:operations-class]{operations}} class object, this range is adjusted
#' to fit that particular object.
#'
#' @section Constructor function:
#' Should you need to manually generate objects of this class, the constructor function 
#' \code{loan(...)} can be used instead of
#' \code{new("loan", ...)}.
#'
#' @section: Use in operations:
#' \code{operations} class objects only accept \code{revenue} or \code{expense} objects. Therefore, you might use a respective
#' \code{\link[businessPlanR:as_transaction,-methods]{as_transaction}} method to coerce the \code{loan} object into what you need.
#'
#' @slot type A character string, for valid values see \code{valid_types}. You might use all valid types pre-defined for either
#'    \code{revenue} or \code{expense}, considering that you might be the loan giver or receiver.
#' @slot category A character string, custom category for this loan.
#' @slot name A character string, custom name or ID for this loan.
#' @slot value Data frame containing an amortization schedule for the loan, each month in a row named \code{YYYY.MM}.
#'    It has a row for each month and the columns \code{balance_start}, \code{interest}, \code{principal}, \code{total},
#'    \code{cumsum}, and \code{balance_remain}. 
#' @slot valid_types A character string, the model types defined by
#'    \code{\link[businessPlanR:set_types]{set_types}} to be used for validation.
#' @name loan,-class
#' @aliases loan-class
#' @import methods
#' @keywords classes
#' @export loan
#' @exportClass loan
#' @rdname loan-class
#' @examples
#' loan_2019 <- loan(
#'     type="Interest",
#'     category="Bank",
#'     name="New office",
#'     amount=10000,
#'     period=60,
#'     interest=0.075,
#'     first_month="2019.04",
#'     schedule=c("amortization")
#' )
#' 
#' # turn loan object into an expense
#' loan_as_expense_2019 <- as_transaction(
#'     loan_2019,
#'     to="expense",
#'     aspect="interest"
#' )
loan <- setClass("loan",
    contains="transaction"
)

#' @param .Object The object to initialize.
#' @param type A character string defining the type of transaction as defined by \code{valid_types}.
#' @param category A character string, custom category for this transaction.
#' @param name A character string, custom name or ID for this transaction.
#' @param amount Numeric, the amount of money loaned.
#' @param period Integer, number of months to fully repay the loan.
#' @param interest Numeric, the nominal interest rate per annum (a value between 0 and 1).
#' @param first_month Character string in \code{YYYY.MM} format, defining the initial date of the loan.
#' @param schedule One of the following, defining the repayment schedule:
#'    \itemize{
#'        \item{\code{"annuity"}: }{Equal rates of total repayment over \code{period}, thereby interest is relatively higher
#'            and principal payment relatively lower at the beginning.}
#'        \item{\code{"amortization"}: }{Repayment of equal rates of principal payment with decreasing interest and total payments over \code{period}.}
#'        \item{\code{"maturity"}: }{Repayment of the full loan amount at the end of \code{period}, until then only payment of interest.}
#'    }
#' @param due_month Integer value defining the first month of principal repayment. The selected \code{schedule} will not begin before this month,
#'    until then only interest rates are due.
#'    Beware that this is a different behaviour of this argument compared to \code{\link[businessPlanR:transaction-class]{transaction}}.
#' @param valid_types A character string, the model types defined by \code{\link[businessPlanR:set_types]{set_types}} to be used for validation.
#'    If \code{"default"}, pre-defined example types are used.
#' @param value A valid data frame to be used as the value slot directly, omitting calculation via \code{amount}, \code{period}, \code{interest}, etc.
#' @rdname loan-class
setMethod("initialize", "loan",
    function(
        .Object,
        type,
        category,
        name,
        amount,
        period,
        interest,
        first_month=format(Sys.Date(), "%Y.%m"),
        schedule=c("annuity", "amortization", "maturity"),
        due_month=NA,
        valid_types="default",
        value
    ){
        slot(.Object, "type") <- type
        slot(.Object, "category") <- category
        slot(.Object, "name") <- name
        if(missing(value)){
            slot(.Object, "value") <- loan_redemption(
                amount=amount,
                period=period,
                interest=interest,
                due_month=due_month,
                first_month=first_month,
                schedule=schedule
            )
        } else {
            slot(.Object, "value") <- value
        }
        slot(.Object, "valid_types") <- valid_types
        validObject(.Object)
        return(.Object)
    }
)

setValidity("loan", function(object){
    type <- slot(object, "type")
    valid_types <- slot(object, "valid_types")

    validTypes_loans <- c(
      get_types(name=valid_types, class="revenue", names_only=TRUE),
      get_types(name=valid_types, class="expense", names_only=TRUE)
    )
    if(!type %in% validTypes_loans){
        stop(simpleError(paste0("Slot \"type\" must be one of:\n  \"", paste0(validTypes_loans, collapse="\", \""), "\"")))
    } else {}

    return(TRUE)
})
