# Copyright 2022 Meik Michalke <meik.michalke@c3s.cc>
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

#' Define a model node for business plan tables
#' 
#' Tool to define a (possibly nested) model for generating tables for our business plan. The "model" is in fact a nested list.
#' 
#' If you define nested levels, you want to probably only want to combine this node with \code{carry} and neither \code{revenue}
#' nor \code{expense}.
#' 
#' @param ... Optional named lists of nodes (\code{table_model}) or nested sub-nodes (\code{model_node}), like subsections of this
#'     section. You can use \code{model_node} recursive to define these named nodes. Just don't forget to give each a unique name.
#' @param valid_types Optional character string, the name of the type set to use for checking if all used revenue and expense
#'     names are actually valid.
#' @param check_carry Logical, if \code{TRUE} all node names used und the nested list will be looked up if they are referenced by
#'     \code{carry} somewhere down the line.
#' @return A nested, named list.
#' @rdname table_model
#' @export
#' @examples
#' my_model <- table_model(
#'   "Basic Income"=model_node(
#'     revenue="Sale"
#'   ),
#'   "Basic Costs"=model_node(
#'     carry="Basic Income",
#'     expense=c(
#'       "Goods",
#'       "Operation"
#'     )
#'   ),
#'   valid_types="default",
#'   check_carry=TRUE
#' )

table_model <- function(
  ...,
  valid_types,
  check_carry=TRUE
){
  result <- list(...)

  # validity checks
  if(any(!missing(valid_types), isTRUE(check_carry))){
    unlist_result <- unlist(result)

    if(!missing(valid_types)){
      valid_revenue_types <- get_types(name=valid_types, class="revenue", names_only=TRUE)
      valid_expense_types <- get_types(name=valid_types, class="expense", names_only=TRUE)
      check_rev <- grepl(".*revenue$", names(unlist_result))
      check_exp <- grepl(".*expense$", names(unlist_result))
      if(!all(unlist_result[check_rev] %in% valid_revenue_types)){
        stop(simpleError(
          paste0(
            "Undefined revenue types:\n  \"",
            paste0(unique(unlist_result[check_rev][!unlist_result[check_rev] %in% valid_revenue_types]), collapse="\", \""),
            "\"\n\nValid types for \"", valid_types, "\" are:\n  \"",
            paste0(valid_revenue_types, collapse="\", \""),
            "\""
          )
        ))
      } else {}
      if(!all(unlist_result[check_exp] %in% valid_expense_types)){
        stop(simpleError(
          paste0(
            "Undefined expense types:\n  \"",
            paste0(unique(unlist_result[check_exp][!unlist_result[check_exp] %in% valid_expense_types]), collapse="\", \""),
            "\"\n\nValid types for \"", valid_types, "\" are:\n  \"",
            paste0(valid_expense_types, collapse="\", \""),
            "\""
          )
        ))
      } else {}
    } else {}

    if(isTRUE(check_carry)){
      carry_names_used <- unique(unlist_result[grepl(".*carry$", names(unlist_result))])
      if(length(carry_names_used) > 0){
        subpos_defined <- names(unlist_result)[grepl(".*subpos.*", names(unlist_result))]
        missing_subpos <- sapply(
          carry_names_used,
          function(this_name){
            !any(
              grepl(paste0(".*\\.", this_name, "\\."), subpos_defined),
              grepl(paste0(this_name), names(result))
            )
          }
        )
        if(any(missing_subpos)){
          stop(simpleError(
            paste0("Unknown names used with \"carry\":\n  \"", paste0(carry_names_used[missing_subpos], collapse="\", \""), "\"")
          ))
        } else {}
      } else {}
    }
  } else {}

  return(result)
}


#' @param revenue Optional character vector defining names defined as class revenue via \code{\link[businessPlanR:set_types]{set_types}}.
#' @param carry Optional character string, the name of another already defined named list, probably at the same level. The sum of that
#'     list will then be used as the initial value for the calculation of this node.
#' @param expense Optional character vector defining names defined as class expense via \code{\link[businessPlanR:set_types]{set_types}}.
#' @rdname table_model
#' @export
model_node <- function(
  carry,
  ...,
  revenue,
  expense
){
  subpos <- list(...)
  result <- list()
  if(!missing(carry)){
    if(all(!is.character(carry), !identical(carry, c()))){
      stop(simpleError("Invalid value: \"carry\" must be character!"))
    } else {}
    result[["carry"]] <- carry
  } else {}
  if(!missing(revenue)){
    if(all(!is.character(revenue), !identical(revenue, c()))){
      stop(simpleError("Invalid value: \"revenue\" must be character!"))
    } else {}
    result[["revenue"]] <- revenue
  } else {}
  if(!missing(expense)){
    if(all(!is.character(expense), !identical(expense, c()))){
      stop(simpleError("Invalid value: \"expense\" must be character!"))
    } else {}
    result[["expense"]] <- expense
  } else {}
  if(length(subpos) > 0){
    if(!identical(length(unique(names(subpos))), length(subpos))){
      stop(simpleError("Invalid value: sub-nodes must have uniqe names!"))
    } else {}
    result[["subpos"]] <- subpos
  } else {}

  return(result)
}
