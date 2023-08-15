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

#' Define valid types of revenues and expenses
#' 
#' These functions change the globally available options of the running R session.
#' Its values define types of transactions you want to be able to use in your
#' business plan.
#' 
#' The getter functions return a list of default types if none have been defined so far.
#' 
#' @param types Named list, one entry for each type. Values define the color to use in plots.
#' @param class One of \code{"revenue"} or \code{"expense"}.
#' @param name Character string, giving the set of types a name. You can use this to have multiple
#'    sets of types simultaneously in the same session.
#' @return \code{set_types} is a wrapper for \code{\link[base:options]{options}} and adds/replaces a list
#'    called \code{name} to the \code{businessPlanR} option of the running session.
#'    \code{get_types} returns the list from the \code{businessPlanR} option.
#'    \code{get_model} just returns the internal definition of default operations model as a list.
#' @rdname options
#' @export
set_types <- function(
    types,
    class=c("revenue", "expense"),
    name="default"
){
    class <- match.arg(class)
    if(!is.list(types)){
        stop(simpleError("\"types\" must be a named list!"))
    } else {}
    new_op <- getOption("businessPlanR")
    if(is.null(new_op)){
        new_op <- list(
            list(
                types=list(
                    class=types
                )
            )
        )
        names(new_op) <- name
        names(new_op[[name]][["types"]]) <- class
    } else if(!name %in% names(new_op)){
        new_op[[name]] <- list(
            types=list(
                class=types
            )
        )
        names(new_op[[name]][["types"]]) <- class
    } else {
        new_op[[name]][["types"]][[class]] <- types
    }
    options(businessPlanR=new_op)
}


#' @param names_only Logical, whether the full list or only the names of defined types should be returned.
#' @rdname options
#' @export
get_types <- function(
    name="default",
    class=c("revenue", "expense"),
    names_only=FALSE
){
    class <- match.arg(class)
    opt <- getOption("businessPlanR")[[name]][["types"]][[class]]
    if(is.null(opt)){
        opt <- validTypes[[class]]
    } else {}
    if(isTRUE(names_only)){
        return(names(opt))
    } else {
        return(opt)
    }
}


#' @rdname options
#' @export
get_model <- function(){
    ## TODO: this is still a dummy so it can be used in other functions/methods
    return(default_operations_model)
}


# global (internal) definition of default categories
validTypes <- list(
    revenue=list(
        "Sale"=rgb(0.3,0.9,0.4,0.8),
        "Invest income"=rgb(0.2,0.9,0.3,0.8)
    ),
    expense=list(
        "Goods"="darkorange",
        "Operation"="darkred",
        "Depreciation"="red",
        "Interest"="orangered",
        "Loan ammortization"="orange"
    )
)

# global (internal) definition of default operations model
default_operations_model <- list(
    "Gross revenue"=list(
        # - various sources of revenues
        # - all income
        revenue=c("Sale")
    ),
    "Gross profit"=list(
        # - gross revenue minus costs of goods (i.e., production)
        carry=c("Gross revenue"),
        expense=c("Goods")
    ),
    "Operating profit"=list(
        # - gross profit minus operating expenses (sales, general, and administration expenses; rent, salaries, travel, marketing...), and
        #   minus depreciation expenses (investments that lost value; e.g., cars, PCs)
        carry=c("Gross profit"),
        expense=c("Operation", "Depreciation")
    ),
    "Profit before taxes"=list(
        # - operating profit + investment income minus interest expenses
        carry=c("Operating profit"),
        revenue=c("Invest income"),
        expense=c("Interest")
    ),
    "Net profit"=list(
        # - profit after taxes
        carry=c("Profit before taxes"),
        expense=c()
    )
)
