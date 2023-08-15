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

#' Convert model from list to data frame
#' 
#' Converting a model from list format into a data frame makes it easier to work with nested sub-positions,
#' and to check the model for completeness.
#' 
#' The list provided must have named entries which form the top level of the transaction model.
#' Values are in turn a list with optional named arguments:
#' \itemize{
#'    \item{\code{subpos} }{A named list, nested sub-position to this level, structured like any higher level position.}
#'    \item{\code{carry} }{Name of a previous position of the same level, its value is used as the starting value of this position.}
#'    \item{\code{revenue} }{Character vecotor of valid revenue types, their values are added to the position total.}
#'    \item{\code{expense} }{Character vecotor of valid expense types, their values are subtracted from the position total.}
#' }
#' 
#' @param model A named list describing a transaction model.
#' @param factorize Logical, whether columns not representing a transaction type should be returned as a factor.
#' @return A data frame, representing the model structure that was defined with \code{\link[businessPlanR:table_model]{table_model}}.
#' @docType methods
#' @rdname model2df-methods
#' @export
setGeneric(
    "model2df",
    function(
        model=get_model(),
        factorize=TRUE
    ) standardGeneric("model2df")
)

#' @rdname model2df-methods
#' @export
#' @docType methods
#' @aliases
#'    model2df,-methods
#'    model2df,list-method
setMethod("model2df",
    signature=signature(model="list"),
    function (
        model=get_model(),
        factorize=TRUE
    ){
        list2df(l=model, factorize=factorize)
    }
)


# recursively go through all entries of a list named 'nest'
# and finally return the deepest level of nesting
nest_level <- function(l, nest="subpos", current=1){
    current <- max(sapply(
        l,
        function(x){
            if("subpos" %in% names(x)){
                return(nest_level(x[["subpos"]], nest=nest, current=current + 1))
            } else {
                return(current)
            }
        }
    ))
    return(current)
}


# function calls itself recursively to iterate through sub-positions
list2df <- function(l, subpos_of=c(), levels, df, factorize=TRUE){
    if(missing(levels)){
        levels <- nest_level(l=l)
    } else {}
    pos_col_names <- sapply(
        1:levels,
        function(x){
            paste0(paste0(rep("Sub", x - 1), collapse=""), "Position")
        }
    )
    col_names <- c(pos_col_names, "Context", "Type")
    if(missing(df)){
        df <- as.data.frame(
            matrix(
                data=rep(character(), levels + 1),
                ncol=levels + 2,
                dimnames=list(
                    c(),
                    col_names
                )
            )
        )
    }

    for (this_pos in names(l)){
        subpos <- c(subpos_of, this_pos)
        if("subpos" %in% names(l[[this_pos]])){
            # don't use df, otherwise already processed rows will be duplicated all the time
            next_level_df <- list2df(l=l[[this_pos]][["subpos"]], subpos_of=subpos, levels=levels, factorize=FALSE)
        } else {}
        types <- c(l[[this_pos]][["carry"]], l[[this_pos]][["revenue"]], l[[this_pos]][["expense"]])
        context <- c(
            rep("carry", length(l[[this_pos]][["carry"]])),
            rep("revenue", length(l[[this_pos]][["revenue"]])),
            rep("expense", length(l[[this_pos]][["expense"]]))
        )
        if(!identical(unique(types), types)){
            stop(simpleError(paste0("Names of types in model position \"", this_pos, "\" are not unique!")))
        } else {}
        if(length(subpos) < levels){
            subpos <- c(subpos, rep("", max(0, levels - length(subpos))))
        } else {}
        df <- rbind(
            df,
            as.data.frame(
                matrix(
                    data=c(
                        rep(subpos, each=length(types)),
                        context,
                        types
                    ),
                    ncol=length(pos_col_names) + 2,
                    dimnames=list(
                        c(),
                        col_names
                    )
                ),
                stringsAsFactors=FALSE
            )
        )
        if("subpos" %in% names(l[[this_pos]])){
            df <- rbind(
                df,
                next_level_df,
                stringsAsFactors=FALSE
            )
        } else {}
    }
    if(isTRUE(factorize)){
        nontype_cols <- !colnames(df) %in% "Type"
        df[, nontype_cols] <- lapply(df[, nontype_cols], as.factor)
    } else {}
    return(df)
}
