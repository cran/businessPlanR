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

#' Condense operations objects into neat data frame
#' 
#' Uses the provided \code{model} to create a data frame from the
#' \code{\link[businessPlanR:operations-class]{operations}} object.
#' Depending on the type of data frame requestet (i.e., default or cashflow)
#' and the temporal resolution (month, quarter or year), various subsets of
#' the overall data in \code{obj} are returned.
#' 
#' @param obj An object of class \code{\link[businessPlanR:operations-class]{operations}}.
#' @param model A named list of named lists describing the stepwise accounting rules
#'    for all data in in \code{obj}.
#' @param resolution One of \code{"month"}, \code{"quarter"}, or \code{"year"}.
#' @param keep_types Logical, whether the returned data frame should keep the intermediate results
#'    for each relevant type of transaction. This will add a column \code{type} to the data frame.
#' @param cashflow Logical, whether the \code{model} describes a cashflow plan. If \code{TRUE},
#'    calculations will start with the initial value as specified by \code{cf_init} and use the
#'    result of each period as the starting value of following periods.
#' @param cf_init Numeric, used as the initial value for cashflow calculations if \code{cashflow=TRUE}; i.e.,
#'    the first beginning cash value.
#' @param cf_names Character vector with two entries named \code{begin} and \code{end}, used in the resulting
#'    table for beginning cash and ending cash.
#' @param years Character (or numeric) vector defining the year(s) to be represented in the output. This is intended to be
#'    useful for splitting up quarterly or monthly output.
#' @param digits Number of digits used for rounding values, disabled if set to \code{NA}.
#' @return A data frame with a subset of the financial transactions of \code{obj}.
#' @docType methods
#' @rdname condense-methods
#' @aliases condense-methods
#' @export
setGeneric(
    "condense",
    function(
        obj,
        model=get_model(),
        resolution=c("year", "quarter", "month"),
        keep_types=TRUE,
        cashflow=FALSE,
        cf_init=0,
        cf_names=c(
            begin="Begin",
            end="End"
        ),
        years=get_period(obj, years=TRUE),
        digits=2
    ) standardGeneric("condense")
)

#' @rdname condense-methods
#' @export
#' @docType methods
#' @aliases
#'    condense,-methods
#'    condense,operations-method
setMethod("condense",
    signature=signature(obj="operations"),
    function(
        obj,
        model=get_model(),
        resolution=c("year", "quarter", "month"),
        keep_types=TRUE,
        cashflow=FALSE,
        cf_init=0,
        cf_names=c(
            begin="Begin",
            end="End"
        ),
        years=get_period(obj, years=TRUE),
        digits=2
    ){
        resolution <- match.arg(resolution)
        revenue <- get_revenue(obj, resolution=resolution)
        expense <- get_expense(obj, resolution=resolution)
        model_df <- model2df(model=model, factorize=FALSE)
        m_df_revenue <- model_df[["Context"]] %in% "revenue"
        m_df_expense <- model_df[["Context"]] %in% "expense"
        date_cols <- df_date_cols(revenue, scheme=resolution)

        model_df <- cbind(
            model_df,
            matrix(
                0,
                nrow=nrow(model_df),
                ncol=length(date_cols),
                dimnames=list(
                    c(),
                    date_cols
                )
            )
        )
        empty_rows <- c()
        for (r in 1:nrow(model_df)){
            type_context <- model_df[r, "Context"]
            if(type_context %in% "revenue"){
                rev_type_in_model <- revenue[["type"]] %in% model_df[r, "Type"]
                if(any(rev_type_in_model)){
                    model_df[r, date_cols] <- colSums(revenue[rev_type_in_model, date_cols])
                } else {
                    empty_rows <- c(empty_rows, r)
                }
            } else if(type_context %in% "expense"){
                exp_type_in_model <- expense[["type"]] %in% model_df[r, "Type"]
                if(any(exp_type_in_model)){
                    model_df[r, date_cols] <- colSums(expense[exp_type_in_model, date_cols]) * -1
                } else {
                    empty_rows <- c(empty_rows, r)
                }
            } else if(!type_context %in% "carry"){
                stop(simpleError(paste0("Unknown type context: \"", type_context, "\"")))
            }
        }
        if(length(empty_rows) > 0){
            model_df <- model_df[-empty_rows, ]
        } else {}
        model_df <- add_sum_rows(model_df, resolution=resolution)
        model_df <- model_df[, !colnames(model_df) %in% "Context"]

        if(isTRUE(cashflow)){
            begin_row <- fill_missing_cols(
                df=data.frame(
                    Position=cf_names[["begin"]],
                    Type=cf_names[["begin"]],
                    matrix(c(cf_init, rep(0, length(date_cols) - 1)), nrow=1, dimnames=list(c(), date_cols)),
                    check.names=FALSE,
                    stringsAsFactors=FALSE),
                template=model_df
            )
            end_row <- fill_missing_cols(
                df=data.frame(
                    Position=cf_names[["end"]],
                    Type=cf_names[["end"]],
                    matrix(0,nrow=1, ncol=length(date_cols), dimnames=list(c(), date_cols)),
                    check.names=FALSE,
                    stringsAsFactors=FALSE
                ),
                template=model_df
            )
            model_df <- rbind(
                begin_row,
                model_df,
                end_row
            )
            for (thisCol_n in seq_along(date_cols)){
                thisCol <- date_cols[[thisCol_n]]
                if(isTRUE(keep_types)){
                    model_df[model_df[["Position"]] %in% cf_names[["end"]], thisCol] <- sum(model_df[model_df[["Type"]] %in% "Sum" | model_df[["Position"]] %in% cf_names[["begin"]], thisCol])
                } else {
                    model_df[model_df[["Position"]] %in% cf_names[["end"]], thisCol] <- sum(model_df[!model_df[["Position"]] %in% cf_names[["end"]], thisCol])
                }
                if(thisCol_n < length(date_cols)){
                    model_df[model_df[["Position"]] %in% cf_names[["begin"]], date_cols[[thisCol_n + 1]]] <- model_df[model_df[["Position"]] %in% cf_names[["end"]], thisCol]
                } else {}
            }
        } else {}
        
        if(!all(get_period(obj, years=TRUE) %in% years)){
            all_cols <- colnames(model_df)
            model_df <- model_df[, all_cols[!all_cols %in% df_date_cols(model_df, scheme=resolution) | all_cols %in% df_date_cols(model_df, scheme=resolution, years=years)]]
        } else {}

        rownames(model_df) <- NULL
        empty_cols <- sapply(
            model_df,
            function(col){
                !sum(nchar(as.character(col))) > 0
            }
        )
        if(any(empty_cols)){
            model_df <- model_df[, !empty_cols]
        } else {}
        if(!is.na(digits)){
            model_df[, df_date_cols(model_df, scheme=resolution)] <- round(model_df[, df_date_cols(model_df, scheme=resolution)], digits=digits)
        } else {}
        if(isTRUE(keep_types)){
            return(model_df)
        } else {
            return(model_df[, !colnames(model_df) %in% "Type"])
        }
    }
)
