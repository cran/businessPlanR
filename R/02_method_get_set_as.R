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

#' Getter/setter methods for businessPlanR objects
#' 
#' These methods return the requested slots from objects of class
#' \code{\link[businessPlanR:operations-class]{operations}},
#' \code{\link[businessPlanR:revenue-class]{revenue}},
#' \code{\link[businessPlanR:expense-class]{expense}},
#' \code{\link[businessPlanR:transaction_plan-class]{transaction_plan}},
#' \code{\link[businessPlanR:loan-class]{loan}} or
#' \code{\link[businessPlanR:depreciation-class]{depreciation}},
#' or, in case of their \code{<-} counterparts, replace slots with a given \code{value}.
#'
#' If \code{as_transaction(..., aspect="balance_start")} is being called on a loan object, only the initial
#' value (and perhaps growth instead of declining values) is used, e.g. as revenue for calculations.
#' 
#' @docType methods
#' @param obj An object of class
#' \code{\link[businessPlanR:operations-class]{operations}},
#' \code{\link[businessPlanR:revenue-class]{revenue}},
#' \code{\link[businessPlanR:expense-class]{expense}},
#' \code{\link[businessPlanR:transaction_plan-class]{transaction_plan}},
#' \code{\link[businessPlanR:loan-class]{loan}} or
#' \code{\link[businessPlanR:depreciation-class]{depreciation}}.
#' @param drop_nonyear_cols Logical, whether to drop or keep columns specifying type, category or name or rows.
#' @param resolution One of \code{"month"}, \code{"quarter"}, or \code{"year"}.
#' @param only_type Optional character vector, if given, only rows with matching type are returned.
#'    Overrides \code{not_type} if both are provided.
#' @param not_type Optional character vector, if given, only rows with types not matching the vector entries are returned.
#' @return Depending on the method, either a data frame or a numeric value.
#' @rdname get_set_as-methods
#' @export

##############
## get_revenue
setGeneric(
    "get_revenue",
    function(
        obj,
        drop_nonyear_cols=FALSE,
        resolution=c("month", "quarter", "year"),
        only_type,
        not_type
    ) standardGeneric("get_revenue")
)

#' @rdname get_set_as-methods
#' @export
#' @docType methods
#' @aliases
#'    get_revenue,-methods
#'    get_revenue,operations-method
setMethod("get_revenue",
    signature=signature(obj="operations"),
    function (
        obj,
        drop_nonyear_cols=FALSE,
        resolution=c("month", "quarter", "year"),
        only_type,
        not_type
    ){
        return(
            get_formatted_df(
                obj,
                slot="revenue",
                drop_nonyear_cols=drop_nonyear_cols,
                resolution=resolution,
                only_type=only_type,
                not_type=not_type
            )
        )
    }
)


##############
## get_expense

#' @docType methods
#' @rdname get_set_as-methods
#' @export
setGeneric(
    "get_expense",
    function(
        obj,
        drop_nonyear_cols=FALSE,
        resolution=c("month", "quarter", "year"),
        only_type,
        not_type
    ) standardGeneric("get_expense")
)

#' @rdname get_set_as-methods
#' @export
#' @docType methods
#' @aliases
#'    get_expense,-methods
#'    get_expense,operations-method
setMethod("get_expense",
    signature=signature(obj="operations"),
    function (
        obj,
        drop_nonyear_cols=FALSE,
        resolution=c("month", "quarter", "year"),
        only_type,
        not_type
    ){
        return(
            get_formatted_df(
                obj,
                slot="expense",
                drop_nonyear_cols=drop_nonyear_cols,
                resolution=resolution,
                only_type=only_type,
                not_type=not_type
            )
        )
    }
)


############
## get_value

#' @docType methods
#' @rdname get_set_as-methods
#' @export
setGeneric(
    "get_value",
    function(
        obj,
        drop_nonyear_cols=FALSE,
        resolution=c("month", "quarter", "year")
    ) standardGeneric("get_value")
)

#' @rdname get_set_as-methods
#' @export
#' @docType methods
#' @aliases
#'    get_value,-methods
#'    get_value,transaction_plan-method
setMethod("get_value",
    signature=signature(obj="transaction_plan"),
    function (
        obj,
        drop_nonyear_cols=FALSE,
        resolution=c("month", "quarter", "year")
    ){
        return(
            get_formatted_df(
                obj,
                slot="plan",
                drop_nonyear_cols=drop_nonyear_cols,
                resolution=resolution,
                transaction_type=get_plan_type(obj)
            )
        )
    }
)

#' @rdname get_set_as-methods
#' @export
#' @docType methods
#' @aliases
#'    get_value,-methods
#'    get_value,loan-method
setMethod("get_value",
    signature=signature(obj="loan"),
    function (
        obj,
        drop_nonyear_cols=FALSE,
        resolution=c("month", "quarter", "year")
    ){
        return(
            get_formatted_df(
                obj,
                slot="value",
                drop_nonyear_cols=drop_nonyear_cols,
                resolution=resolution
            )
        )
    }
)

#' @rdname get_set_as-methods
#' @export
#' @docType methods
#' @aliases
#'    get_value,-methods
#'    get_value,depreciation-method
setMethod("get_value",
    signature=signature(obj="depreciation"),
    function (
        obj,
        drop_nonyear_cols=FALSE,
        resolution=c("month", "quarter", "year")
    ){
        return(
            get_formatted_df(
                obj,
                slot="value",
                drop_nonyear_cols=drop_nonyear_cols,
                resolution=resolution
            )
        )
    }
)

#' @rdname get_set_as-methods
#' @export
#' @docType methods
#' @aliases
#'    get_value,-methods
#'    get_value,revenue-method
setMethod("get_value",
    signature=signature(obj="revenue"),
    function (
        obj,
        drop_nonyear_cols=FALSE,
        resolution=c("month", "quarter", "year")
    ){
        return(
            get_formatted_df(
                obj,
                slot="value",
                drop_nonyear_cols=drop_nonyear_cols,
                resolution=resolution
            )
        )
    }
)

#' @rdname get_set_as-methods
#' @export
#' @docType methods
#' @aliases
#'    get_value,-methods
#'    get_value,expense-method
setMethod("get_value",
    signature=signature(obj="expense"),
    function (
        obj,
        drop_nonyear_cols=FALSE,
        resolution=c("month", "quarter", "year")
    ){
        return(
            get_formatted_df(
                obj,
                slot="value",
                drop_nonyear_cols=drop_nonyear_cols,
                resolution=resolution
            )
        )
    }
)


############
## get_loans

#' @docType methods
#' @rdname get_set_as-methods
#' @export
setGeneric(
    "get_loans",
    function(
        obj,
        as_data_frame=TRUE
    ) standardGeneric("get_loans")
)

#' @rdname get_set_as-methods
#' @export
#' @docType methods
#' @aliases
#'    get_loans,-methods
#'    get_loans,operations-method
setMethod("get_loans",
    signature=signature(obj="operations"),
    function (
        obj,
        as_data_frame=TRUE
    ){
        if(isTRUE(as_data_frame)){
            return(slot(obj, "loan"))
        } else {
            result <- transaction_plan(
                plan_type="loan",
                plan=slot(obj, "loan")
            )
            return(result)
        }
    }
)


###########
## get_plan

#' @param category A character string, custom category for this transaction.
#' @param valid_types A character string, the model types defined by
#'    \code{\link[businessPlanR:set_types]{set_types}} to be used for validation.
#'    If \code{"default"}, pre-defined example types are used.
#' @docType methods
#' @rdname get_set_as-methods
#' @export
setGeneric(
    "get_plan",
    function(
        obj,
        type,
        category,
        name,
        valid_types="default"
    ) standardGeneric("get_plan")
)

#' @rdname get_set_as-methods
#' @export
#' @docType methods
#' @aliases
#'    get_plan,-methods
#'    get_plan,transaction_plan-method
setMethod("get_plan",
    signature=signature(obj="transaction_plan"),
    function (
        obj,
        type,
        category,
        name,
        valid_types="default"
    ){
        all_plans <- slot(obj, "plan")
        if(!type %in% all_plans[["type"]]){
            warning(paste0("'type' not found in object: \"", type, "\""), call.=FALSE)
        } else {}
        if(!category %in% all_plans[["category"]]){
            warning(paste0("'category' not found in object: \"", category, "\""), call.=FALSE)
        } else {}
        if(!name %in% all_plans[["name"]]){
            warning(paste0("'name' not found in object: \"", name, "\""), call.=FALSE)
        } else {}
        relevant_rows <- all_plans[["type"]] %in% type & all_plans[["category"]] %in% category & all_plans[["name"]] %in% name
        value <- all_plans[relevant_rows, ]
        rownames(value) <- value[["part"]]
        return(new(
            get_plan_type(obj),
            type=type,
            category=category,
            name=name,
            valid_types=valid_types,
            value=as.data.frame(t(unzero_cols(value[, df_date_cols(x=value, scheme="month")])))
        ))
    }
)


#############
## get_period

#' @docType methods
#' @rdname get_set_as-methods
#' @export
setGeneric(
    "get_period",
    function(
        obj,
        years=FALSE
    ) standardGeneric("get_period")
)

#' @rdname get_set_as-methods
#' @param years Logical, if \code{TRUE} doesn't return the period vector but a vector of all years in the period.
#' @export
#' @docType methods
#' @aliases
#'    get_period,-methods
#'    get_period,operations-method
setMethod("get_period",
    signature=signature(obj="operations"),
    function (
        obj,
        years=FALSE
    ){
        period <- slot(obj, "period")
        if(isTRUE(years)){
            return(df_years(x=period, fill_missing=TRUE))
        } else {
            return(period)
        }
    }
)

#' @rdname get_set_as-methods
#' @export
#' @docType methods
#' @aliases
#'    get_period,-methods
#'    get_period,transaction_plan-method
setMethod("get_period",
    signature=signature(obj="transaction_plan"),
    function (
        obj,
        years=FALSE
    ){
        period <- df_date_cols(slot(obj, "plan"), scheme="month")
        if(isTRUE(years)){
            return(df_years(x=period, fill_missing=TRUE))
        } else {
            return(period)
        }
    }
)

#' @rdname get_set_as-methods
#' @export
#' @docType methods
#' @aliases
#'    get_period,-methods
#'    get_period,loan-method
setMethod("get_period",
    signature=signature(obj="loan"),
    function (
        obj,
        years=FALSE
    ){
        period <- rownames(slot(obj, "value"))
        if(isTRUE(years)){
            return(df_years(x=period, fill_missing=TRUE))
        } else {
            return(period)
        }
    }
)

#' @rdname get_set_as-methods
#' @export
#' @docType methods
#' @aliases
#'    get_period,-methods
#'    get_period,depreciation-method
setMethod("get_period",
    signature=signature(obj="depreciation"),
    function (
        obj,
        years=FALSE
    ){
        period <- rownames(slot(obj, "value"))
        if(isTRUE(years)){
            return(df_years(x=period, fill_missing=TRUE))
        } else {
            return(period)
        }
    }
)


########################
## get_depreciation_plan

#' @param as_data_frame Logical, if \code{FALSE} returns an object of class \code{transaction_plan}
#'    instead of a data frame.
#' @docType methods
#' @rdname get_set_as-methods
#' @export
setGeneric(
    "get_depreciation_plan",
    function(
        obj,
        as_data_frame=TRUE
    ) standardGeneric("get_depreciation_plan")
)

#' @rdname get_set_as-methods
#' @export
#' @docType methods
#' @aliases
#'    get_depreciation_plan,-methods
#'    get_depreciation_plan,operations-method
setMethod("get_depreciation_plan",
    signature=signature(obj="operations"),
    function (
        obj,
        as_data_frame=TRUE
    ){
        if(isTRUE(as_data_frame)){
            return(slot(obj, "depreciation"))
        } else {
            result <- transaction_plan(
                plan_type="depreciation",
                plan=slot(obj, "depreciation")
            )
            return(result)
        }
    }
)


################
## get_plan_type

#' @docType methods
#' @rdname get_set_as-methods
#' @export
setGeneric(
    "get_plan_type",
    function(
        obj
    ) standardGeneric("get_plan_type")
)

#' @rdname get_set_as-methods
#' @export
#' @docType methods
#' @aliases
#'    get_plan_type,-methods
#'    get_plan_type,transaction_plan-method
setMethod("get_plan_type",
    signature=signature(obj="transaction_plan"),
    function (
        obj
    ){
        return(slot(obj, "plan_type"))
    }
)


###########
## get_misc

#' @docType methods
#' @param name Character or integer, specifying which element to get/set. If missing, the whole list is returned/replaced.
#' @rdname get_set_as-methods
#' @export
setGeneric(
    "get_misc",
    function(
        obj,
        name
    ) standardGeneric("get_misc")
)

#' @rdname get_set_as-methods
#' @export
#' @docType methods
#' @aliases
#'    get_misc,-methods
#'    get_misc,operations-method
setMethod("get_misc",
    signature=signature(obj="operations"),
    function (
        obj,
        name
    ){
        if(missing(name)){
            return(slot(obj, "misc"))
        } else {
            return(slot(obj, "misc")[[name]])
        }
    }
)


#############
## set_misc<-

#' @param value A value to assign to the object.
#' @docType methods
#' @rdname get_set_as-methods
#' @export
setGeneric(
    "set_misc<-",
    function(
        obj,
        name,
        value
    ) standardGeneric("set_misc<-")
)

#' @rdname get_set_as-methods
#' @export
#' @docType methods
#' @aliases
#'    set_misc<-,-methods
#'    set_misc<-,operations-method
setMethod("set_misc<-",
    signature=signature(obj="operations"),
    function (
        obj,
        name,
        value
    ){
        if(missing(name)){
            slot(obj, "misc") <- value
        } else {
            misc <- get_misc(obj)
            misc[[name]] <- value
            slot(obj, "misc") <- misc
        }
        return(obj)
    }
)


#############
## list_plans

#' @docType methods
#' @rdname get_set_as-methods
#' @export
setGeneric(
    "list_plans",
    function(
        obj
    ) standardGeneric("list_plans")
)

#' @rdname get_set_as-methods
#' @export
#' @docType methods
#' @aliases
#'    list_plans,-methods
#'    list_plans,transaction_plan-method
setMethod("list_plans",
    signature=signature(obj="transaction_plan"),
    function (
        obj
    ){
        all_plans <- slot(obj, "plan")
        return(unique(all_plans[, c("type", "category", "name")]))
    }
)


##########
## get_sum

#' @docType methods
#' @rdname get_set_as-methods
#' @export
setGeneric("get_sum", function(obj) standardGeneric("get_sum"))

#' @rdname get_set_as-methods
#' @export
#' @docType methods
#' @aliases
#'    get_sum,-methods
#'    get_sum,revenue-method
setMethod("get_sum",
    signature=signature(obj="revenue"),
    function (obj){
        return(operations_by_type(get_value(obj), global_sum=TRUE, drop_nonyear_cols=TRUE))
    }
)

#' @rdname get_set_as-methods
#' @export
#' @docType methods
#' @aliases
#'    get_sum,-methods
#'    get_sum,expense-method
setMethod("get_sum",
    signature=signature(obj="expense"),
    function (obj){
        return(operations_by_type(get_value(obj), global_sum=TRUE, drop_nonyear_cols=TRUE))
    }
)


#################
## as_transaction

#' @docType methods
#' @rdname get_set_as-methods
#' @export
setGeneric("as_transaction", function(
    obj,
    to,
    aspect,
    valid_types="default",
    type
) standardGeneric("as_transaction"))

#' @rdname get_set_as-methods
#' @param to Character string, the transaction class to coerce into.
#' @param aspect Character string, the row/column of the input objects's \code{value} data frame to use in the resulting object.
#'    All additional data are silently dropped.
#' @param type Character string, a valid type name for the resulting object.
#' @export
#' @docType methods
#' @aliases
#'    as_transaction,-methods
#'    as_transaction,loan-method
setMethod("as_transaction",
    signature=signature(obj="loan"),
    function (
        obj,
        to=c("revenue", "expense"),
        aspect=c("interest", "balance_start", "principal", "total", "cumsum", "balance_remain"),
        valid_types="default",
        type
    ){
        to <- match.arg(to)
        aspect <- match.arg(aspect)
        value_df <- get_value(obj, drop_nonyear_cols=TRUE)
        # to properly use balance_start, e.g. as revenue, we don't want the
        # value repeated and thus cumulated, but just take the first amount
        # and watch out for growth
        if(identical(aspect, "balance_start")){
            balance_start <- growth(as.numeric(value_df[aspect, ]))
            balance_start[1] <- value_df[aspect, 1]
            balance_start[balance_start < 0] <- 0
            value_df[aspect, ] <- balance_start
        } else {}
        value_list <- as.list(value_df[aspect, ])
        if(missing(type)){
            type <- slot(obj, "type")
        } else {}
        if(identical(to, "revenue")){
            result <- revenue(
                type=type,
                category=slot(obj, "category"),
                name=slot(obj, "name"),
                valid_types=valid_types,
                .list=value_list
            )
        } else {
            result <- expense(
                type=type,
                category=slot(obj, "category"),
                name=slot(obj, "name"),
                valid_types=valid_types,
                .list=value_list
            )
        }
        return(result)
    }
)

#' @rdname get_set_as-methods
#' @export
#' @docType methods
#' @aliases
#'    as_transaction,-methods
#'    as_transaction,depreciation-method
setMethod("as_transaction",
    signature=signature(obj="depreciation"),
    function (
        obj,
        to=c("revenue", "expense"),
        aspect=c("investment", "depreciation", "value"),
        valid_types="default",
        type
    ){
        to <- match.arg(to)
        aspect <- match.arg(aspect)
        value_df <- get_value(obj, drop_nonyear_cols=TRUE)
        value_list <- as.list(value_df[aspect, ])
        if(missing(type)){
            type <- slot(obj, "type")
        } else {}
        if(identical(to, "revenue")){
            result <- revenue(
                type=type,
                category=slot(obj, "category"),
                name=slot(obj, "name"),
                valid_types=valid_types,
                .list=value_list
            )
        } else {
            result <- expense(
                type=type,
                category=slot(obj, "category"),
                name=slot(obj, "name"),
                valid_types=valid_types,
                .list=value_list
            )
        }
        return(result)
    }
)


###################
## helper functions

get_formatted_df <- function(
    obj,
    slot,
    drop_nonyear_cols,
    resolution=c("month", "quarter", "year"),
    only_type,
    not_type,
    transaction_type=c("depreciation", "loan")
){
    resolution <- match.arg(resolution)
    df <- slot(obj, slot)

    if(!missing(only_type)){
        df <- df[df[["type"]] %in% only_type,]
    } else if(!missing(not_type)){
        df <- df[!df[["type"]] %in% not_type,]
    } else {}

    if(any(is(obj, "loan"), is(obj, "depreciation"))){
        df <- as.data.frame(t(df))
        if(!isTRUE(drop_nonyear_cols)){
            df <- cbind(
                data.frame(
                    type=slot(obj, "type"),
                    category=slot(obj, "category"),
                    name=slot(obj, "name"),
                    part=row.names(df),
                    stringsAsFactors=FALSE
                ),
                df
            )
        } else {}
        df_orig <- df
    } else if(is(obj, "transaction_plan")){
        df_orig <- df
    } else {}
    df <- switch(
        resolution,
        year=df_yearly(df, quarterly=FALSE, drop_nonyear_cols=drop_nonyear_cols),
        quarter=df_yearly(df, quarterly=TRUE, drop_nonyear_cols=drop_nonyear_cols),
        if(isTRUE(drop_nonyear_cols)){
            df[, df_date_cols(x=df, scheme="month")]
        } else {
            df
        }
    )

    if(!identical(resolution, "month")){
        if(is(obj, "loan")){
            df <- fix_loan_resolution(df=df, df_months=df_orig, resolution=resolution)
        } else if(is(obj, "depreciation")){
            df <- fix_depreciation_resolution(df=df, df_months=df_orig, resolution=resolution)
        } else if(is(obj, "transaction_plan")){
            transaction_type <- match.arg(transaction_type)
            ## TODO: can we do this more efficiently without a for loop?
            if(identical(transaction_type, "depreciation")){
                val_rows <- which(df_orig[["part"]] %in% "value")
                for (this_row in val_rows){
                    df[this_row, df_date_cols(x=df, scheme=resolution)] <- fix_depreciation_resolution(
                        df[this_row, df_date_cols(x=df, scheme=resolution), drop=FALSE],
                        df_months=df_orig[this_row, df_date_cols(x=df_orig, scheme="month"), drop=FALSE],
                        resolution=resolution
                    )
                }
            } else if(identical(transaction_type, "loan")){
                val_rows <- which(df_orig[["part"]] %in% c("balance_start", "cumsum", "balance_remain"))
                # this is a bit more complicated as we must do this in groups of three rows each
                n_items <- length(val_rows) / 3
                for (this_item_group in 1:n_items){
                    n_rows <- val_rows[((this_item_group * 3) - 2):(this_item_group * 3)]
                    df[n_rows, df_date_cols(x=df, scheme=resolution)] <- fix_loan_resolution(
                        df[n_rows, df_date_cols(x=df, scheme=resolution)],
                        df_months=df_orig[n_rows, df_date_cols(x=df_orig, scheme="month"), drop=FALSE],
                        resolution=resolution
                    )
                }
            } else {
                stop(simpleError(paste0("Usupported transaction type: ", transaction_type)))
            }
        } else {}
    } else {}

    return(df)
}


# loan objects have columns "balance_start", "cumsum" and "balance_remain" that must not be cumulated
# when switching between months, quarters and years
fix_loan_resolution <- function(
    df,
    df_months,
    resolution=c("month", "quarter", "year")
){
    resolution <- match.arg(resolution)
    fix_rows <- c("balance_start", "cumsum", "balance_remain")
    # if there's three (unnamed) rows, this is probably from a transaction_plan object
    if(nrow(df) == 3){
        rownames(df) <- rownames(df_months) <- fix_rows
    } else {}
    if(identical(resolution, "quarter")){
        q_mtx <- vapply(
            df_date_cols(df, scheme="quarter"),
            function(q){
                q_months <- paste0(y_from_q(q=q), ".", q_mon(q=q_num(q=q)))
                relevant_qm <- q_months[q_months %in% names(df_months)]
                return(c(
                    balance_start=df_months["balance_start", min(relevant_qm)],
                    cumsum=df_months["cumsum", max(relevant_qm)],
                    balance_remain=df_months["balance_remain", max(relevant_qm)]
                ))
            },
            FUN.VALUE=c(
                balance_start=0,
                cumsum=0,
                balance_remain=0
            )
        )
        df[fix_rows, colnames(q_mtx)] <- q_mtx[fix_rows, ]
    } else if(identical(resolution, "year")){
        y_mtx <- vapply(
            df_years(df_months),
            function(y){
                y_months <- paste0(y, ".", sprintf("%02d", 1:12))
                relevant_ym <- y_months[y_months %in% names(df_months)]
                return(c(
                    balance_start=df_months["balance_start", min(relevant_ym)],
                    cumsum=df_months["cumsum", max(relevant_ym)],
                    balance_remain=df_months["balance_remain", max(relevant_ym)]
                ))
            },
            FUN.VALUE=c(
                balance_start=0,
                cumsum=0,
                balance_remain=0
            )
        )
        df[fix_rows, colnames(y_mtx)] <- y_mtx[fix_rows, ]
    } else {}
    return(df)
}
# depreciation objects have the column "value" that must not be cumulated
# when switching between months, quarters and years
fix_depreciation_resolution <- function(df, df_months, resolution=c("month", "quarter", "year")){
    resolution <- match.arg(resolution)
    # if there's only one (unnamed) row, this is probably from a transaction_plan object
    if(nrow(df) == 1){
        rownames(df) <- rownames(df_months) <- "value"
    } else {}
    if(identical(resolution, "quarter")){
        q_vct <- sapply(
            df_date_cols(df, scheme="quarter"),
            function(q){
                q_months <- paste0(y_from_q(q=q), ".", q_mon(q=q_num(q=q)))
                relevant_qm <- q_months[q_months %in% names(df_months)]
                return(df_months["value", max(relevant_qm)])
            }
        )
        df["value", names(q_vct)] <- q_vct
    } else if(identical(resolution, "year")){
        y_vct <- sapply(
            df_years(df_months),
            function(y){
                y_months <- paste0(y, ".", sprintf("%02d", 1:12))
                relevant_ym <- y_months[y_months %in% names(df_months)]
                return(df_months["value", max(relevant_ym)])
            }
        )
        df["value", names(y_vct)] <- y_vct
    } else {}
    return(df)
}
