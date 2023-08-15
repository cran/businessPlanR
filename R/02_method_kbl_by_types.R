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

#' Format table from collection of types of operations objects
#' 
#' This method uses the \code{kableExtra} package for table formatting.
#' 
#' @param obj An object of class \code{\link[businessPlanR:operations-class]{operations}} or \code{\link[businessPlanR:loan-class]{loan}}.
#' @param types A named character vector of types to fetch from \code{obj} and print in the resulting table.
#'    Names must be the type names, their value must be one of \code{"revenue"} or \code{"expense"} so the method knows what
#'    to use in case identical type names are defined for both.
#' @param resolution One of \code{"month"}, \code{"quarter"}, or \code{"year"}.
#' @param currency Character defining a currency symbol.
#' @param digits Integer, round values to number of digits.
#' @param DIY Logical, if \code{TRUE} returns the \code{kable} object prior to any row packing, specs or kable styling,
#'    so you can apply all of those as you wish.
#' @param font_size Passed to \code{\link[kableExtra:kable_styling]{kable_styling}}.
#' @param latex_options Passed to \code{\link[kableExtra:kable_styling]{kable_styling}}.
#' @param stripe_color Passed to \code{\link[kableExtra:kable_styling]{kable_styling}}.
#' @param years Character (or numeric) vector defining the year(s) to be represented in the output. This is intended to be
#'    useful for splitting up quarterly or monthly output.
#' @param sum_names A named character vector with two entries, \code{subtotal} and \code{total},
#'    to be used in the resulting table for those values.
#' @param type_colors A named character vector with two entries, \code{color} and \code{background}, defining the color scheme
#'    for type headlines.
#' @param space Character, a space definition to put between currency and value.
#' @param ... Additional arguments passed on to \code{\link[kableExtra:kbl]{kbl}}.
#' @return An object of class \code{kable}.
#' @docType methods
#' @rdname kbl_by_types-methods
#' @export
setGeneric(
    "kbl_by_types",
    function(
        obj,
        types,
        resolution=c("year", "quarter", "month"),
        currency="\u20ac",
        digits=0,
        DIY=FALSE,
        font_size=NULL,
        latex_options="striped",
        stripe_color="gray!6",
        years=get_period(obj, years=TRUE),
        sum_names=c(
            subtotal="Subtotal",
            total="Total"
        ),
        type_colors=c(
            color="white",
            background="grey"
        ),
        space=c(
            html="&#8239;",
            latex="\\,"
        ),
        ...
    ) standardGeneric("kbl_by_types")
)

#' @rdname kbl_by_types-methods
#' @importFrom kableExtra kable_styling kbl pack_rows row_spec
#' @export
#' @docType methods
#' @aliases
#'    kbl_by_types,-methods
#'    kbl_by_types,operations-method
setMethod("kbl_by_types",
    signature=signature(obj="operations"),
    function(
        obj,
        types,
        resolution=c("year", "quarter", "month"),
        currency="\u20ac",
        digits=0,
        DIY=FALSE,
        font_size=NULL,
        latex_options="striped",
        stripe_color="gray!6",
        years=get_period(obj, years=TRUE),
        sum_names=c(
            subtotal="Subtotal",
            total="Total"
        ),
        type_colors=c(
            color="white",
            background="grey"
        ),
        space=c(
            html="&#8239;",
            latex="\\,"
        ),
        ...
    ){
        resolution <- match.arg(resolution)

        types_df_pre <- list()

        if("revenue" %in% types){
            types_df_pre[["revenue"]] <- get_revenue(obj, resolution=resolution, only_type=names(types)[types %in% "revenue"])
        } else {}
        if("expense" %in% types){
            types_df_pre[["expense"]] <- get_expense(obj, resolution=resolution, only_type=names(types)[types %in% "expense"])
            data_cols <- df_date_cols(types_df_pre[["expense"]], scheme=resolution)
            types_df_pre[["expense"]][, data_cols] <-  types_df_pre[["expense"]][, data_cols] * -1
        } else {
            data_cols <- df_date_cols(types_df_pre[["revenue"]], scheme=resolution)
        }

        types_df <- Reduce(
            function(...){
                merge(
                    ...,
                    all=TRUE,
                    sort=FALSE
                )
            },
            types_df_pre
        )
        all_cols <- colnames(types_df)
        nondata_cols <- all_cols[!all_cols %in% data_cols]

        if(!missing(years)){
            types_df <- types_df[, all_cols[!all_cols %in% data_cols | all_cols %in% df_date_cols(types_df, scheme=resolution, years=years)]]
            data_cols <- as.character(years)
        } else {}
        types_df[["category"]] <- NULL
        nondata_cols <- nondata_cols[!nondata_cols %in% "category"]
        
        if(any(sum_names %in% types_df[["name"]])){
            stop(simpleError("The \"sum_names\" you chose ssem to appear as entry names in the data object, please fix!"))
        } else {}

        types_df_list <- lapply(
            names(types),
            function(this_type){
                t_df <- types_df[types_df[["type"]] %in% this_type, , drop=FALSE]
                t_df[nrow(t_df) + 1, data_cols] <- colSums(t_df[, data_cols])
                t_df[nrow(t_df), nondata_cols] <- c(this_type, rep("", length(nondata_cols) - 2), sum_names[["subtotal"]])
                return(t_df)
            }
        )

        result <- Reduce(
            function(...){
                merge(
                    ...,
                    all=TRUE,
                    sort=FALSE
                )
            },
            types_df_list
        )

        result[nrow(result) + 1, data_cols] <- colSums(result[result[["name"]] %in% sum_names[["subtotal"]], data_cols])
        result[nrow(result), nondata_cols] <- c(sum_names[["total"]], rep("", length(nondata_cols) - 2), sum_names[["total"]])
        result[,data_cols] <- nice_numbers(as.matrix(result[,data_cols]), suffix=currency, digits=digits, space=space)

        result_kbl <- kbl(
            result[, -1],
            col.names=c("", data_cols),
            align="r",
            booktabs=TRUE,
            escape=FALSE,
            ...
        )

        if(!isTRUE(DIY)){
            pos_vector <- sapply(unique(result[["type"]]), function(this_pos){sum(result[["type"]] %in% this_pos)})
            result_kbl <- pack_rows(
                result_kbl,
                index=pos_vector,
                color=type_colors[["color"]],
                background=type_colors[["background"]]
            )
            result_kbl <- kable_styling(
                result_kbl,
                latex_options=latex_options,
                row_label_position="r",
                full_width=FALSE,
                font_size=font_size,
                stripe_color=stripe_color
            )
            result_kbl <- row_spec(result_kbl, row=which(result[["name"]] %in% sum_names), bold=TRUE)
        } else {}

        return(result_kbl)
    }
)
