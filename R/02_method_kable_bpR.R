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

#' Format table from condensed objects
#' 
#' This method uses the \code{kableExtra} package for table formatting.
#' 
#' @param obj An object of class \code{\link[businessPlanR:operations-class]{operations}} or \code{\link[businessPlanR:loan-class]{loan}}.
#' @param model A named list of named lists describing the stepwise accounting rules
#'    for all data in in \code{obj}.
#' @param resolution One of \code{"month"}, \code{"quarter"}, or \code{"year"}.
#' @param keep_types Logical, whether the returned data frame should keep the intermediate results
#'    for each relevant type of transaction. This will add a column \code{type} to the data frame.
#' @param detailed Logical, supersedes \code{keep_types}. If \code{TRUE}, the table includes detailed
#'    information all the way down to types, categories, and transaction names.
#' @param cashflow Logical, whether the \code{model} describes a cash flow plan. If \code{TRUE},
#'    calculations will start with the initial value as specified by \code{cf_init} and use the
#'    result of each period as the starting value of following periods. This only works if \code{detailed=FALSE}.
#' @param currency Character defining a currency symbol.
#' @param DIY Logical, if \code{TRUE} returns the \code{kable} object prior to any row collapsing, column specs or kable styling,
#'    so you can apply all of those as you wish.
#' @param longtable_clean_cut Passed to \code{\link[kableExtra:collapse_rows]{collapse_rows}}.
#' @param font_size Passed to \code{\link[kableExtra:kable_styling]{kable_styling}}.
#' @param latex_options Passed to \code{\link[kableExtra:kable_styling]{kable_styling}}.
#' @param stripe_color Passed to \code{\link[kableExtra:kable_styling]{kable_styling}}.
#' @param years Character (or numeric) vector defining the year(s) to be represented in the output. This is intended to be
#'    useful for splitting up quarterly or monthly output.
#' @param detail_names A named character vector with two entries, \code{revenue} and \code{expense},
#'    defining the global names used for the two transaction classes in the data frame if \code{detailed=TRUE}.
#' @param detail_colors A named character vector with two entries, \code{color} and \code{background}, defining the color scheme
#'    for position headlines (revenue and expense). Only relevant if \code{detailed=TRUE}.
#' @param cf_init Numeric, used as the initial value for cash flow calculations if \code{cashflow=TRUE}; i.e.,
#'    the first beginning cash value.
#' @param cf_names Character vector with two entries named \code{begin} and \code{end}, used in the resulting
#'    table for beginning cash and ending cash.
#' @param detail_width Optional vector of length 3, if given defined the width of the three categorial columns,
#'    \code{Type}, \code{Category}, and \code{Name}.
#' @param space Character, a space definition to put between currency and value.
#' @param ... Additional arguments passed on to \code{\link[kableExtra:kbl]{kbl}}.
#' @return An object of class \code{kable}.
#' @docType methods
#' @rdname kable_bpR-methods
#' @export
setGeneric(
    "kable_bpR",
    function(
        obj,
        model=get_model(),
        resolution=c("year", "quarter", "month"),
        keep_types=TRUE,
        detailed=FALSE,
        cashflow=FALSE,
        currency="\u20ac",
        DIY=FALSE,
        longtable_clean_cut=TRUE,
        font_size=NULL,
        latex_options="striped",
        stripe_color="gray!6",
        years=get_period(obj, years=TRUE),
        detail_names=c(
            revenue="Revenue",
            expense="Exepense"
        ),
        detail_colors=c(
            color="white",
            background="grey"
        ),
        cf_init=0,
        cf_names=c(
            begin="Begin",
            end="End"
        ),
        space=c(
            html="&#8239;",
            latex="\\,"
        ),
        detail_width,
        ...
    ) standardGeneric("kable_bpR")
)

#' @rdname kable_bpR-methods
#' @importFrom kableExtra collapse_rows column_spec kable_styling kbl pack_rows row_spec
#' @export
#' @docType methods
#' @aliases
#'    kable_bpR,-methods
#'    kable_bpR,operations-method
setMethod("kable_bpR",
    signature=signature(obj="operations"),
    function(
        obj,
        model=get_model(),
        resolution=c("year", "quarter", "month"),
        keep_types=TRUE,
        detailed=FALSE,
        cashflow=FALSE,
        currency="\u20ac",
        DIY=FALSE,
        longtable_clean_cut=TRUE,
        font_size=NULL,
        latex_options="striped",
        stripe_color="gray!6",
        years=get_period(obj, years=TRUE),
        detail_names=c(
            revenue="Revenue",
            expense="Exepense"
        ),
        detail_colors=c(
            color="white",
            background="grey"
        ),
        cf_init=0,
        cf_names=c(
            begin="Begin",
            end="End"
        ),
        space=c(
            html="&#8239;",
            latex="\\,"
        ),
        detail_width,
        ...
    ){
        resolution <- match.arg(resolution)
        if(isTRUE(detailed)){
            cond_obj <- condensed_details(
                obj,
                resolution=resolution,
                detail_names=detail_names,
                years=years
            )
            data_cols <- df_date_cols(cond_obj, scheme=resolution)
            cond_obj[,data_cols] <- nice_numbers(as.matrix(cond_obj[,data_cols]), suffix=currency, space=space)
            if(!isTRUE(DIY)){
                kable_body <- kbl(
                    cond_obj[, -1],
                    col.names=c(" ", " ", " ", data_cols),
                    align=c(paste0(rep("r", length(data_cols) + 3), collapse="")),
                    booktabs=TRUE,
                    escape=FALSE,
                    ...
                )
                kable_body <- column_spec(kable_body, column=1, bold=TRUE)
                kable_body <- column_spec(kable_body, column=2, italic=TRUE)
                if(!missing(detail_width)){
                    if(length(detail_width) != 3){
                        stop(simpleError("\"detail_width\" must be of length 3!"))
                    } else {}
                    for (this_col in seq_along(detail_width)){
                        kable_body <- column_spec(kable_body, column=this_col, width=detail_width[[this_col]])
                    }
                } else {}
                kable_body <- collapse_rows(
                    kable_body,
                    columns=1:3,
                    valign="top",
                    latex_hline="none",
                    longtable_clean_cut=longtable_clean_cut
                )
                pos_vector <- sapply(unique(cond_obj[["Position"]]), function(this_pos){sum(cond_obj[["Position"]] %in% this_pos)})
                kable_body <- pack_rows(kable_body, index=pos_vector, color=detail_colors[["color"]], background=detail_colors[["background"]])
                # moved kable_styling() after collapse_rows() because of buggy LaTeX output with
                # kableExtra
                kable_body <- kable_styling(
                    kable_body,
                    latex_options=latex_options,
                    full_width=FALSE,
                    font_size=font_size,
                    stripe_color=stripe_color
                )
            } else {
                kable_body <- kbl(
                    cond_obj,
                    col.names=c(" ", " ", " ", " ", data_cols),
                    align=c(paste0(rep("r", length(data_cols) + 4), collapse="")),
                    booktabs=TRUE,
                    escape=FALSE,
                    ...
                )
            }
        } else {
            cond_obj <- condense(
                obj=obj,
                model=model,
                resolution=resolution,
                keep_types=keep_types,
                cashflow=cashflow,
                cf_init=cf_init,
                cf_names=cf_names,
                years=years
            )
            data_cols <- df_date_cols(cond_obj, scheme=resolution)
            cond_obj[,data_cols] <- nice_numbers(cond_obj[,data_cols], suffix=currency, space=space)
            if(isTRUE(cashflow)){
                cf_rows <- which(cond_obj[["Type"]] %in% cf_names)
                cond_obj[cf_rows, "Type"] <- ""
            } else {}
            sub_pos_cols <- grepl("*SubPosition", colnames(cond_obj))
            if(all(c("Position", "Type") %in% colnames(cond_obj))){
                sum_rows <- which(cond_obj[["Type"]] %in% "Sum")
                cond_obj[sum_rows, "Type"] <- ""
                if(sum(sub_pos_cols) > 0){
                    for(this_SP_col in colnames(cond_obj)[sub_pos_cols]){
                        for(this_sub in unique(cond_obj[[this_SP_col]])){
                            this_sub_rows <- which(cond_obj[[this_SP_col]] %in% this_sub)
                            if(length(this_sub_rows) > 1){
                                cond_obj[this_sub_rows[-1], this_SP_col] <- ""
                            } else {}
                        }
                    }
                } else {}
                kable_body <- kbl(
                    cond_obj[, !colnames(cond_obj) %in% "Position"],
                    col.names=c(rep(" ", sum(sub_pos_cols) + 1), data_cols),
                    align=c(paste0(rep("r", length(data_cols) + 1), collapse="")),
                    booktabs=TRUE,
                    escape=FALSE,
                    ...
                )
                if(!isTRUE(DIY)){
                    kable_body <- kable_styling(
                        kable_body,
                        latex_options=latex_options,
                        full_width=FALSE,
                        font_size=font_size,
                        stripe_color=stripe_color
                    )
                    kable_body <- column_spec(kable_body, column=1, italic=TRUE)
                    kable_body <- row_spec(kable_body, row=sum_rows, bold=TRUE)
                    for (pos in unique(cond_obj[["Position"]])){
                        all_r <- which(cond_obj[["Position"]] %in% pos)
                        kable_body <- pack_rows(
                            kable_body,
                            group_label=pos,
                            min(all_r),
                            max(all_r),
                            escape=FALSE
                        )
                    }
                } else {}
            } else {
                kable_body <- kbl(
                    cond_obj,
                    col.names=c(rep(" ", sum(sub_pos_cols) + 1), data_cols),
                    align=c(paste0("l", paste0(rep("r", length(data_cols)), collapse=""))),
                    booktabs=TRUE,
                    escape=FALSE,
                    ...
                )
                if(!isTRUE(DIY)){
                    kable_body <- kable_styling(
                        kable_body,
                        latex_options=latex_options,
                        full_width=FALSE,
                        font_size=font_size,
                        stripe_color=stripe_color
                    )
                    kable_body <- column_spec(kable_body, column=1, bold=TRUE)
                    if(sum(sub_pos_cols) > 0){
                        kable_body <- collapse_rows(
                            kable_body,
                            columns=c(1, which(sub_pos_cols)),
                            valign="top",
                            latex_hline="none",
                            longtable_clean_cut=longtable_clean_cut
                        )
                    } else {}
                } else {}
            }
            if(all(isTRUE(cashflow), !isTRUE(DIY))){
                kable_body <- row_spec(kable_body, row=cf_rows, bold=TRUE)
            } else {}
        }

        return(kable_body)
    }
)


#' @rdname kable_bpR-methods
#' @importFrom kableExtra collapse_rows column_spec kable_styling kbl pack_rows row_spec
#' @export
#' @docType methods
#' @aliases
#'    kable_bpR,-methods
#'    kable_bpR,loan-method
setMethod("kable_bpR",
    signature=signature(obj="loan"),
    function(
        obj,
        resolution=c( "month", "quarter", "year"),
        currency="\u20ac",
        DIY=FALSE,
        font_size=NULL,
        latex_options="striped",
        stripe_color="gray!6",
        loan_names=c(
            balance_start="Balance start",
            interest="Interest",
            principal="Principal",
            total="Total",
            cumsum="Cumulated",
            balance_remain="Balance remain"
        ),
        space=c(
            html="&#8239;",
            latex="\\,"
        ),
        ...
    ){
        resolution <- match.arg(resolution)
        loan_df <- as.data.frame(t(get_value(obj, resolution=resolution, drop_nonyear_cols=TRUE)))
        data_cols <- names(loan_df)
        loan_df[,data_cols] <- nice_numbers(loan_df[,data_cols], suffix=currency, digits=2, space=space)

        kable_body <- kbl(
            loan_df,
            col.names=loan_names,
            align=c(paste0(rep("r", ncol(loan_df) + 1), collapse="")),
            booktabs=TRUE,
            escape=FALSE,
            ...
        )

        if(!isTRUE(DIY)){
            kable_body <- kable_styling(
                kable_body,
                latex_options=latex_options,
                full_width=FALSE,
                font_size=font_size,
                stripe_color=stripe_color
            )

            kable_body <- column_spec(kable_body, column=1, color="grey")
            kable_body <- column_spec(kable_body, column=which(names(loan_df) %in% "total") + 1, bold=TRUE)
            kable_body <- column_spec(kable_body, column=which(names(loan_df) %in% c("balance_start", "balance_remain")) + 1, italic=TRUE)

            kable_body <- pack_rows(
                kable_body,
                group_label=slot(obj, "name"),
                start_row=1,
                end_row=nrow(loan_df),
                escape=FALSE
            )
        } else {}

        return(kable_body)
    }
)

#' @param dep_names A named character vector with four entries, \code{investment}, \code{depreciation}, \code{value},
#'    and \code{sum}, used in table to describe the rows of each depreciation item, with \code{sum} only being used
#'    in the final set of rows showing a summary over all items.
#' @param loan_names Like \code{dep_names} but with seven named entries, \code{balance_start}, \code{interest},
#'    \code{principal}, \code{total}, \code{cumsum}, \code{balance_remain}, and \code{sum}, for loan plans, respectively.
#' @param zeroes Named character vector defining the text color to use for zero amounts, for both LaTeX and HTML format.
#' @rdname kable_bpR-methods
#' @importFrom kableExtra cell_spec collapse_rows column_spec kable_styling kbl pack_rows row_spec
#' @importFrom knitr is_latex_output
#' @export
#' @docType methods
#' @aliases
#'    kable_bpR,-methods
#'    kable_bpR,transaction_plan-method
setMethod("kable_bpR",
    signature=signature(obj="transaction_plan"),
    function(
        obj,
        resolution=c("month", "quarter", "year"),
        keep_types=FALSE,
        currency="\u20ac",
        DIY=FALSE,
        longtable_clean_cut=TRUE,
        font_size=NULL,
        latex_options="basic",
        stripe_color="gray!6",
        years=get_period(obj, years=TRUE),
        dep_names=c(
            investment="Investment",
            depreciation="Depreciation",
            value="Value",
            sum="Sum"
        ),
        loan_names=c(
            balance_start="Balance start",
            interest="Interest",
            principal="Principal",
            total="Total",
            cumsum="Cumulated",
            balance_remain="Balance remain",
            sum="Sum"
        ),
        space=c(
            html="&#8239;",
            latex="\\,"
        ),
        zeroes=c(
            html="#C0C0C0",
            latex="gray!25"
        ),
        ...
    ){
        resolution <- match.arg(resolution)
        obj_df <- get_value(obj, resolution=resolution)
        data_cols <- df_date_cols(obj_df, scheme=resolution)
        plan_type <- get_plan_type(obj)

        trns_names <- switch(
            plan_type,
            "depreciation"=dep_names,
            "loan"=loan_names
        )
        trns_names_in_obj <- unique(c(obj_df[["part"]], "sum"))
        required_names <- names(trns_names)
        if(!all(trns_names_in_obj %in% required_names)){
            missing_names <- trns_names_in_obj[!trns_names_in_obj %in% required_names]
            stop(simpleError(
                paste0(
                    "Missing value in \"", ifelse(identical(plan_type, "loan"), "loan_names", "dep_names"), "\":\n  ",
                    paste0(missing_names, collapse=", ")
                )
            ))
        } else {}

        if(!all(get_period(obj, years=TRUE) %in% years)){
            all_cols <- names(obj_df)
            obj_df <- obj_df[, c(all_cols[!all_cols %in% data_cols | all_cols %in% df_date_cols(obj_df, scheme=resolution, years=years)])]
            data_cols <- df_date_cols(obj_df, scheme=resolution)
        } else {}

        parts <- valid_parts(plan_type=plan_type)
        for (this_part in seq_along(parts)){
            if(identical(parts[this_part], "depreciation")){
                obj_df[obj_df[["part"]] %in% parts[this_part], data_cols] <- obj_df[obj_df[["part"]] %in% parts[this_part], data_cols] * -1
            } else {}
            new_row <- nrow(obj_df) + 1
            obj_df[new_row, data_cols] <- colSums(obj_df[obj_df[["part"]] %in% parts[this_part], data_cols, drop=FALSE])
            obj_df[new_row, 1:4] <- c(" ", " ", trns_names["sum"], parts[this_part])
        }
        obj_df[["part"]] <- trns_names[obj_df[["part"]]]

        zero_cells <- obj_df == 0
        obj_df[,data_cols] <- nice_numbers(obj_df[,data_cols], suffix=currency, digits=2, space=space)
        for (this_datacol in data_cols){
            obj_df[zero_cells[, this_datacol], this_datacol] <- cell_spec(
                obj_df[zero_cells[, this_datacol], this_datacol],
                color=ifelse(knitr::is_latex_output(), zeroes[["latex"]], zeroes[["html"]]),
                escape=FALSE
            )
        }

        if(keep_types){
            col_names <- c(" ", " ", " ", " ", data_cols)
            nondate_cols <- 4
        } else {
            obj_df[["type"]] <- NULL
            col_names <- c(" ", " ", " ", data_cols)
            nondate_cols <- 3
        }

        kable_body <- kbl(
            obj_df,
            col.names=col_names,
            align=c(paste0(rep("r", ncol(obj_df) + 1), collapse="")),
            booktabs=TRUE,
            escape=FALSE,
            ...
        )

        if(!isTRUE(DIY)){
            kable_body <- column_spec(kable_body, column=nondate_cols - 1, bold=TRUE)
            kable_body <- column_spec(kable_body, column=nondate_cols, italic=TRUE)

            kable_body <- collapse_rows(
                kable_body,
                columns=1:nondate_cols,
                valign="top",
                latex_hline="custom",
                custom_latex_hline=which(colnames(obj_df) %in% "name"),
                longtable_clean_cut=longtable_clean_cut
            )

            kable_body <- kable_styling(
                kable_body,
                latex_options=latex_options,
                full_width=FALSE,
                font_size=font_size,
                stripe_color=stripe_color
            )
        } else {}

        return(kable_body)
    }
)
