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

#' Plot business plan transactions
#'
#' @param height An object of class \code{\link[businessPlanR:operations-class]{operations}}, \code{\link[businessPlanR:revenue-class]{revenue}} or \code{\link[businessPlanR:expense-class]{expense}}.
#' @param resolution One of \code{"month"}, \code{"quarter"}, or \code{"year"}.
#' @param scope One of \code{"revenue"}, \code{"expense"}, \code{"rev_exp"}, \code{"profit"}.
#' @param types Character string naming the model types defined by
#'    \code{\link[businessPlanR:set_types]{set_types}} to be used.
#' @param ... Any other argument suitable for \code{barplot()}.
#' @return See \code{\link[graphics:barplot]{barplot}}.
#' @export
#' @docType methods
#' @rdname barplot-methods
setGeneric("barplot", function(height, ...) standardGeneric("barplot"))

#' @export
#' @docType methods
#' @rdname barplot-methods
#' @aliases barplot,revenue-method
#' @include 01_class_03_revenue.R
#' @importFrom graphics barplot legend
setMethod("barplot", signature(height="revenue"), function(height, resolution="month", types="default", ...){
    return(invisible(
        barplot_rev_exp(
            obj=prep_barplot_data(
                height,
                resolution=resolution,
                class="revenue",
                types=types
            )
        )
    ))
})

#' @export
#' @docType methods
#' @rdname barplot-methods
#' @aliases barplot,expense-method
#' @include 01_class_04_expense.R
#' @importFrom graphics barplot legend
setMethod("barplot", signature(height="expense"), function(height, resolution="month", types="default", ...){
    return(invisible(
        barplot_rev_exp(
            obj=prep_barplot_data(
                height,
                resolution=resolution,
                class="expense",
                types=types
            )
        )
    ))
})

#' @export
#' @docType methods
#' @rdname barplot-methods
#' @aliases barplot,operations-method
#' @include 01_class_01_operations.R
#' @importFrom graphics barplot legend
setMethod("barplot", signature(height="operations"), function(height, resolution="month", scope="profit", types="default", ...){
    cnd_data <- condensed_data(
        obj=height,
        resolution=resolution,
        scope=scope,
        types=types
    )
    ylim_max <- ylim_max(df=cnd_data[["mtx"]])
    ylim_min <- ylim_min(df=cnd_data[["mtx"]])
    return(invisible(
        barplot_rev_exp(
            obj=list(
                name=cnd_data[["name"]],
                data=cnd_data[["mtx"]],
                color=cnd_data[["color"]],
                ylim_max=ylim_max,
                ylim_min=ylim_min
            ),
            beside=TRUE,
            legend=!identical(scope, "profit")
        )
    ))
})


## internal functions

ylim_max <- function(df){
    return(ceiling(max(df) * 1.25 / 10) * 10)
}

ylim_min <- function(df){
    return(min(0, floor(min(df) * 1.2 / 10) * 10))
}


prep_barplot_data <- function(obj, resolution, class, types="default"){
    data <- df_resolution(df=slot(obj, "value"), resolution=resolution)
    ylim_max <- ylim_max(df=data)
    ylim_min <- ylim_min(df=data)
    type <- slot(obj, "type")
    category <- slot(obj, "category")
    name <- slot(obj, "name")
    color <- get_types(name=types, class=class)[[type]]
    return(list(
        type=type,
        category=category,
        name=name,
        data=data,
        color=color,
        ylim_max=ylim_max,
        ylim_min=ylim_min
    ))
}


barplot_rev_exp <- function(
    obj,
    beside=FALSE,
    legend=TRUE
){
    bp <- barplot(
        obj[["data"]],
        border=FALSE,
        las=2,
        col=obj[["color"]],
        ylim=c(obj[["ylim_min"]], obj[["ylim_max"]]),
        beside=beside
    )
#     abline(
#         v=c(4.9 , 9.7), #TODO
#         col="grey"
#     )
    if(isTRUE(legend)){
        legend(
            "topleft",
            legend=obj[["name"]],
            col=obj[["color"]],
            bty="n",
            pch=20,
            pt.cex=2,
            cex=0.8,
            horiz=FALSE,
            inset=c(0.05, 0.05)
        )
    } else {}
    return(invisible(bp))
}
