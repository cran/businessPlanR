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

#' Format numbers in nice layout
#' 
#' Uses \code{\link[base:format]{format}} with some customized defaults.
#' It's being called by \code{\link[businessPlanR:kable_bpR]{kable_bpR}}.
#' 
#' @param x The numeric value to format. Can be a single number, numeric vector, matrix, or data frame.
#' @param prefix An optional symbol to prepend, ignored if missing.
#' @param suffix An optional symbol to append, ignored if missing.
#' @param digits See \code{\link[base:round]{round}}.
#' @param width See \code{\link[base:format]{format}}.
#' @param nsmall See \code{\link[base:format]{format}}.
#' @param space Named character vector, a space definition to put between prefix/suffix and value.
#'    Defaults to a thin space for both, LaTeX and HTML.
#'    If you use provide one character, that one is used regardless of the output environment.
#' @return A formatted character string.
#' @rdname nice_numbers
#' @importFrom knitr is_latex_output
#' @export
#' @examples
#' nice_numbers(12345.6789, suffix="€", digits=2)
nice_numbers <- function(
    x,
    prefix,
    suffix,
    digits=0L,
    width=NULL,
    nsmall=digits,
    space=c(
        html="&#8239;",
        latex="\\,"
    )
){
    n <- format(
        round(x, digits=digits),
        decimal.mark=",",
        big.mark=".",
        small.mark=".",
        small.interval=3,
        scientific=FALSE,
        width=width,
        nsmall=nsmall
    )
    if(length(space) > 1){
        if(knitr::is_latex_output()){
            space <- space[["latex"]]
        } else {
            space <- space[["html"]]
        }
    } else {}
    if(!missing(prefix)){
        if(any(is.data.frame(n), is.matrix(n))){
            n[] <- apply(n, 2, function(x){paste0(prefix, space, x)})
        } else {
            n <- paste0(prefix, space, n)
        }
    } else {}
    if(!missing(suffix)){
        if(any(is.data.frame(n), is.matrix(n))){
            n[] <- apply(n, 2, paste0, space, suffix)
        } else {
            n <- paste0(n, space, suffix)
        }
    } else {}
    return(n)
}
