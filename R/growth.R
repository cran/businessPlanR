# Copyright 2021 Meik Michalke <meik.michalke@c3s.cc>
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

#' Growth of a numeric vector
#'
#' Calculates the differences between consecutive values in a numeric vector.
#' 
#' @param x A numeric vector.
#' @param round One of \code{"round"} (invokes \code{\link[base:round]{round}} on \code{x} before calculation),
#'    \code{"ceiling"} (calling \code{\link[base:ceiling]{ceiling}}), or \code{"floor"} (calling
#'    \code{\link[base:floor]{floor}} instead of \code{round}, respectively).
#' @param digits Integer, passed to \code{round} if \code{round="round"}.
#' @param init Numeric, the initial value to compare the first element of \code{x} to.
#' @return A numeric vector the same length as \code{x}.
#' @rdname growth
#' @export
#' @examples
#' growth(c(1,10,12,15,122))
growth <- function(
    x,
    round=c("round", "ceiling", "floor"),
    digits=0,
    init=x[1]
){
    round <- match.arg(round)
    if(!is.numeric(x)){
        stop(simpleError("'x' must be numeric!"))
    } else {}
    x <- switch(
        round,
        "round"=round(x, digits=digits),
        "ceiling"=ceiling(x),
        "floor"=floor(x)
    )
    return(x - c(init, x[1:(length(x) - 1)]))
}
