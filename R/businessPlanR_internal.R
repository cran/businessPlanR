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

## function df_date_cols()
# returns the names of operations columns in a date format
# - years: an optional argument specifying years which should be returned exclusively
df_date_cols <- function(x, scheme=c("year", "quarter", "month"), years){
    scheme <- match.arg(scheme)
    if(is.data.frame(x)){
      x <- colnames(x)
    } else {}
    if(missing(years)){
        regex_year <- "[0-9]{4}"
    } else {
        regex_year <- paste0("(", paste0(years, collapse="|"), ")")
    }
    result <- switch(scheme,
        "month"=x[grepl(paste0("^", regex_year, ".[0-9]{2}$"), x)],
        "quarter"=x[grepl(paste0("^", regex_year, ".Q[1-4]$"), x)],
        "year"=x[grepl(paste0("^", regex_year, "$"), x)],
        stop(simpleError("Wrong scheme!"))
    )
    return(result)
} ## end function df_date_cols()


## function df_years()
# calculates the years covered by a data frame
# x must be a character vector or data frame
df_years <- function(x, fill_missing=FALSE){
    if(is.data.frame(x)){
      x <- df_date_cols(x=x, scheme="month")
    } else {}
    years <- unique(gsub("([0-9]{4}).([0-9]{2})", "\\1", x, perl=TRUE))
    if(isTRUE(fill_missing)){
        years <- as.numeric(years)
        years <- as.character(min(years):max(years))
    } else {}
    return(years)
} ## end function df_years()


## function df_period()
# returns the time period covered by a data frame, as a vector of values in YYYY.MM format
# x must be a character vector or data frame
df_period <- function(x, fill_missing=FALSE){
    if(is.data.frame(x)){
      x <- df_date_cols(x=x, scheme="month")
    } else {}
    period <- x
    if(isTRUE(fill_missing)){
        period <- YYYY_MM(c(min(period), max(period)))
    } else {}
    return(period)
} ## end function df_period()


## function check_YYYY_MM()
# checks if 'date' is already in YYYY.MM format. if not, month is given, and value is in YYYY format,
# it is changed into YYYY.MM; if no month is given or value is no year either, an error is thrown 
check_YYYY_MM <- function(date, month){
    date <- as.character(date)
    if(grepl("^[0-9]{4}.[0-1][0-9]$", date)){
        return(date)
    } else if(grepl("^[0-9]{4}$", date)){
        if(!missing(month)){
            month <- as.character(month)
            if(grepl("^[0-1][0-9]$", month)){
                return(paste0(date, ".", month))
            } else {
                stop(simpleError("Months must be in MM format!"))
            }
        } else {
            stop(simpleError("Time period must be in YYYY.MM format!"))
        }
    } else {
        stop(simpleError("Time period must be in YYYY.MM format (or YYYY at least)!"))
    }
} ## end function check_YYYY_MM()


## function YYYY_MM()
# returns a character vector in "YYYY.MM" format from a time period or vector of years
YYYY_MM <- function(period){
    m_first <- check_YYYY_MM(date=period[1], month="01")
    m_last <- check_YYYY_MM(date=period[length(period)], month="12")
    result <- months2YYYY_MM(first=m_first, n=count_months(period=c(m_first, m_last)))
    return(result)
} ## end function YYYY_MM()


## function count_months()
# takes a time period in the form of a vector of two values in "YYYY.MM" format and returns the number
# of months from first to second; e.g. c("2021.01", "2021.02") returns 2
count_months <- function(period){
    if(all(grepl("^[0-9]{4}.[0-1][0-9]$", period), length(period) == 2)){
        first <- as.POSIXlt(gsub("([0-9]{4}).([0-9]{2})", "\\1-\\2-01", period[1], perl=TRUE))
        last <- as.POSIXlt(gsub("([0-9]{4}).([0-9]{2})", "\\1-\\2-01", period[2], perl=TRUE))
        return(12 * (last$year - first$year) + (last$mon - first$mon) + 1)
    } else {
        stop(simpleError("count_months() expects two values in YYYY.MM format!"))
    }
} ## end function count_months()


## function months2YYYY_MM()
# like YYYY_MM(), but lets you define the number of months ('n') beginning from a first one in 'YYYY.MM' format
months2YYYY_MM <- function(first, n){
    first_year <- as.numeric(gsub("([0-9]{4}).([0-9]{2})", "\\1", first, perl=TRUE))
    first_month <- gsub("([0-9]{4}).([0-9]{2})", "\\2", first, perl=TRUE)
    years <- as.numeric(n) %/% 12
    # add one extra year to have enough headroom for selection in the next step
    last_year <- first_year + years + 1
    full_years <- paste0(rep(first_year:last_year, each=12), ".", sprintf("%02d", 1:12))
    first_idx <- which(full_years %in% first)
    return(full_years[first_idx:(first_idx + n - 1)])
} ## end function months2YYYY_MM()


## function q_num()
# returns the quarter as a numeric (1:4) from YYYY.QQ
q_num <- function(q){
    return(as.numeric(gsub("([0-9]{4}).Q([1-4])", "\\2", q, perl=TRUE)))
} ## end function q_num()


## function q_mon()
# returns all months in MM format belonging to a quarter (1:4)
q_mon <- function(q){
    if(all(q > 0, q < 5)){
        return(sprintf("%02d", ((q * 3) - 2) : (q * 3)))
    } else {
        stop(simpleError(paste0("Quarters must be numbered 1 to 4, not \"", q, "\"!")))
    }
} ## end function q_mon()


## function y_from_q()
# returns the year part from YYYY.QQ
y_from_q <- function(q){
    return(gsub("([0-9]{4}).Q([1-4])", "\\1", q, perl=TRUE))
} ## end function y_from_q()


## function quarters_in_period()
# takes a period and returns a vector of relevant quarters
quarters_in_period <- function(period){
    months <- YYYY_MM(period)
    for(q in 1:4){
        q_months <- q_mon(q=q)
        months <- gsub(paste0("([0-9]{4}).(", paste0(q_months, collapse="|"), ")"), paste0("\\1.Q", q), months, perl=TRUE)
    }
    return(unique(months))
} ## end function quarters_in_period()


## function months_in_quarter()
# kind of an reversed quarters_in_period()
# takes a quarter in YYYY.QQ format and returns all months in that quarter
months_in_quarter <- function(q){
    y <- y_from_q(q=q)
    q_n <- q_num(q=q)
    q_months <- q_mon(q=q_n)
    return(paste0(y, ".", q_months))
} ## end function months_in_quarter()


## function df_yearly()
# transforms an operations data frame from monthly to yearly columns
# quarterly: logical, return quarterly instead of yearly results
# drop_nonyear_cols: logical, if TRUE only returns columns with actual transaction data
df_yearly <- function(
    df,
    quarterly=FALSE,
    drop_nonyear_cols=TRUE
){
    relevant_cols <- df_date_cols(x=df, scheme="month")
    if(isTRUE(quarterly)){
        years <- quarters_in_period(c(min(relevant_cols), max(relevant_cols)))
    } else {
        years <- df_years(x=df)
    }
    # sum up the respective yearly values
    years_sum <- as.data.frame(lapply(
        years,
        function(y){
            if(isTRUE(quarterly)){
                q <- q_num(q=y)
                q_months <- q_mon(q=q)
                y <- y_from_q(q=y)
                y_cols <- relevant_cols[grepl(paste0(y, ".(", paste0(q_months, collapse="|"), ")"), relevant_cols)]
            } else {
                y_cols <- relevant_cols[grepl(paste0(y, ".[0-9]{2}"), relevant_cols)]
            }
            return(rowSums(df[, colnames(df) %in% y_cols, drop=FALSE]))
        }
    ))
    colnames(years_sum) <- as.character(years)
    if(isTRUE(drop_nonyear_cols)){
        return(years_sum)
    } else {
        return(cbind(
          df[, !colnames(df) %in% relevant_cols, drop=FALSE],
          years_sum
        ))
    }
} ## end function df_yearly()


## function empty_df()
# period is expected to be a vector of length 2 in "YYYY.MM" format
empty_df <- function(period, first_cols=default_cols){
    if(missing(period)){
        months <- 0
        m_cols <- NULL
    } else {
        months <- count_months(period=period)
        m_cols <- YYYY_MM(period)
    }
    df <- data.frame(
        matrix(
            numeric(),
            nrow=0,
            ncol=months + length(first_cols)
        ),
        stringsAsFactors=FALSE
    )
    colnames(df) <- c(first_cols, m_cols)
    return(df)
} ## end function empty_df()


## function operations_by_type()
# takes a data frame, e.g. from an operations object, and calculates sums by type of transaction
# can also calculate a global sum of all columns
operations_by_type <- function(df, global_sum=FALSE, mode="sum", drop_nonyear_cols=FALSE){
    relevant_cols <- df_date_cols(x=df, scheme="month")
    if(isTRUE(global_sum)){
        # global sum will be calculated by a hack: set all types to mode
        df[["type"]] <- mode
    } else {}
    all_types <- unique(df[["type"]])
    # strip all data, but leave columns
    if(isTRUE(drop_nonyear_cols)){
        result <- df[FALSE,relevant_cols]
    } else {
        result <- df[FALSE,]
    }
    # TODO: use a for loop for now, let's see if we can optimize this later on
    for (this_type in all_types){
        sum_data <- t(colSums(df[df[["type"]] %in% this_type,relevant_cols]))
        colnames(sum_data) <- relevant_cols
        if(isTRUE(drop_nonyear_cols)){
            result <- rbind(
                result,
                sum_data
            )
        } else {
            result <- rbind(
                result,
                cbind(
                    data.frame(type=this_type, name=this_type),
                    sum_data
                )
            )
        }
    }
    return(result)
} ## end function operations_by_type()


## function df_resolution()
# takes a monthly data frame and turns it into a plotable matrix according to
# the given resolution
df_resolution <- function(df, resolution=c("year", "quarter", "month")){
    resolution <- match.arg(resolution)
    if(identical(resolution, "month")){
        # get monthly data
        data <- as.matrix(
            df[,df_date_cols(x=df, scheme="month")]
        )
    } else if(identical(resolution, "quarter")){
        # get quarterly data
        data <- as.matrix(
            df_yearly(df=df, quarterly=TRUE)
        )
    } else{
        # get yearly data
        data <- as.matrix(
            df_yearly(df=df)
        )
    }
    return(data)
} ## end function df_resolution()



## function condensed_data()
# transforms an object of class operations into a matrix according to
# time resolution (monthly, quarterly, yearly) and scope (revenue, expenses, rev_exp, profit)
# returns a list with additional meta data
## TODO: extra colors for legend
condensed_data <- function(obj, resolution, scope=c("revenue", "expense", "rev_exp", "profit"), types="default"){
    scope <- match.arg(scope)
    if(identical(scope, "revenue")){
        # plot revenue by type
        data <- operations_by_type(get_revenue(obj))
        name <- unique(data[["name"]])
        data <- df_resolution(data, resolution=resolution)
        color <- unlist(get_types(name=types, class="revenue"))[name]
    } else if(identical(scope, "expense")){
        # plot expenses by type
        data <- operations_by_type(get_expense(obj))
        name <- unique(data[["name"]])
        data <- df_resolution(data, resolution=resolution)
        color <- unlist(get_types(name=types, class="expense"))[name]
    } else if(identical(scope, "rev_exp")){
        # plot global revenue vs. global expenses
        data <- df_resolution(
            rbind(
                operations_by_type(get_revenue(obj), global_sum=TRUE),
                operations_by_type(get_expense(obj), global_sum=TRUE)
            ),
            resolution=resolution
        )
        name <- c("Revenues", "Expenses")
        color <- c(get_types(name=types, class="revenue")[[1]], get_types(name=types, class="expense")[[1]])
    } else if(identical(scope, "profit")){
        # plot revenue - expenses
        data <- df_resolution(
            operations_by_type(get_revenue(obj), global_sum=TRUE, drop_nonyear_cols=TRUE) - operations_by_type(get_expense(obj), global_sum=TRUE, drop_nonyear_cols=TRUE),
            resolution=resolution
        )
        name <- "Profit"
        color <- sapply(
            data[1,],
            function(x){
                ifelse(
                    x < 0,
                    get_types(name=types, class="expense")[[1]],
                    get_types(name=types, class="revenue")[[1]]
                )
            }
        )
    } else {}
    return(list(
            name=name,
            mtx=data,
            color=color
        )
    )
} ## end function condensed_data()


## function condensed_details()
# this function is similar to condensed_data(), but much simpler, as it
# simply returns a list of two data frames transformed into the desired time resolution,
# one for revenues and expenses each, keeping all other columns
condensed_details <- function(
    obj,
    resolution=c("year", "quarter", "month"),
    detail_names=c(
        revenue="Revenue",
        expense="Exepense"
    ),
    years
){
    resolution <- match.arg(resolution)
    obj_rev <- get_revenue(obj)
    obj_exp <- get_expense(obj)
    result <- rbind(
        cbind(
            data.frame(Position=detail_names[["revenue"]]),
            obj_rev[,!colnames(obj_rev) %in% df_date_cols(obj_rev, scheme="month")],
            df_resolution(obj_rev, resolution=resolution)
        ),
        cbind(
            data.frame(Position=detail_names[["expense"]]),
            obj_exp[,!colnames(obj_exp) %in% df_date_cols(obj_exp, scheme="month")],
            df_resolution(obj_exp, resolution=resolution) * -1
        )
    )
    colnames(result) <- sapply(
        colnames(result),
        function(n){
            paste0(toupper(substring(n, 1,1)), substring(n, 2))
        }
    )
    if(!missing(years)){
        all_cols <- colnames(result)
        result <- result[, all_cols[!all_cols %in% df_date_cols(result, scheme=resolution) | all_cols %in% df_date_cols(result, scheme=resolution, years=years)]]
    } else {}
    return(result)
}
## end function condensed_details()


## function value_list2month_df()
# take an (possibly incomplete) list of values named YYYY.MM and returns a data frame
# of full years with monthly values
# - due_month: character value(s), defining the months of a year where transactions are due
#     if given, the calculated values are aggregated/moved to those months
#     WARNING: if the last month is missing, payments may be dropped!
#' @importFrom stats approx
value_list2month_df <- function(value_list, type, category, name, missing=c("rep", "interpol", "0"), due_month=NA){
    missing <- match.arg(missing)
    if(all(sapply(value_list, is.numeric, USE.NAMES=FALSE))){
        value_list <- value_list[sort(names(value_list))]
        period <- names(value_list)[c(1, length(value_list))]
        new_df <- empty_df(period=period)
        new_df[1, default_cols] <- c(type, category, name)
        # check if all values are given or some must be repeated
        all_months <- YYYY_MM(period=period)
        defined_months <- which(all_months %in% names(value_list))
        if(missing %in% c("rep", "interpol")){
            rep_until <- sapply(seq_along(defined_months), function(n){
                if(n < length(defined_months)){
                  return(defined_months[n+1] - defined_months[n])
                } else {
                  return(1)
                }
            })
            if(identical(missing, "rep")){
                new_df[1,(length(default_cols) + 1):ncol(new_df)] <- rep(value_list, rep_until)
            } else {
                new_df[1,(length(default_cols) + 1):ncol(new_df)] <- unlist(sapply(
                    seq_along(rep_until),
                    function(x){
                        if(x < length(value_list)){
                            val_start <- value_list[[x]]
                            val_end <- value_list[[x + 1]]
                            n <- rep_until[[x]] + 1 
                            return(approx(1:2, c(val_start,val_end), n=n)[["y"]][-n])
                        } else {
                            return(value_list[[x]])
                        }
                    }
                ))
            }
        } else {
            if(!identical(missing,  "0")){
                stop(simpleError("'missing' must be one of \"rep\", \"interpol\", or \"0\"!"))
            } else {}
            new_df[1,which(all_months %in% names(value_list)) + length(default_cols)] <- value_list
            new_df[1,which(!all_months %in% names(value_list)) + length(default_cols)] <- 0
        }
    } else {
        stop(simpleError("All values must be numeric!"))
    }
    if(!all(is.na(due_month))){
        due_month <- coerce_month(month=due_month, call="value_list2month_df", arg="due_month")
        years <- df_years(names(value_list), fill_missing=TRUE)
        all_due_months <- sort(paste0(years, ".", rep(due_month, each=length(years))))
        all_due_months_n <- which(all_months %in% all_due_months)
        due_months_values <- sapply(
            seq_along(all_due_months),
            function(n){
                if(n > 1){
                    return(sum(new_df[1,all_months[(all_due_months_n[n - 1] + 1):all_due_months_n[n]]]))
                } else {
                    return(sum(new_df[1,all_months[1:all_due_months_n[n]]]))
                }
            }
        )
        new_df[1,all_months] <- 0
        new_df[1,all_due_months] <- due_months_values
    }
    return(new_df)
} ## end function


## function insert_row()
# reworked from https://stackoverflow.com/a/11562428
insert_row <- function(df, new, row) {
  if(row > nrow(df)){
      df <- rbind(
          df,
          new
      )
  } else {
      df[seq(row + 1, nrow(df) + 1), ] <- df[seq(row, nrow(df)), ]
      df[row, ] <- new
  }
  rownames(df) <- NULL
  return(df)
} ## end function insert_row()


## function add_sum_rows()
# goes through all position columns of a data frame and adds a sum of all rows
add_sum_rows <- function(df, resolution, col, drop_carry=TRUE){
    all_cols <- colnames(df)
    data_cols <- df_date_cols(df, scheme=resolution)
    non_pos_cols <- c("Context", "Type", data_cols)
    pos_cols <- all_cols[!all_cols %in% non_pos_cols]
    if(missing(col)){
        col <- pos_cols[[1]]
    } else {}
    for (p in unique(df[[col]])){
        p_pos_rows <- which(df[[col]] %in% p)
        carry_row <- df[p_pos_rows, "Context"] %in% "carry"
        if(any(carry_row)){
            carry_from <- df[p_pos_rows[carry_row], "Type"]
            df[p_pos_rows[carry_row], data_cols] <- df[df[[col]] %in% carry_from & df[["Context"]] %in% "sum", data_cols]
        } else {}
        dummy <- df[p_pos_rows[1], ]
        dummy[[col]] <- p
        dummy[, c(pos_cols[!pos_cols %in% col])] <- rep("", sum(!pos_cols %in% col))
        dummy[["Context"]] <- "sum"
        dummy[["Type"]] <- "Sum"
        dummy[,data_cols] <- colSums(df[p_pos_rows, data_cols])
        df <- insert_row(df=df, new=dummy, row=max(p_pos_rows) + 1)
    }
    if(isTRUE(drop_carry)){
        df <- df[!df[["Context"]] %in% "carry", ]
    }
    return(df)
} ## end function add_sum_rows()


## function fill_missing_cols()
fill_missing_cols <- function(df, template){
    tmpl_cols <- colnames(template)
    df_cols <- colnames(df)
    missing_cols <- tmpl_cols[!tmpl_cols %in% df_cols]
    if(length(missing_cols) > 0){
        df <- cbind(
            df,
            matrix("", ncol=length(missing_cols), nrow=nrow(df), dimnames=list(c(), missing_cols)),
            stringsAsFactors=FALSE
        )
        df <- df[, tmpl_cols]
    } else {}
    return(df)
} ## end function fill_missing_cols()


## coerce_month
# makes sure, a value given as "month" is character and in "MM" format, if possible
coerce_month <- function(month, call, arg="month", valid=sprintf("%02d", 1:12)){
    if(!all(is.character(month))){
        month <- sprintf("%02d", month)
    } else {}
    if(!all(month %in% valid)){
        if(missing(call)){
            call <- ""
        } else {
            call <- paste0(" in call \"", call, "\"")
        }
        stop(simpleError(paste0("Argument \"", arg, "\"", call, " is expected to be character and in MM format!")))
    } else {}
    return(month)
} ## end coerce_month


## function monthly_interest()
# calculates monthy interest rates from p.a. rates
monthly_interest <- function(pa){
    return(100 * ((1 + pa / 100)^(1/12) - 1))
} ## end function monthly_interest()


## function annuity()
# calculates yearly or monthly annuity rates
# if 'months' is set instead of 'years', monthly interest rates will be calculated from 'interest_pa'
annuity <- function(
    loan,
    years, # if omitted, months is mandatory
    months,
    interest_pa
){
    if(missing(years)){
        n <- months
        interest <- monthly_interest(interest_pa)
    } else {
        n <- years
        interest <- interest_pa
    }
    ant <- loan * interest * (1 + interest)^n / ((1 + interest)^n - 1)
    return(round(ant, digits=2))
} ## end function annuity()


## function annuity_mtx()
# calculation of repayment schedule for annuity loans.
# calculates the interest and principal payments for each month,
# and thereby also the remaining balance.
# - mtx: the initial matrix
# - rates: repayment rates
# - interest: the interest rate for the loan
# - due_month: numeric value indicating ho many months are already covered in 'mtx'
annuity_mtx <- function(mtx, rates, interest, due_month){
    months <- last <- nrow(mtx)
    init <- 1
    if(is.numeric(due_month)){
        months <- months - due_month + 1
        init <- due_month
    } else {}
    mtx[init:last, "total"] <- rep(rates, months)
    mtx[, "cumsum"] <- cumsum(mtx[, "total"])
    for (m in 1:months){
        m_row <- init + m - 1
        mtx[m_row, "interest"] <- round(mtx[m_row, "balance_start"] * monthly_interest(interest), digits=2)
        if(m < months){
            mtx[m_row, "principal"] <- round(mtx[m_row, "total"] - mtx[m_row, "interest"], digits=2)
            mtx[m_row, "balance_remain"] <- round(mtx[m_row, "balance_start"] - mtx[m_row, "principal"], digits=2)
            mtx[m_row + 1, "balance_start"] <- mtx[m_row, "balance_remain"]
        } else {
            mtx[m_row, "total"] <- mtx[m_row, "balance_start"]
            mtx[m_row, "principal"] <- round(mtx[m_row, "total"] - mtx[m_row, "interest"], digits=2)
            mtx[m_row, "cumsum"] <- mtx[m_row - 1, "cumsum"] + mtx[m_row, "total"]
            mtx[m_row, "balance_remain"] <- 0
        }
    }
    return(mtx)
} ## end function annuity_mtx()


## function loan_redemption()
loan_redemption <- function(
    amount,
    period, # in months
    interest,
    due_month, # first month of actual principal repayment,
    first_month,
    schedule=c("annuity", "amortization", "maturity")
){
    schedule <- match.arg(schedule)

    last <- period

    result <- matrix(
        c(amount, rep(0, (period * 6) - 1)),
        ncol=6,
        byrow=TRUE,
        dimnames=list(
            months2YYYY_MM(first=first_month, n=last),
            c("balance_start", "interest", "principal", "total", "cumsum", "balance_remain")
        )
    )

    if(all(!identical(schedule, "maturity"), is.numeric(due_month))){
        # for the period given only pay interest
        result[1:due_month, "balance_start"] <- result[1:(due_month - 1), "balance_remain"] <- amount
        result[1:due_month, "interest"] <- round(amount * monthly_interest(interest), digits=2)
        result[1:(due_month - 1), "total"] <- result[1:(due_month - 1), "interest"]
        period <- period - due_month + 1
        init <- due_month
    } else {
        init <- 1
    }

    if(identical(schedule, "annuity")){
        rates <- annuity(loan=amount, months=period, interest_pa=interest)
        result <- annuity_mtx(mtx=result, rates=rates, interest=interest, due_month=due_month)
    } else if(identical(schedule, "amortization")) {
        result[init:last, "principal"] <- amount / period
        result[init:last, "balance_remain"] <- amount - cumsum(result[init:last, "principal"])
        # don't try to round these values before calculating the remaining balance, you'll end up
        # with outstanding payment because of rounding errors
        result[init:last, "principal"] <- round(result[init:last, "principal"], digits=2)
        result[init:last, "balance_remain"] <- round(result[init:last, "balance_remain"], digits=2)
        result[(init + 1):last, "balance_start"] <- result[init:(last - 1), "balance_remain"]
        result[init:last, "interest"] <- round(result[init:last, "balance_start"] * monthly_interest(interest), digits=2)
        result[init:last, "total"] <- result[init:last, "principal"] + result[init:last, "interest"]
        result[, "cumsum"] <- cumsum(result[, "total"])
    } else {
        result[, "balance_start"] <- result[init:(last - 1), "balance_remain"] <- result[last, "principal"] <- amount
        result[, "interest"] <- round(amount * monthly_interest(interest), digits=2)
        result[, "total"] <- result[, "interest"]
        result[last, "total"] <- amount + result[last, "interest"]
        result[, "cumsum"] <- cumsum(result[, "total"])
    }

    result <- as.data.frame(result)

    return(result)
} ## end function loan_redemption()


## function obsolescence()
obsolescence <- function(
    amount,
    obsolete,
    invest_month,
    method=c("linear", "writedown", "sumofyears", "doubledecline")
){
    method <- match.arg(method)
    if(!identical(method, "linear")){
        stop(simpleError(paste0("Depreciation method not yet implemented: \"", method, "\"")))
    } else {}

    result <- matrix(
        c(amount, rep(0, (obsolete * 3) - 1)),
        ncol=3,
        byrow=TRUE,
        dimnames=list(
            months2YYYY_MM(first=invest_month, n=obsolete),
            c("investment", "depreciation", "value")
        )
    )    

    if(identical(method, "linear")){
        result[, "depreciation"] <- amount / obsolete
        result[, "value"] <- amount - cumsum(result[, "depreciation"])
        # to prevent ending up with non-zero results, rounding takes place afterwards
        result[, "depreciation"] <- round(result[, "depreciation"], digits=2)
        result[, "value"] <- round(result[, "value"], digits=2)
## TODO:
#     } else if(identical(method, "writedown")){
#     } else if(identical(method, "sumofyears")){
#     } else if(identical(method, "doubledecline")){
    } else {}

    result <- as.data.frame(result)

    return(result)
} ## end function obsolescence()


## function tailor_value_data()
# takes a 'value' slot from a transaction object (expense, revenue) and checks if
# months matches 'period'; if that is FALSE and 'cut_to_period=TRUE', missing
# columns will be added and superfluous columns dropped, otherwise an error rises.
# returns the 'value' if all went well.
tailor_value_data <- function(value, period, cut_to_period=TRUE, warning=FALSE){
    obj_period <- YYYY_MM(period)
    val_period <- df_period(value, fill_missing=TRUE)

    months_missing <- obj_period[!obj_period %in% val_period]
    months_too_many <- val_period[!val_period %in% obj_period]

    if(any(length(months_missing) > 0, length(months_too_many) > 0)){
        if(isTRUE(cut_to_period)){
            if(length(months_missing) > 0){
                if(isTRUE(warning)){
                    warning(paste0("Adding missing months with 0 values: \n  ", paste0(months_missing, collapse=", ")), call.=FALSE)
                } else {}
                non_date_cols <- names(value)[!names(value) %in% val_period]
                value[, months_missing] <- 0
                value <- value[, c(non_date_cols, obj_period)]
            } else {}
            if(length(months_too_many) > 0){
                if(isTRUE(warning)){
                    warning(paste0("Dropping months: \n  ", paste0(months_too_many, collapse=", ")), call.=FALSE)
                } else {}
                value <- value[, !names(value) %in% months_too_many]
            } else {}
        } else {
            stop(simpleError(paste0("Years in value (", paste0(val_period, collapse=", "), ") don't match object (", paste0(obj_period, collapse=", "), ")!")))
        }
    } else {}

    return(value)
} ## end function tailor_value_data()


## function unzero_cols()
# takes a data frame and removes columns full of zeroes at start and end of covered period
unzero_cols <- function(df){
    obj_period <- df_date_cols(df, scheme="month")
    non_data_cols <- sum(!names(df) %in% obj_period)
    zero_cols <- apply(df[obj_period], 2 , function(col){all(col == 0)})
    if(any(zero_cols)){
        rm_cols <- c()
        first_nonzero_column <- min(which(!zero_cols))
        # note then next one is counted in reverse
        last_nonzero_column <- min(which(!rev(zero_cols)))
        if(first_nonzero_column > 1){
            rm_cols <- c(rm_cols, 1:(first_nonzero_column - 1))
        } else {}
        if(last_nonzero_column > 1){
            rm_cols <- c(rm_cols, (length(obj_period) - last_nonzero_column + 2):length(obj_period))
        } else {}
        df[,(rm_cols + non_data_cols)] <- NULL 
    }
    return(df)
} ## end function unzero_cols()


## function valid_parts()
valid_parts <- function(plan_type=c("depreciation", "loan")){
    plan_type <- match.arg(plan_type)
    parts <- switch(
        plan_type,
        "depreciation"=c("investment", "depreciation", "value"),
        "loan"=c("balance_start", "interest", "principal", "total", "cumsum", "balance_remain")
    )
    return(parts)
} ## end function valid_parts()


## function do_as_transaction_call()
# takes an operations class object, an object of class loan or depreciation, and a list of
# arguments for as_transaction().
# performs all as_transaction(value, args) calls and updates obj accordingly.
do_as_transaction_call <- function(
    obj,
    value,
    args_list
){
    stopifnot(is(obj, "operations"))
    stopifnot(any(is(value, "loan"), is(value, "depreciation")))
    if(all(length(args_list) > 0, is.list(args_list))){
        valid_args <- names(formals(as_transaction))
        for(this_args in args_list){
            if(all(names(this_args) %in% valid_args)){
                update_operations(obj) <- do.call(as_transaction, args=append(this_args, values=list(obj=value)))
            } else {
                stop(simpleError(paste0("Invalid arguments in 'as_transaction':\n  \"", paste0(names(this_args)[!names(this_args) %in% valid_args], collapse="\", \""), "\"")))
            }
        }
    } else {
        stop(simpleError("'args_list' must be a list of vectors with arguments for as_transaction()!"))
    }
    return(obj)
} ## end function do_as_transaction_call()
