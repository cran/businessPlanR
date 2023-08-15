context("object classes")

test_that("operations", {
    obj_op_yn <- operations(
        period=2021:2023
    )
    obj_op_yc <- operations(
        period=c("2021", "2023")
    )
    obj_op_m <- operations(
        period=c("2021.07", "2023.06")
    )

    expect_identical(
        obj_op_yn,
        obj_op_yc
    )
    expect_true(
        identical(get_period(obj_op_yn), c("2021.01", "2023.12"))
    )
    expect_true(
        identical(get_period(obj_op_m), c("2021.07", "2023.06"))
    )
    expect_identical(
        get_expense(obj_op_yn),
        get_revenue(obj_op_yc)
    )
    expect_equal(
        ncol(get_expense(obj_op_yn)),
        39
    )
    expect_equal(
        ncol(get_revenue(obj_op_m)),
        27
    )
    expect_true(
        all(c("type", "category", "name", "2021.07", "2023.06") %in% names(get_revenue(obj_op_m)))
    )
    expect_false(
        any(c(paste0("2021.", sprintf("%02d", 1:6)), paste0("2023.", sprintf("%02d", 7:12))) %in% names(get_expense(obj_op_m)))
    )
})

test_that("revenue", {
    rev <- revenue(
        type="Sale",
        category="Merch",
        name="T-Shirts",
        "2019.03"=100,
        "2019.08"=140,
        "2020.03"=340,
        "2020.09"=560,
        "2021.02"=780,
        "2021.07"=1020,
        "2021.10"=1260
    )

    # this should amount to the same values as in 'rev'
    rev_per_use <- revenue(
        type="Sale",
        category="Merch",
        name="T-Shirts",
        per_use=20,
        "2019.03"=5,
        "2019.08"=7,
        "2020.03"=17,
        "2020.09"=28,
        "2021.02"=39,
        "2021.07"=51,
        "2021.10"=63
    )

    # this should have the same sum, but only revenues in october
    rev_due_month <- revenue(
        type="Sale",
        category="Merch",
        name="T-Shirts",
        per_use=20,
        due_month="10",
        "2019.03"=5,
        "2019.08"=7,
        "2020.03"=17,
        "2020.09"=28,
        "2021.02"=39,
        "2021.07"=51,
        "2021.10"=63
    )

    rev_interpol <- revenue(
        type="Sale",
        category="Merch",
        name="T-Shirts",
        missing="interpol",
        "2019.03"=100,
        "2019.08"=140,
        "2020.03"=340,
        "2020.09"=560,
        "2021.02"=780,
        "2021.07"=1020,
        "2021.10"=1260
    )

    rev_null <- revenue(
        type="Sale",
        category="Merch",
        name="T-Shirts",
        missing="0",
        "2019.03"=100,
        "2019.08"=140,
        "2020.03"=340,
        "2020.09"=560,
        "2021.02"=780,
        "2021.07"=1020,
        "2021.10"=1260
    )

    expect_identical(
        rev,
        rev_per_use
    )
    expect_identical(
        names(get_value(rev)),
        c(
            "type",    "category",   "name", "2019.03", "2019.04", "2019.05",
            "2019.06", "2019.07", "2019.08", "2019.09", "2019.10", "2019.11",
            "2019.12", "2020.01", "2020.02", "2020.03", "2020.04", "2020.05",
            "2020.06", "2020.07", "2020.08", "2020.09", "2020.10", "2020.11",
            "2020.12", "2021.01", "2021.02", "2021.03", "2021.04", "2021.05",
            "2021.06", "2021.07", "2021.08", "2021.09", "2021.10"
        )
    )
    expect_identical(
        sum(get_sum(rev_due_month)),
        sum(get_sum(rev))
    )
    expect_identical(
        sum(get_sum(rev)),
        sum(get_value(rev_due_month)[, c("2019.10", "2020.10", "2021.10")])
    )
    expect_identical(
        sum(get_sum(rev_interpol)),
        16930
    )
    expect_identical(
        sum(get_sum(rev_null)),
        4200
    )
})

test_that("expense", {
    exp <- expense(
        type="Goods",
        category="Merch",
        name="T-Shirts",
        "2019.03"=100,
        "2019.08"=140,
        "2020.03"=340,
        "2020.09"=560,
        "2021.02"=780,
        "2021.07"=1020,
        "2021.10"=1260
    )

    # this should amount to the same values as in 'exp'
    exp_per_use <- expense(
        type="Goods",
        category="Merch",
        name="T-Shirts",
        per_use=20,
        "2019.03"=5,
        "2019.08"=7,
        "2020.03"=17,
        "2020.09"=28,
        "2021.02"=39,
        "2021.07"=51,
        "2021.10"=63
    )

    # this should have the same sum, but only expenses in october
    exp_due_month <- expense(
        type="Goods",
        category="Merch",
        name="T-Shirts",
        per_use=20,
        due_month="10",
        "2019.03"=5,
        "2019.08"=7,
        "2020.03"=17,
        "2020.09"=28,
        "2021.02"=39,
        "2021.07"=51,
        "2021.10"=63
    )

    exp_interpol <- expense(
        type="Goods",
        category="Merch",
        name="T-Shirts",
        missing="interpol",
        "2019.03"=100,
        "2019.08"=140,
        "2020.03"=340,
        "2020.09"=560,
        "2021.02"=780,
        "2021.07"=1020,
        "2021.10"=1260
    )

    exp_null <- expense(
        type="Goods",
        category="Merch",
        name="T-Shirts",
        missing="0",
        "2019.03"=100,
        "2019.08"=140,
        "2020.03"=340,
        "2020.09"=560,
        "2021.02"=780,
        "2021.07"=1020,
        "2021.10"=1260
    )

    expect_identical(
        exp,
        exp_per_use
    )
    expect_identical(
        names(get_value(exp)),
        c(
            "type",    "category",   "name", "2019.03", "2019.04", "2019.05",
            "2019.06", "2019.07", "2019.08", "2019.09", "2019.10", "2019.11",
            "2019.12", "2020.01", "2020.02", "2020.03", "2020.04", "2020.05",
            "2020.06", "2020.07", "2020.08", "2020.09", "2020.10", "2020.11",
            "2020.12", "2021.01", "2021.02", "2021.03", "2021.04", "2021.05",
            "2021.06", "2021.07", "2021.08", "2021.09", "2021.10"
        )
    )
    expect_identical(
        sum(get_sum(exp_due_month)),
        sum(get_sum(exp))
    )
    expect_identical(
        sum(get_sum(exp)),
        sum(get_value(exp_due_month)[, c("2019.10", "2020.10", "2021.10")])
    )
    expect_identical(
        sum(get_sum(exp_interpol)),
        16930
    )
    expect_identical(
        sum(get_sum(exp_null)),
        4200
    )
})

test_that("update_operations", {
    # the first value is three months after the defined period
    rev <- revenue(
        type="Sale",
        category="Merch",
        name="T-Shirts",
        "2019.06"=100,
        "2019.08"=140,
        "2020.03"=340,
        "2020.09"=560,
        "2021.02"=780,
        "2021.07"=1020,
        "2021.10"=1260
    )

    # the last value goes two months beyond the defined period
    exp <- expense(
        type="Goods",
        category="Merch",
        name="T-Shirts",
        "2019.03"=100,
        "2019.08"=140,
        "2020.03"=340,
        "2020.09"=560,
        "2021.02"=780,
        "2021.07"=1020,
        "2021.10"=1260,
        "2021.12"=2000
    )

    opr <- operations(
        period=c("2019.03", "2021.10")
    )

    expect_warning(
        update_operations(opr, warning=TRUE) <- rev,
        regexp="Adding missing months with 0 values"
    )
    expect_warning(
        update_operations(opr, warning=TRUE) <- exp,
        regexp="Dropping months"
    )
})
