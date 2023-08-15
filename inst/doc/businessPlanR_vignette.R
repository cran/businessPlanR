## ---- include=FALSE, cache=FALSE----------------------------------------------
library(businessPlanR)

## ---- set-options, echo=FALSE, cache=FALSE-----------------------------------------
old_options <- options(width=85)

## ----------------------------------------------------------------------------------
set_types(
    types=list(
        "Obst"=rgb(0.1,0.9,0.2,0.8),
        "Gemüse"=rgb(0.2,0.9,0.4,0.8),
        "Merchandise"=rgb(0.3,0.8,0.3,0.8),
        "Kreditaufnahme"=rgb(0.3,0.7,0.2,0.8)
    ),
    class="revenue",
    name="Saftladen"
)

## ----------------------------------------------------------------------------------
options("businessPlanR")

## ----------------------------------------------------------------------------------
set_types(
    types=list(
        "Löhne und Gehälter"=rgb(0.7,0.2,0.4,0.8),
        "Sozialabgaben"=rgb(0.7,0.2,0.4,0.8),
        "Waren"=rgb(0.7,0.3,0.5,0.8),
        "Infrastruktur"=rgb(0.75,0.2,0.4,0.8),
        "Abschreibung"=rgb(0.9,0.2,0.4,0.8),
        "Kredittilgung"=rgb(0.9,0.2,0.4,0.8),
        "Zinsen"=rgb(0.9,0.2,0.4,0.8),
        "Steuern"=rgb(0.9,0.2,0.4,0.8)
    ),
    class="expense",
    name="Saftladen"
)

## ----------------------------------------------------------------------------------
saftladen_2024_2026 <- operations(
    period=c("2024.01", "2026.12")
)

## ----------------------------------------------------------------------------------
# By default, revenues are repeated every month until
# the value changes.
rev_merch_2024_2026 <- revenue(
    type="Merchandise",
    category="Merch",
    name="T-Shirts",
    valid_types="Saftladen",
    "2024.03"=30,
    "2024.08"=40,
    "2025.03"=50,
    "2025.09"=70,
    "2026.02"=90,
    "2026.07"=100,
    "2026.10"=110
)

# We plan to produce our T-shirts in January each year.
# Since these expenses should not be repeated every month,
# we can set missing="0"; alternatives are "rep" for repeat
# (default) or "interpol" for automatic interpolation.
exp_merch_2024_2026 <- expense(
    type="Waren",
    category="Merch",
    name="T-Shirts",
    missing="0",
    valid_types="Saftladen",
    "2024.01"=200,
    "2025.01"=400,
    "2026.01"=600
)

## ----------------------------------------------------------------------------------
get_value(rev_merch_2024_2026)
get_value(exp_merch_2024_2026)

## ----------------------------------------------------------------------------------
# By default, the sales are repeated each month until
# the value changes.
rev_merch_2024_2026 <- revenue(
    type="Merchandise",
    category="Merch",
    name="T-Shirts",
    per_use=10,
    valid_types="Saftladen",
    "2024.03"=3,
    "2024.08"=4,
    "2025.03"=5,
    "2025.09"=7,
    "2026.02"=9,
    "2026.07"=10,
    "2026.10"=11
)
get_value(rev_merch_2024_2026)

## ----------------------------------------------------------------------------------
get_value(rev_merch_2024_2026, resolution="quarter")
get_value(exp_merch_2024_2026, resolution="year")

## ----------------------------------------------------------------------------------
update_operations(saftladen_2024_2026) <- rev_merch_2024_2026
update_operations(saftladen_2024_2026) <- exp_merch_2024_2026

## ----------------------------------------------------------------------------------
saftladen_inc_stamt <- table_model(
    # Gross income from various sources
    "Bruttoeinnahmen"=model_node(
        revenue=c(
            "Obst",
            "Gemüse",
            "Merchandise"
        )
    ),
    # Gross revenue, i.e. minus costs of goods
    "Bruttoertrag"=model_node(
        carry="Bruttoeinnahmen",
        expense=c(
            "Waren"
        )
    ),
    # Gross profit, i.e. minus operating and depreciation expenses
    "Betriebsergebnis"=model_node(
        carry="Bruttoertrag",
        expense=c(
            "Löhne und Gehälter",
            "Sozialabgaben",
            "Infrastruktur",
            "Abschreibung"
        )
    ),
    # Operating profit, i.e. minus interest expenses
    "Gewinn vor Steuern"=model_node(
        carry="Betriebsergebnis",
        expense=c(
            "Zinsen"
        )
    ),
    # Profit after taxes
    "Nettogewinn"=model_node(
        carry="Gewinn vor Steuern",
        expense=c(
          "Steuern"
        )
    ),
    valid_types="Saftladen"
)

## ----------------------------------------------------------------------------------
condense(
    saftladen_2024_2026,
    model=saftladen_inc_stamt
)

## ----------------------------------------------------------------------------------
condense(
    saftladen_2024_2026,
    model=saftladen_inc_stamt,
    resolution="quarter"
)

## ----------------------------------------------------------------------------------
kable_bpR(
    saftladen_2024_2026,
    model=saftladen_inc_stamt,
    resolution="year"
)

## ----------------------------------------------------------------------------------
# Apples
update_operations(saftladen_2024_2026) <- revenue(
    type="Obst",
    category="Obst",
    name="Äpfel",
    valid_types="Saftladen",
    "2024.01"=2200,
    "2024.08"=3000,
    "2024.09"=3400,
    "2024.12"=2600,
    "2025.01"=2300,
    "2025.08"=3100,
    "2025.09"=3500,
    "2025.12"=2700,
    "2026.01"=2400,
    "2026.08"=3200,
    "2026.09"=3600,
    "2026.12"=2800
)
update_operations(saftladen_2024_2026) <- expense(
    type="Waren",
    category="Obst",
    name="Äpfel",
    valid_types="Saftladen",
    "2024.01"=1650,
    "2024.08"=2250,
    "2024.09"=2550,
    "2024.12"=1950,
    "2025.01"=1725,
    "2025.08"=2325,
    "2025.09"=2625,
    "2025.12"=2025,
    "2026.01"=1800,
    "2026.08"=2400,
    "2026.09"=2700,
    "2026.12"=2100
)
# Grapes
update_operations(saftladen_2024_2026) <- revenue(
    type="Obst",
    category="Obst",
    name="Trauben",
    missing="interpol",
    valid_types="Saftladen",
    "2024.01"=480,
    "2025.01"=590,
    "2026.01"=730,
    "2026.12"=820
)
update_operations(saftladen_2024_2026) <- expense(
    type="Waren",
    category="Obst",
    name="Trauben",
    missing="interpol",
    valid_types="Saftladen",
    "2024.01"=340,
    "2025.01"=410,
    "2026.01"=510,
    "2026.12"=580
)
# Potatoes
update_operations(saftladen_2024_2026) <- revenue(
    type="Gemüse",
    category="Gemüse",
    name="Kartoffeln",
    missing="interpol",
    valid_types="Saftladen",
    "2024.01"=2440,
    "2025.01"=2670,
    "2026.01"=2800,
    "2026.12"=2980
)
update_operations(saftladen_2024_2026) <- expense(
    type="Waren",
    category="Gemüse",
    name="Kartoffeln",
    missing="interpol",
    valid_types="Saftladen",
    "2024.01"=1950,
    "2025.01"=2130,
    "2026.01"=2240,
    "2026.12"=2400
)

## ----------------------------------------------------------------------------------
kable_bpR(
    saftladen_2024_2026,
    model=saftladen_inc_stamt,
    resolution="year"
)

## ----------------------------------------------------------------------------------
kable_bpR(
    saftladen_2024_2026,
    model=saftladen_inc_stamt,
    resolution="year",
    detailed=TRUE
)

## ----------------------------------------------------------------------------------
dep_cashreg1 <- depreciation(
    type="Abschreibung",
    category="Laden",
    name="Kasse 1",
    amount=450,
    obsolete=72,
    invest_month="2024.01",
    valid_types="Saftladen"
)
dep_cashreg2 <- depreciation(
    type="Abschreibung",
    category="Laden",
    name="Kasse 2",
    amount=450,
    obsolete=72,
    invest_month="2025.07",
    valid_types="Saftladen"
)

## ----------------------------------------------------------------------------------
update_operations(
    saftladen_2024_2026,
    as_transaction=list(
        c(
            to="expense",
            aspect="investment",
            valid_types="Saftladen",
            type="Infrastruktur"
        ),
        c(
            to="expense",
            aspect="depreciation",
            valid_types="Saftladen",
            type="Abschreibung"
        )
    )
) <- dep_cashreg1
update_operations(
    saftladen_2024_2026,
    as_transaction=list(
        c(
            to="expense",
            aspect="investment",
            valid_types="Saftladen",
            type="Infrastruktur"
        ),
        c(
            to="expense",
            aspect="depreciation",
            valid_types="Saftladen",
            type="Abschreibung"
        )
    )
) <- dep_cashreg2

kable_bpR(
    saftladen_2024_2026,
    model=saftladen_inc_stamt,
    resolution="year"
)

## ----------------------------------------------------------------------------------
dep_plan <- transaction_plan(plan_type="depreciation")
update_plan(dep_plan) <- dep_cashreg1
update_plan(dep_plan) <- dep_cashreg2

kable_bpR(
    dep_plan,
    dep_names=c(
        investment="Investition",
        depreciation="Abschreibung",
        value="Wert",
        sum="Summe"
    ),
    resolution="year",
    years=2024:2029
)

## ----------------------------------------------------------------------------------
loan_2024 <- loan(
    type="Kreditaufnahme",
    category="Bank",
    name="Anschubfinanzierung",
    amount=5000,
    period=60,
    interest=0.075,
    first_month="2024.01",
    schedule=c("amortization"),
    valid_types="Saftladen"
)
update_operations(
    saftladen_2024_2026,
    as_transaction=list(
        c(
            to="expense",
            aspect="principal",
            valid_types="Saftladen",
            type="Kredittilgung"
        ),
        c(
            to="expense",
            aspect="interest",
            valid_types="Saftladen",
            type="Zinsen"
        ),
        c(
            to="revenue",
            aspect="balance_start",
            valid_types="Saftladen",
            type="Kreditaufnahme"
        )
    )
) <- loan_2024

kable_bpR(
    saftladen_2024_2026,
    model=saftladen_inc_stamt,
    resolution="year"
)

## ----------------------------------------------------------------------------------
loan_plan <- transaction_plan(plan_type="loan")
update_plan(loan_plan) <- loan_2024

kable_bpR(
    loan_plan,
    loan_names=c(
        balance_start="Restschuld Beginn",
        interest="Zinsen",
        principal="Tilgung",
        total="Zahlung",
        cumsum="Kumuliert",
        balance_remain="Restschuld Ende",
        sum="Summe"
    ),
    resolution="year"
)

## ----------------------------------------------------------------------------------
saftladen_cashflow <- table_model(
    "Einzahlungen"=model_node(
        revenue=c(
            "Obst",
            "Gemüse",
            "Merchandise",
            "Kreditaufnahme"
        )
    ),
    "Auszahlungen"=model_node(
        expense=c(
            "Löhne und Gehälter",
            "Sozialabgaben",
            "Infrastruktur",
            "Waren",
            "Zinsen",
            "Kredittilgung",
            "Steuern"
        )
    )
)

kable_bpR(
    saftladen_2024_2026,
    model=saftladen_cashflow,
    resolution="year",
    cashflow=TRUE
)

## ----------------------------------------------------------------------------------
assumed <- list(
    total_customers=1860, # number of potential customers?
    pct_year_1st=25,      # how many of them will buy at our store?
    pct_year_last=35,     # how will the number progress?
    pct_demand=30,        # how much will they buy from us?
    kg_per_customer=c(    # how many Kg were cosumed in recent years?
        apples=23
    ),
    exp_per_kg=c(         # how much will it cost to buy goods?
        apples=2.3
    ),
    rev_per_kg=c(         # at what price will we sell?
        apples=3.49
    )
)

## ----------------------------------------------------------------------------------
n_customers <- function(data, years){
    customer_pct <- seq(
        from=data[["pct_year_1st"]],
        to=data[["pct_year_last"]],
        length.out=length(years)
    ) / 100
    result <- round(data[["total_customers"]] * customer_pct)
    names(result) <- years
    return(result)
}

n_customers(data=assumed, years=2024:2026)

## ----------------------------------------------------------------------------------
kg_sale <- function(what, data, years){
    avg_amount_total <- data[["kg_per_customer"]][[what]]
    result <- round(
        avg_amount_total * 
        (data[["pct_demand"]] / 100) *
        n_customers(data=data, years=years)
    )
    return(result)
}

kg_sale(what="apples", data=assumed, years=2024:2026)

## ----------------------------------------------------------------------------------
spread_sales <- function(what, distribution, data, years){
    yearly_sales <- kg_sale(what=what, data=data, years=years)
    distrib <- round(as.vector(sapply(
        yearly_sales,
        function(kg){
            kg * distribution
        }
    )))
    names(distrib) <- paste0(rep(years, each=length(distribution)), names(distribution))
    return(distrib)
}

sales_apples_kg <- spread_sales(
    what="apples",
    distribution=c(
        ".01"=0.20,
        ".08"=0.27,
        ".09"=0.30,
        ".12"=0.23
    ),
    data=assumed,
    years=2024:2026
)

# multiply by prices
(sales_apples_expense <- round(sales_apples_kg * assumed[["exp_per_kg"]][["apples"]]))
(sales_apples_revenue <- round(sales_apples_kg * assumed[["rev_per_kg"]][["apples"]]))

## ----------------------------------------------------------------------------------
update_operations(saftladen_2024_2026) <- revenue(
    type="Obst",
    category="Obst",
    name="Äpfel",
    valid_types="Saftladen",
    .list=as.list(sales_apples_revenue)
)
update_operations(saftladen_2024_2026) <- expense(
    type="Waren",
    category="Obst",
    name="Äpfel",
    valid_types="Saftladen",
    .list=as.list(sales_apples_expense)
)

kable_bpR(
    saftladen_2024_2026,
    model=saftladen_inc_stamt,
    resolution="year"
)

## ---- reset-options, echo=FALSE, cache=FALSE----------------------------------
options(old_options)

