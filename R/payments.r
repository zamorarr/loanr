#' Compute payment plan
#'
#' @param balance current balance or principal
#' @param income adjusted gross income
#' @param rate interest rate on loan
#' @param years number of years to calculate. Most debt forgiven after 25 years.
#' @param plan which payment plan to use
#' @param subsidized whether the government will help subsidze accrued interest
#' @param income_increase Expected increase in income per year.
#' @export
compute_payments <- function(balance, income, rate, years,
                             plan = c("repaye"),
                             subsidized = FALSE,
                             income_increase = 0.05,
                             family_size = 1
                             ) {
  # inputs
  plan <- match.arg(plan)
  interest <- 0
  repaye_rate <- 0.1

  df <- vector("list", years)

  # compute loan payments over time
  for (yr in seq_len(years)) {
    # calculate payment per month
    ppm <- compute_ppm(income, family_size, repaye_rate)

    # payment_per_year - interest_accrued
    net_payment <- compute_net_payment(balance, rate, ppm)

    # subsidized interest
    if (plan == "repaye") {
      govt_help <- ifelse(subsidized*(yr <= 3), 1, 0.5)
    } else {
      govt_help <- 0
    }
    subsidized_interest <- compute_subsidized_interest(net_payment, govt_help)

    # new balances. pay off the interest first!
    if (net_payment < 0) {
      # interest more than payment
      payment_to_interest <- -subsidized_interest
      payment_to_balance <- 0
    } else {
      # payment more than interest
      payment_to_interest <- min(interest, net_payment)
      payment_to_balance <- net_payment - payment_to_interest
    }

    new_balance <- balance - payment_to_balance
    new_interest <- interest - payment_to_interest

    # update list
    df[[yr]] <- tibble::tibble(
      year = yr,
      principal_before = balance,
      interest_per_month = balance*rate/12,
      payment_per_month = ppm,
      govt_help = govt_help,
      principal_after = new_balance,
      interest_accrued = new_interest
    )

    # update balance
    balance <- new_balance
    interest <- new_interest

    # income increase causes payment increase
    income <- income*(1 + income_increase)
  }

  # bind list
  dplyr::bind_rows(df) %>%
    dplyr::filter(!!rlang::sym("principal_before") >= 0) %>%
    dplyr::mutate(principal_after = pmax(0, !!rlang::sym("principal_after")))
}

#' Compute the net yearly payment
#'
#' Subracts student payment from accrued interest
#' @param balance current balance
#' @param rate interest rate
#' @param ppm payment per month
#'
#' @keywords internal
compute_net_payment <- function(balance, rate, ppm) {
  12*ppm - balance*rate
}

#' Compute the subsidized interest
#'
#' Interest may be subsidized depending on your repayment plan.
#'
#' @param net_payment net payment
#' @param govt_help percentage of subsidzation the government will provide
compute_subsidized_interest <- function(net_payment, govt_help) {
  if (net_payment >= 0) return(0)
  -net_payment*(1 - govt_help)
}

compute_poverty_line <- function(family_size) {
  n <- nrow(poverty)
  if (family_size <= n) {
    poverty[["poverty_line"]][family_size]
  } else {
    poverty[["poverty_line"]][n] + 4320*(family_size - n)
  }
}

#' Payments per month
#'
#' @param income adjusted gross income
#' @param family_size members in family including self, spouse, and dependents
#' @param repaye_rate REPAYE max payment rate is 10% of adjusted income
compute_ppm <- function(income, family_size, repaye_rate = 0.1) {
  poverty_line <- compute_poverty_line(family_size)
  ppm <- (income - 1.5*poverty_line)*repaye_rate/12
  max(ppm, 0)
}

