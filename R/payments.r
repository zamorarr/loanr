#' Compute payment plan
#'
#' @param balance current balance or principal
#' @param rate interest rate
#' @param ppm student payment per month
#' @param years number of years to calculate. Most debt forgiven after 25 years.
#' @param plan which payment plan to use
#' @param subsidized whether the government will help subsidze accrued interest
#' @param ppm_increase how much to increase the monthly payment amount per year. This is done with the hopes that a student will get a good job and be able to pay more over time.
#' @export
compute_payments <- function(balance, rate, ppm, years,
                             plan = c("repaye", "ibr"),
                             subsidized = FALSE,
                             ppm_increase = 0
                             ) {
  orig_balance <- balance
  hidden_balance <- balance
  plan <- match.arg(plan)
  df <- vector("list", years)

  # compute loan payments over time
  for (yr in seq_len(years)) {
    # payment_per_year - interest_accrued
    net_payment <- compute_net_payment(balance, rate, ppm)

    # subsidized interest
    if (plan == "repaye") {
        govt_help <- ifelse(subsidized*(yr <= 3), 1, 0.5)
    } else {
      govt_help <- 0
    }

    hidden_interest <- compute_hidden_interest(net_payment, govt_help)


    # new balances
    hidden_balance <- hidden_balance + ifelse(net_payment >=0, -net_payment, hidden_interest)
    new_balance <- min(balance - net_payment, balance)

    # update list
    df[[yr]] <- tibble::tibble(
      year = yr,
      balance_before = balance,
      interest_rate = rate,
      interest = balance*rate,
      payment = 12*ppm,
      govt_help = govt_help,
      new_balance = new_balance,
      hidden_balance = hidden_balance,
      hidden_interest = hidden_balance - new_balance
    )

    # update balance
    balance <- new_balance
    ppm <- ppm + ppm_increase
  }

  # bind list
  dplyr::bind_rows(df) %>%
    dplyr::filter(!!rlang::sym("hidden_balance") >= 0)
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

#' Compute the hidden interest
#'
#' Sometimes loans will hide the interest if it causes the balance to exceed
#' the original principal. This interest may be capitalized if you change plans
#' or consolidate loans.
#'
#' @param net_payment net payment
#' @param govt_help percentage of subsidzation the government will provide
compute_hidden_interest <- function(net_payment, govt_help) {
  if (net_payment >= 0) return(0)
  -net_payment*(1 - govt_help)
}
