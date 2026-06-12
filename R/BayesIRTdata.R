#' Generate plotting data for Bayesian IRT analysis
#'
#' Generate plotting data for Bayesian IRT analysis conducted in `blavaan`. `BayesIRTdata()` reads in a fitted IRT model from `blavaan`.
#' The generated datasets may be used for the following types of plots: Item-Person Map (IPM), Item Characteristic Curve (ICC), Item Information Curve (IIC),
#' Test Characteristic Curve (TCC), Test Information Curve (TIC), Reliability Plot (REL).
#'
#' @param blavaanobj A fitted IRT model from `blavaan`.
#'
#' @returns A list of datasets and summary statistics used for plotting.
#' \describe{
#'   \item{draws_by_item_theta}{
#'   Posterior draws of item parameter estimates, probabilities of a correct response, and item information across theta value. Used in ICC and IIC.
#'   }
#'   \item{summary_by_item}{
#'   Summary of item threshold estimates across posterior draws. Used in IPM.
#'   }
#'   \item{summary_by_item_theta}{
#'   Mean probabilities of a correct response and item information across posterior draws, by item and theta. Used in ICC and IIC.
#'   }
#'   \item{summary_by_draw_theta}{
#'   Test expected scores and test information, by posterior draw and theta. Used in TCC and TIC.
#'   }
#'   \item{summary_by_theta}{
#'   Mean test expected scores and test information across posterior draws, by theta. Used in TCC and TIC.
#'   }
#'   \item{summary_by_draw}{
#'   Marginal reliability for each posterior draw. Used in REL.
#'   }
#'   \item{mean_reliability}{
#'   Mean marginal reliability across posterior draws. Used in REL.
#'   }
#' }
#'
#' @examples
#' BayesIRTdata(tma.IRT.fitted.model)
#'
#' # Access individual dataset
#' data <- BayesIRTdata(tma.IRT.fitted.model)
#' data$draws_by_item_theta
#'
#' @export
BayesIRTdata <- function(blavaanobj)

{
  #####################################
  # * Data Pre-processing -----
  #####################################
  # extract the stanfit object
  fitted.model.sim.stanfit <- blavaan::blavInspect(blavaanobj, what = "mcobj")
  # convert draws to mcmc.list
  draws.sim.to.analyze <- MCMCvis::MCMCchains(fitted.model.sim.stanfit, mcmc.list = TRUE)
  # extract number of items
  nitems = length(lavaan::lavNames(blavaanobj, type = "ov"))

  # rename parameters
  a_start <- 1 # starting column number for a parameters
  a_end <- nitems  # ending column number for a parameters
  tau_start <- nitems+1 # starting column number for tau parameters
  tau_end <- nitems*2  # ending column number for tau parameters

  # rename a parameters
  coda::varnames(draws.sim.to.analyze)[a_start:a_end] <- paste0("a_", seq(from = 1, to = nitems))
  # rename tau parameters
  coda::varnames(draws.sim.to.analyze)[tau_start:tau_end] <- paste0("tau_", seq(from = 1, to = nitems))

  # combine chains row-wise (if have multiple chains)
  draws_df_sim <- as.data.frame(do.call(rbind, draws.sim.to.analyze))

  # keep only parameters used in the analysis
  draws_df_sim<-draws_df_sim[, 1:tau_end]
  # create a column for draws, from 1 to total number of draws
  draws_df_sim$draw <- 1:nrow(draws_df_sim)

  #### reformatting data frame to long format (number of items * number of draws = total rows)
  # long format: for a parameters
  draws_long_a <- draws_df_sim |>
    dplyr::select(draw, tidyselect::starts_with("a_")) |>
    tidyr::pivot_longer(
      cols = -draw,
      names_to = "label_a",
      values_to = "a"
    )
  # long format: for tau parameters
  draws_long_tau <- draws_df_sim |>
    dplyr::select(draw, tidyselect::starts_with("tau_")) |>
    tidyr::pivot_longer(
      cols = -draw,
      names_to = "label_tau",
      values_to = "tau"
    )
  # merge
  drawsfinalsim <- cbind(draws_long_a, draws_long_tau)
  # add item number
  drawsfinalsim$itemnum <- seq(from = 1, to = nitems)
  # keep columns that are needed
  drawsfinalsim <- drawsfinalsim[ , c("draw", "a", "tau", "itemnum")]

  ##############################
  # calculate the probability of correct for theta in [-6,6]
  p.from.blavaan.for.theta.values <- function(pars){

    # define the range for theta
    theta = seq(-6, 6, by = .2)

    prob_correct <- NULL
    lambda <- pars$a
    tau <- pars$tau

    for(i in 1:nrow(pars)){
      prob_correct[[i]] <- stats::pnorm(tau[i], mean = lambda[i]*theta, sd = 1, lower.tail = FALSE) # calculate probability
    }
    # convert to dataframe
    prob_correct_df <- data.frame(t(sapply(prob_correct, c)))
    # add columns for draw number and item number
    prob_correct_df <- cbind(prob_correct_df, draw = pars$draw, itemnum = pars$itemnum)
    return (prob_correct_df)
  }

  prob_correct_all <- p.from.blavaan.for.theta.values(drawsfinalsim)

  # convert to long format
  # each row is for one item, one draw, one theta value and the corresponding probability of correct
  draws_theta <- prob_correct_all |>
    dplyr::select(draw, itemnum, tidyselect::starts_with("X")) |>
    tidyr::pivot_longer(
      cols = -c(draw, itemnum),
      names_to = "Theta_label",
      values_to = "p"
    )
  # theta values
  draws_theta$theta <- rep(seq(-6, 6, 0.2), length.out = nrow(draws_theta))
  # drop column
  draws_theta$Theta_label <- NULL

  #################
  # merge with draws of parameter values
  draws_theta_final <- merge(draws_theta, drawsfinalsim, by = c("draw","itemnum"))
  # transformation of a parameter (from probit to logit)
  draws_theta_final$a <- draws_theta_final$a*1.702
  # item information
  draws_theta_final$i <- with(draws_theta_final, a^2 * p * (1 - p))
  # reorder it by draw and itemnum
  draws_theta_final <- draws_theta_final[order(draws_theta_final$draw,
                                               draws_theta_final$itemnum), ]


  #####################
  # summary data for plot

  # summary by item
  # mean, IQR of tau parameter across draws
  sum_by_item <- draws_theta_final |>
    dplyr::distinct(draw, itemnum, tau) |> # remove duplicated rows
    dplyr::group_by(itemnum) |>
    dplyr::summarize(
      mean_tau = mean(tau),
      q1_tau = stats::quantile(tau, probs = 0.25), #Q1
      q2_tau = stats::quantile(tau, probs = 0.5), #Median
      q3_tau = stats::quantile(tau, probs = 0.75), #Q3
      min_tau   = q1 - 1.5 * stats::IQR(tau),
      max_tau   = q3 + 1.5 * stats::IQR(tau),
      .groups = "drop"
    )

  # summary by item and theta
  # mean probability of correct across draws
  # mean item information across draws
  sum_by_item_theta <- draws_theta_final |>
    dplyr::group_by(itemnum, theta) |>
    dplyr::summarise(
      p = mean(p),
      i = mean(i),
      .groups = "drop"
    )

  # summary by theta and draw
  # calculate test information and expected total score
  sum_by_draw_theta <- draws_theta_final |>
    dplyr::group_by(theta, draw) |>
    dplyr::summarise(
      test_i = sum(i),
      test_p = sum(p),
      .groups = "drop"
    )

  # summary by theta
  # mean test information across draws
  sum_by_theta <- sum_by_draw_theta |>
    dplyr::group_by(theta) |>
    dplyr::summarise(
      test_i = mean(test_i),
      test_p = mean(test_p),
      .groups = "drop"
    )

  # summary by draw
  # reliability for each draw
  sum_by_draw <- sum_by_draw_theta |>
    dplyr::mutate(rel_by_theta = test_i/(test_i+1)) |>
    dplyr::group_by(draw) |>
    dplyr::mutate(
      weight = stats::dnorm(theta),
      rel_with_weight = rel_by_theta * weight/sum(weight, na.rm = TRUE)
    ) |>
    dplyr::summarise(
      rel = sum(rel_with_weight, na.rm = TRUE),
      .groups = "drop"
    )

  # reliability across draws
  # for each theta, first calculate mean test information across all draws, then calculate reliability
  # then add up across theta based on weights
  final_rel <- sum_by_theta |>
    dplyr::mutate(
      rel_by_theta = test_i/(test_i+1),
      weight = stats::dnorm(theta),
      rel_with_weight = rel_by_theta * weight/sum(weight, na.rm = TRUE)
    ) |>
    dplyr::summarise(
      rel = sum(rel_with_weight, na.rm = TRUE)
    ) |>
    dplyr::pull(rel)

    return(list(
      draws_by_item_theta = draws_theta_final,
      summary_by_item = sum_by_item,
      summary_by_item_theta = sum_by_item_theta,
      summary_by_draw_theta = sum_by_draw_theta,
      summary_by_theta = sum_by_theta,
      summary_by_draw = sum_by_draw,
      mean_reliability = final_rel
    ))
}
