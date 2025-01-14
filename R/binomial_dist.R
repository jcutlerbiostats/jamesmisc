#' @importFrom magrittr %>%
NULL

#' Plot the distribution of a specified binomial random variable, with probability of specified event
#'
#' This function takes parameters for a binomial experiment (n trials, probability of success)
#' and a tail cutoff point for the tail of the distribution that represents the event for which
#' you want to know the probability (an event consisting of a range of values for the number of
#' successes), and plots the distribution with the portion corresponding to the event filled in.
#'
#' @param n_trials A non-zero integer representing the number of Bernoulli trials in the experiment.
#' @param prob A numeric value representing the probability of success for each trial. Range: 0 to 1.
#' @param tail_cutoff An integer representing the cutoff point for the number of successes. To the right or left of this cutoff (inclusive) is the desired range or the portion of the distribution that corresponds to the event of interest.
#' @param left_or_right A character indicating whether your event of interest corresponds to the possible range of successes to the right or to the left of your cutoff point. Possible values: c("left", "right").
#'
#' @return An interactive plotly plot with the specified binomial distribution, a portion of the distribution colored in
#' to represent the event of interest, and the probability of the event in the plot title.
#'
#' @examples
#' # If I apply to 8 schools, each with an acceptance rate of 20% ...
#' # ... what is the probability that I get into at least one of them?
#' binomial_dist(8,.20,1,"right")
#'
#' # 55 people received a particular surgery, were followed to see if they regretted it.
#' # Zero regretted it. If you were expecting 5% of them to regret it, then ...
#' # ... under that assumption what was the probability zero would end up regretting it?
#' binomial_dist(55,.05,0,"left")
#'
#' @export
binomial_dist <- function(n_trials,
                          prob,
                          tail_cutoff,
                          left_or_right){
  checkmate::assert_integerish(n_trials,lower = 1)
  checkmate::assert_numeric(prob,lower = 0,upper = 1)
  checkmate::assert_integerish(tail_cutoff,lower = 0,upper = n_trials)
  checkmate::assert_choice(left_or_right,choices = c("left","right"))

  # Which possibilities for the number of successes are included within the event
  if (left_or_right == "left"){
    event <- 0:tail_cutoff
  }
  if (left_or_right == "right"){
    event <- tail_cutoff:n_trials
  }

  # Probabilities for each possible number of successes
  all_probs <- dbinom(
    x    = 0:n_trials,
    size = n_trials,
    prob = prob
  )

  # Tibble of probabilities and corresponding n successes
  df <- tibble::tibble(
    prob        = all_probs,
    n_successes = 0:n_trials
  ) %>%
    dplyr::mutate(event = dplyr::case_when(
      n_successes %in% event ~ "Event",
      TRUE ~ "Not part of event"
    ))

  # Probability of event (for plot title)
  event_prob <- df %>%
    dplyr::filter(event == "Event") %>%
    dplyr::pull(prob) %>%
    sum()

  # Plot
  g <- df %>%
    ggplot2::ggplot(ggplot2::aes(
      n_successes,prob,fill = event,
      text = paste0("Probability: ",scales::percent(prob,accuracy=.1),"\n",
                    "Number of Successes: ",n_successes)
    )) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(
      values = c("Event" = "#ae4544","Not part of event" = "#cccccc")
    ) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n=10)) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = glue::glue(
        "Probability of Event (%): {scales::percent(event_prob,accuracy=.01)}, i.e., {scales::scientific(event_prob)}"
      ),
      x = glue::glue("Possible Number of Successes. Successes expected: {round(n_trials*prob,1)}"),
      y = "Probability",
      fill = "Event Value"
    )

  plotly::ggplotly(g,tooltip = "text")
}
