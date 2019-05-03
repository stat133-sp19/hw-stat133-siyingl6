# 1.1) Private Checker Functions
# check_prob tests if an input 'prob' is a valid probability value
check_prob <- function(prob){
  if(length(prob) == 1){
    if(0 <= prob & prob <= 1){
      return(TRUE)
    }else{
      stop("p has to be a number between 0 and 1")
    }
  }else{
    stop("invalid prob value")
  }
}

# check_trials tests if an input 'trials' is a valid value for number of trials
check_trials <- function(trials){
  if(length(trials) == 1){
    if(trials %% 1 == 0 & trials >= 0){
      return(TRUE)
    }else{
      stop("invalid trials value")
    }
  }else{
    stop("invalid trials value")
  }
}

# check_success tests if an input success is a valid value for number of successes
check_success <- function(success, trials){
  if(all(success %% 1 == 0) & max(success) <= trials & min(success) >= 0){
    return(TRUE)
  }else if(max(success) > trials){
    stop("success cannot be greater than trials")
  }else{
    stop("invalid success value")
  }
}


# 1.2) Private Auxiliary Functions
# calculate the expected value of a binomial distribution
aux_mean <- function(trials, prob){
  mean <- trials * prob
  return(mean)
}

# calculate the variance of a binomial distribution
aux_variance <- function(trials, prob){
  variance <- trials * prob * (1 - prob)
  return(variance)
}

# calculate the mode of a binomial distribution
aux_mode <- function(trials, prob){
  m <- trials * prob + prob
  mode <- length(integer(m))
  if(m %% 1 == 0){
    mode <- c(mode, mode -1)
  }else{
    mode <- mode
  }
  return(mode)
}

# calculate the skewness of a binomial distribution
aux_skewness <- function(trials, prob){
  variance <-  trials * prob * (1 - prob)
  skewness = (1 - 2 * prob)/ sqrt(variance)
  return(skewness)
}

# calculate the kurtosis of a binomial distribution
aux_kurtosis <- function(trials, prob){
  variance <-  trials * prob * (1 - prob)
  kurtosis <- (1 - 6 * prob * (1 - prob)) / variance
  return (kurtosis)
}


# 1.3) Function bin_choose()
#' @title bin_choose function
#' @description calculate the number of combinations of a binomial distribution
#' @param n number of trials
#' @param k number of successes
#' @return number of combinations in which k successes can occur in n trials
#' @export
#' @examples
#' bin_choose(n = 5, k = 2)
#' bin_choose(5, 0)
#' bin_choose(5, 1:3)
#'
bin_choose <- function(n, k){
  if (max(k) > n){
    stop("k cannot be greater than n")
  }
  num_combination <- factorial(n) / (factorial(k) * factorial(n - k))
  return(num_combination)
}


# 1.4) Function bin_probability()
#' @title bin_probability function
#' @description calculate the probability of a binomial distribution
#' @param success number of successes
#' @param trials number of trials
#' @param prob probability of success on each trial
#' @return probability of getting k success in n trials
#' @export
#' @examples
#' bin_probability(success = 2, trials = 5, prob = 0.5)
#' bin_probability(success = 0:2, trials = 5, prob = 0.5)
#' bin_probability(success = 55, trials = 100, prob = 0.45)
#'

bin_probability <- function(success, trials, prob){
  check_trials(trials)
  check_prob(prob)
  check_success(success, trials)
  num_combination <- bin_choose(trials, success)
  probability <- num_combination * prob^success * (1 - prob)^(trials - success)
  return(probability)
}


# 1.5) Function bin_distribution()
#' @title bin_distribution function
#' @description generate a binomial probability distribution
#' @param trials number of trials
#' @param prob probability of success on each trial
#' @return data frame of a binomial probability distribution
#' @export
#' @examples
#' bin_distribution(trials = 5, prob = 0.5)
#'
bin_distribution <- function(trials, prob){
  success <- seq(0, trials, by = 1)
  probability <- c(0, rep = trials + 1)
  for (i in success){
    probability[i+1] <- bin_probability(i, trials, prob)
  }
  distribution <- data.frame(success = success, probability = probability)
  class(distribution) <- c("bindis", "data.frame")
  return(distribution)
}

# plot.bindis()
#' @title plot.bindis()
#' @description graph the cumulative distribution in an object "bindis"
#' @export
#' @examples
#' dis1 <- bin_distribution(trials = 5, prob = 0.5)
#' plot(dis1)
#'
plot.bindis <- function(x, ...){
  barplot(x$probability, names.arg = x$success, xlab = "successes", ylab = "probability")
}


# 1.6) Function bin_cumulative()
#' @title bin_cumulative function
#' @description generate a binomial cumulative distribution
#' @param trials number of trials
#' @param prob probability of success on each trial
#' @return data frame of a binomial cumulative distribution
#' @export
#' @examples
#' bin_cumulative(trials = 5, prob = 0.5)
#'
bin_cumulative <- function(trials, prob){
  cumulative <-bin_distribution(trials, prob)
  for (i in cumulative$success){
    if (i == 0){
      cumulative$cumulative[i+1] <- cumulative$probability[1]
    }else{
      cumulative$cumulative[i+1] <- cumulative$cumulative[i] + cumulative$probability[i+1]
    }
  }
  class(cumulative) <- c("bincum", "data.frame")
  return(cumulative)
}

# plot.bincum()
#' @title plot.bincum()
#' @description generate a scatter plot of the cumulative distribution
#' @export
#' @examples
#' dis2 <- bin_cumulative(trials = 5, prob = 0.5)
#' plot(dis2)
#'
plot.bincum <- function(x, ...){
  plot(x$success, x$cumulative, xlab = "successes", ylab = "probability", type = "b")
}


# 1.7) Function bin_variable()
#' @title bin_variable function
#' @description generate binomial random variables
#' @param trials number of trials
#' @param prob probability of success on each trial
#' @return an object of class "binvar", a binomial random variable object
#' @export
#' @examples
#' bin_variable(trials = 10, p = 0.3)
#'
bin_variable <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  lst <- list(trials, prob)
  names(lst) <- c("trials", "prob")
  class(lst) <- "binvar"
  return(lst)
}

# Method print.binvar()
#' @title print.binvar()
#' @description create a method function to be able to nicely print the content of and object
#' @export
#' @examples
#' bin1 <- bin_variable(trials = 10, p = 0.3)
#' bin1
#'
print.binvar <- function(x, ...){
  cat('"Bionomial variable"\n\n')
  cat("Paramters", "\n")
  cat("- number of trials: ", x$trials, "\n")
  cat("- prob of success : ", x$prob)
}

# Method summary.binvar()
#' @title summary.binvar()
#' @description create a method function to return a list of the summary output
#' @export
#' @examples
#' bin1 <- bin_variable(trials = 10, p = 0.3)
#' binsum1 <- summary(bin1)
#'
summary.binvar <- function(x, ...){
  summary <- data.frame(
    trials = x$trials,
    prob = x$prob,
    mean = aux_mean(x$trials, x$prob),
    variance = aux_variance(x$trials, x$prob),
    mode = aux_mode(x$trials, x$prob),
    skewness = aux_skewness(x$trials, x$prob),
    kurtosis = aux_kurtosis(x$trials, x$prob)
  )
  class(summary) <- "summary.binvar"
  return(summary)
}

# Method print.summary.binvar()
#' @title print.summary.binvar()
#' @description create a method function to nicely print the summary measures
#' @export
#' @examples
#' binsum1 <- summary(bin1)
#' binsum1
#'
print.summary.binvar <- function(x, ...){
  cat('"Summary Bionomial"\n\n')
  cat("Paramters", "\n")
  cat("- number of trials: ", x$trials, "\n")
  cat("- prob of success : ", x$prob,"\n\n")
  cat("Measures", "\n")
  cat("- mean    : ", x$mean, "\n")
  cat("- variance: ", x$variance, "\n")
  cat("- mode    : ", x$mode, "\n")
  cat("- skewness: ", x$skewness, "\n")
  cat("- kurtosis: ", x$kurtosis)
}


# 1.8) Functions of measures
#' @title bin_mean
#' @description generate the expected value or mean of a binomial distrbution
#' @param trials number of random trials
#' @param prob probability of successes
#' @return mean of a binomial distribution
#' @export
#' @examples
#' bin_mean(10, 0.3)
#'
bin_mean <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  mean <- aux_mean(trials, prob)
  return(mean)
}

#' @title bin_variance
#' @description generate the variance of a binomial distrbution
#' @param trials number of random trials
#' @param prob probability of successes
#' @return variance of a binomial distribution
#' @export
#' @examples
#' bin_variance(10, 0.3)
#'
bin_variance <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  variance <- aux_variance(trials, prob)
  return(variance)
}

#' @title bin_mode
#' @description generate the mode of a binomial distrbution
#' @param trials number of random trials
#' @param prob probability of successes
#' @return mode of a binomial distribution
#' @export
#' @examples
#' bin_mode(10, 0.3)
#'
bin_mode <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  mode <- aux_mode(trials, prob)
  return(mode)
}


#' @title bin_skewness
#' @description generate the skewness of a binomial distrbution
#' @param trials number of random trials
#' @param prob probability of successes
#' @return skewness of a binomial distribution
#' @export
#' @examples
#' bin_skewness(10, 0.3)
#'
bin_skewness <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  skewness <- aux_skewness(trials, prob)
  return(skewness)
}

#' @title bin_kurtosis
#' @description generate the kurtosis of a binomial distrbution
#' @param trials number of random trials
#' @param prob probability of successes
#' @return kurtosis of a binomial distribution
#' @export
#' @examples
#' bin_kurtosis(10, 0.3)
#'
bin_kurtosis <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  kurtosis <- aux_kurtosis(trials, prob)
  return(kurtosis)
}
