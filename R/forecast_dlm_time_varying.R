#' Title
#'
#' @param mod The DLM object (or an object cooercible to DLM)
#' @param nAhead The number of steps ahead to forecast
#' @param method Currently does nothing
#' @param sampleNew
#'
#' @return
#' @export
#'
#' @examples
forecast_dlm_time_varying <- function(mod, nAhead = 1, method = c('plain', 'svd'), sampleNew = F)
{
  # This function mostly follows the DLM forecast from the actual package,
  # but expanded to my use case. Note that I will check for the existence
  # of time varying model components and error out if they are false.
  # This is intended behavior to encourage use of the cannoical dlmForecast
  # in cases where it may be used
  require('dlm')
  require('stringr')
  # Invalidate time invariant DLMs
  # QUESTION: Should I include something here for X?
  if (all(is.null(mod$JFF), is.null(mod$JV), is.null(mod$JGG), is.null(mod$JW))) {
    stop('Use dlmForecast for constant models')
  }
  # This currently does nothing
  method <- match.arg(method)

  if (dlm:::is.dlmFiltered(mod)) {
    modFuture <- mod$mod
    # mod m is the filtered state vector values
    lastObsIndex <- NROW(mod$m)
    ##QUESTION: what do U.C and D.C correspond to?
    modFuture$C0 <- with(mod,
                         dlmSvd2var(U.C[[lastObsIndex]],
                                    D.C[[lastObsIndex, ]])
    )
    if (is.ts(mod$m)) {
      modFuture$m0 <- window(mod$m, start = end(mod$m))
    } else {
      modFuture$m0 <- window(mod$m, start = lastObsIndex)
      tsp(modFuture$m0) <- NULL
    }
    # I really dislike this choice
    ##TODO: Clean up names
    mod <- modFuture
  }


  # This will return time periods associated with m0 including
  # frequency

  ytsp <- tsp(mod$m0)
  p <- length(mod$m0)
  # dimension of FF matches JFF so this is fine
  m <- nrow(mod$FF)
  a <- rbind(mod$m0, matrix(0, nAhead, p))
  R <- vector("list", nAhead + 1)
  R[[1]] <- mod$C0
  f <- matrix(0, nAhead, m)
  Q <- vector("list", nAhead)
  ### Predicting Future Values According to Kalman Filter ###
  for (it in 1:nAhead) {
    ## FIXME: Include logic to introduce time variant component at every iteration
    # The way that this is handled in the dlm package is... really obtuse.
    # maybe I can simplify that logic a bit and get it all to slot back in
    # easily
    for (time_varying_matrix in c('JFF', 'JGG', 'JV', 'JW')) {
      time_invariant_matrix = str_remove(time_varying_matrix, 'J')
      # Check to make sure that the object exists
      if (time_varying_matrix %in% names(mod)) {
        # We need to record the value in each position of the matrix and know
        # to look to that column of X for this matrix entry
        nonzero_positions = which(time_varying_matrix != 0, arr.ind = T)
        # Then, loop over these to move the correct values into place
        for (i in 1:nrow(nonzero_positions)) {
          row_to_replace = nonzero_positions[i, 1]
          col_to_replace = nonzero_positions[i, 2]
          x_col_value = time_varying_matrix[row_to_replace, col_to_replace]
          mod[[time_invariant_matrix]][row_to_replace, col_to_replace] = mod$X[it, x_col_value]
        }
      }
    }

    a[it + 1] <- mod$GG %*% a[it, ]
    R[[it + 1, ]] <- mod$GG %*% R[[it]] %*% t(mod$GG) + mod$W
    f[it, ] <- mod$FF %*% a[it + 1, ]
    Q[[it]] <- mod$FF %*% R[[it + 1]] %*% t(mod$FF) + mod$V

  }
  a <- a[-1, drop = F]
  R <- R[-1]
  # The package implementation is prfoundly unreadable.
  # changing order of if checking should help increase
  # readability
  if (!sampleNew) {
    # Nothing left to do, let's get the answer
    if (!is.null(ytsp)) {
      a <- ts(a, start = ytsp[2] + 1/ytsp[3],
              frequency = ytsp[3])
      f <- ts(f, start = ytsp[2] + 1/ytsp[3],
              frequency = ytsp[3])
    }
    return(list(a = a, R = R, f = f, Q = Q))
  } else {
    ### SampleNew Portion ###

    ## TODO: Fix this spaghetti mess of names holy cow
    newStates <- vector("list", sampleNew)
    newObs <- vector("list", sampleNew)
    newS <- matrix(0, nAhead, p)
    newO <- matrix(0, nAhead, m)
    tmp_v <- La.svd(mod$V, nu = 0)
    Ut.V <- tmp_v$vt
    D.V <- sqrt(tmp_v$d)
    tmp_w <- La.svd(mod$W, nu = 0)
    Ut.W <- tmp_w$tmp_v
    D.W <- sqrt(tmp_w$d)
    for (i in 1:sampleNew) {
      tmp <- La.svd(R[[1]], nu = 0)
      newS[1, ] <- a[1,] + crossprod(tmp$vt,
                                     rnorm(p, sd = sqrt(tmp$d)))
      newO[1, ] <- mod$FF %*% newS[1, ] + crossprod(
        Ut.V, rnorm(m, sd = D.V))
      if (nAhead > 1) {
        for (it in 2:nAhead) {
          newS[it, ] <- mod$GG %*% newS[it - 1, ] +
            crossprod(Ut.W, rnorm(p, sd = D.W))
          newO[it, ] <- mod$FF %*% newS[it, ] +
            crossprod(Ut.V, rnorm(m, sd = D.V))
        }
        newStates[[i]] <- newS
        newObs[[i]] <- newO
      }
    }
    if (!is.null(ytsp)) {
      a <- ts(a, start = ytsp[2] + 1 / ytsp[3],
              frequency = ytsp[3])
      f <- ts(f, start = ytsp[2] + 1 / ytsp[3],
              frequency = ytsp[3])
      newStates <- lapply(newStates,
                          function(x) {
                            ts(x, start = ytsp[2] + 1/ytsp[3],
                               frequncy = ytsp[3])
                          })
    }
    return(list(a = a, R = R, f = f,
                Q = Q, newStates = newStates, newObs = newObs))
  }
}
