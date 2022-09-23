zeromean_confirm <- modalDialog(
  "Mean of item parameters is not zero! Are you sure you want to continue?",
  title = "Constraint",
  footer = tagList(
    actionButton("cancel", "Cancel"),
    actionButton("ok", "Continue", class = "btn btn-danger")
  )
)

nobsnitms_confirm <- modalDialog(
  "The input files of item parameters and person parameters are the same! Are you sure you want to continue?",
  title = "Constraint",
  footer = tagList(
    actionButton("canceln", "Cancel"),
    actionButton("okn", "Continue", class = "btn btn-danger")
  )
)

#============= Define item response functions ==================================

#------------- Dichotomous response --------------------------------------------
irffct <- function(theta, b, ii){
  eta <- exp(theta - b[ii])
  pbs <- eta / (1 + eta)
  pbs <- cbind(1 - pbs, pbs)
  return(pbs)
}
#------------- Polytomous response ---------------------------------------------
pcmfct <- function(theta, a = NULL, b, ii){
  N <- length(theta)  # number of persons
  M <- nrow(b)        # max number of categories - 1 for items
  
  beta0 <- 0# - sum(beta[, i]) #
  matb <- matrix(c(beta0, b[, ii]), nrow = N, ncol = M+1, byrow = TRUE)
  matx <- matrix(0:M, nrow = N, ncol = M+1, byrow = TRUE)
  eta <- exp(theta * matx - matb)
  pbs <- eta / rowSums(eta, na.rm=TRUE)
  return(pbs)
}

#============= End define item response functions ==============================


#============= Define fit statistics functions =================================

outfitfct <- function(beta, theta, data, N, K) {
  
  M <- max(data, na.rm = TRUE)            # max number of categories - 1 for items
  mi <- apply(data, 2, max, na.rm = TRUE) # number of categories - 1 for each item
  
  #--------- Polytomous items --------------------------------------------------
  if (any(class(beta) == "matrix")) {
    
    E <- matrix(nrow = N, ncol = K)
    W <- matrix(nrow = N, ncol = K)
    Civ <- matrix(nrow = N, ncol = K)
    
    for (i in 1:K) {
      
      probssim <- pcmfct(theta = theta, b = beta, ii = i)
      matx <- matrix(0:M, nrow = N, ncol = M+1, byrow = TRUE)
      E[, i] <- rowSums(matx * probssim, na.rm = TRUE)
      W[, i] <- rowSums((matx - E[, i])^2 * probssim, na.rm = TRUE)
      Civ[, i] <- rowSums((matx - E[, i])^4 * probssim, na.rm = TRUE)
      
    }
  #--------- Dichotomous items -------------------------------------------------
  } else if (class(beta) == "numeric") {
    
    probssim <- sapply(1:K, function(ii) irffct(theta = theta, b = beta, ii)[, 2])
    E <- probssim
    W <- probssim * (1-probssim)
    Civ <- E^4 * (1 - probssim) + (1 - E)^4 * probssim
    
  }

  R <- data - E
  Z <- R / sqrt(W) # unconditional standardised residuals
  Z[Z %in% c(Inf, -Inf)] <- NA
  
  outfitsim <- colSums(Z^2) / N
  infitsim <- colSums(R^2) / colSums(W)
  
  f <- (N * K - N - sum(mi) + 1) / K
  Y2 <- colSums(Z^2)
  Vout <- colSums(Civ / W^2) / N^2 - 1 / N
  fitresid <- (log(Y2) - log(f)) / sqrt(1 / f^2 * Vout * N^2)
  
  list(outfitsim = outfitsim, infitsim = infitsim, fitresid = fitresid)
}

#============= End define fit statistics functions =============================


#============= Define density plot functions ===================================

plotfct <- function(z, probs, breaks, labels, colours, xtitle, title) {
  
  dens <- density(z) 
  df <- data.frame(x = dens$x, y = dens$y)
  quantiles <- quantile(z, prob = probs) 
  df$quant <- factor(findInterval(df$x, quantiles),
                     labels = labels)
  
  ggplot2::ggplot(df, ggplot2::aes(x, y)) + 
    ggplot2::geom_line() + 
    ggplot2::geom_ribbon(ggplot2::aes(ymin = 0, ymax = y, fill = quant)) + 
    ggplot2::xlab(xtitle) + ggplot2::ylab("") +
    ggplot2::theme_minimal() + 
    ggplot2::ggtitle(title) +
    ggplot2::scale_fill_manual(values = colours, breaks = breaks, labels = breaks, 
                      name = "Quantiles")+ 
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank())
  
}

#============= End define density plot functions ===================================