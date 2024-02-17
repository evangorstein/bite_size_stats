# adapted from Intro to Causal inference with R (Brumback)

get_densities <- function(trt, ehat, bw=0.05) {
  dens_control <- density(ehat[trt==0], bw = bw)
  dens_trt <- density(ehat[trt==1], bw = bw)
  
  densities <- list("control" = dens_control, 
                    "trt" = dens_trt)
}

setup_overlap_plot <- function(densities, title) {
  rng <- range(
    range(densities[["trt"]]$y), 
    range(densities[["control"]]$y)
  )
  plot(c(0,1), rng, type = "n", 
       xlab = "propensity score", 
       ylab = "density", main = title) 
}

prop_overlap <- function() {
  # Generate confounders
  H1 <- rnorm(1000) 
  H2 <- rbinom(1000, 1, 0.3)
  
  # Generate treatment according to a propensity score model that will produce overlap
  e0 <- plogis(-1.5 + H1 + 3*H2)
  T0 <- rbinom(1000, 1, e0)
  
  # Generate treatment according to two different propensity score models that will not produce overlap
  T1 <- H2
  T2 <- pmax(T0, T1)
  
  # Estimate propensity scores in each scenario
  e0hat <- fitted(glm(T0 ~ H1 + H2, family = binomial))
  e1hat <- fitted(glm(T1 ~ H1 + H2, family = binomial))
  e2hat <- fitted(glm(T2 ~ H1 + H2, family = binomial))
  
  # Plot overlap of propensity scores in each case

  # With T0
  d0 <- get_densities(T0, e0hat)
  setup_overlap_plot(d0, "Overlap of propensity scores under T0")
  lines(d0[["control"]], lty = 2)
  lines(d0[["trt"]], lty = 1)
  legend("topleft", c("T0 = 0", "T0 = 1"), lty = c(2,1))
  
  # With T1
  d1 <- get_densities(T1, e1hat)
  setup_overlap_plot(d1, "Overlap of propensity scores under T1")
  lines(d1[["control"]], lty = 2)
  lines(d1[["trt"]], lty = 1)
  legend("topleft", c("T1 = 0", "T1 = 1"), lty = c(2,1))
  
  # With T2
  d2 <- get_densities(T2, e2hat)
  setup_overlap_plot(d2, "Overlap of propensity scores under T2")
  lines(d2[["control"]], lty = 2)
  lines(d2[["trt"]], lty = 1)
  legend("topleft", c("T2 = 0", "T2 = 1"), lty = c(2,1))
  
}

prop_overlap()


