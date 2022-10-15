library(magrittr)
library(dplyr)
djm_flow <- function(
    colors,
    background = "#fafafa",
    nlines = 500,
    tapering = c("constant", "linear", "bellshaped", "edge", "random"),
    noise_types = c("perlin", "knn", "svm", "rf", "cubic", "simplex", "worley"),
    geom_type = c("geom_path", "geom_point"),
    lwd_par = 0.05,
    iterations = 100,
    stepmax = 0.01,
    min_length = 0,
    max_length = 100 * stepmax,
    polar = FALSE,
    angles = NULL,
    left = 100 * -0.5,
    right = 100 * 1.5,
    bottom = 100 * -0.5,
    top = 100 * 1.5,
    plot_it = TRUE,
    pvec = NULL,
    color_fun = function(x, y) sample(colors, 1)) {
  aRtsy:::.checkUserInput(background = background, iterations = iterations)
  if (is.null(pvec)) pvec = rep(1, length(colors)) / length(colors)
  stopifnot(length(pvec) == length(colors))
  pvec <- pvec / sum(pvec)
  tapering <- match.arg(tapering)
  geom <- match.arg(geom_type)
  ncols <- right - left
  nrows <- top - bottom
  if (is.null(angles)) {
    noise_types <- match.arg(noise_types, several.ok = TRUE)
    noise_types <- sample(noise_types, 1)
    angles <- aRtsy:::.noise(
      dims = c(nrows, ncols),
      n = sample(100:300, size = 1),
      type = noise_types,
      limits = c(-pi, pi))
  } else {
    if (!is.matrix(angles)) stop("'angles' must be a matrix")
    if (nrow(angles) != nrows || ncol(angles) != ncols) {
      stop(paste0("'angles' must be a ", nrows, " x ", ncols, " matrix"))
    }
  }

  js <- 1:nlines
  steps <- stats::runif(nlines, min = min_length, max = max_length)
  streamlines <- purrr::map2_dfr(js, steps, ~ aRtsy:::iterate_flow(
    angles, .x, iterations, left, right, top, bottom, .y), .id = "z") %>%
    tibble::as_tibble()
  streamlines <- streamlines %>%
    dplyr::group_by(z) %>%
    dplyr::mutate(
      size = dplyr::case_when(
        tapering == "constant" ~ lwd_par,
        tapering == "linear" ~ lin_taper(1:n(), lwd_par),
        tapering == "bellshaped" ~ norm_taper(1:n(), lwd_par),
        tapering == "edge" ~ edge_taper(1:n(), lwd_par),
        TRUE ~ rand_taper(n(), lwd_par))
    )

  s <- streamlines %>%
    dplyr::summarise(mx = mean(x), my = mean(y)) %>%
    dplyr::mutate(color = color_fun(mx, my)) %>%
    dplyr::ungroup() %>%
    dplyr::select(z, color)

  streamlines <- dplyr::left_join(streamlines, s, by = "z")


  if (!plot_it) return(streamlines)

  artwork <- plot_flow(streamlines, geom, polar, background)
  return(artwork)
}

plot_flow <- function(streamlines,
                      geom_type = c("geom_path", "geom_point"),
                      polar = FALSE,
                      background = "#fafafa") {
  geom <- match.arg(geom_type)
  artwork <- ggplot2::ggplot(streamlines, ggplot2::aes(x, y, group = z))
  artwork <- switch(
    geom,
    geom_path = artwork +
      ggplot2::geom_path(size = streamlines$size, color = streamlines$color,
                         lineend = "round"),
    geom_point = artwork +
      ggplot2::geom_point(size = streamlines$size, color = streamlines$color)
  )
  if (polar) {
    artwork <- artwork + ggplot2::coord_polar()
  } else {
    artwork <- artwork +
      ggplot2::coord_cartesian(xlim = c(0, 100), ylim = c(0, 100))
  }
  artwork <- aRtsy::theme_canvas(artwork, background = background)
  return(artwork)
}

lin_taper <- function(s, lwd_par = 0.05) {
  s <- s / (length(s) + 1)
  x <- (1 - 2*abs(0.5 - s))
  x / sd(x) * lwd_par
}

norm_taper <- function(s, lwd_par = 0.05) {
  s <- s / (length(s) + 1)
  x <- dbeta(s, 5, 5)
  x / sd(x) * lwd_par
}

edge_taper <- function(s, lwd_par = 0.05) {
  s <- s / (length(s) + 1)
  x <- (s * (1 - s))^0.25
  x / sd(x) * lwd_par
}

rand_taper <- function(n, lwd_par = 0.05) {
  x <- abs(cumsum(rnorm(n, 0, sqrt(1))))
  x / sd(x) * lwd_par
}


rbin <- function(pmat) {
  u <- runif(nrow(pmat))
  pmat <- t(apply(pmat[,-ncol(pmat)], 1, cumsum))
  return(rowSums(u < pmat) + 1)
}

convex_comb <- function(colorpal, pvec, binop = function(x, y) x + y) {
  function(x, y) {
    n <- length(x)
    zz <- binop(x, y)
    zz <- (zz - min(zz)) / (max(zz) - min(zz))
    p <- zz %o% pvec + (1 - zz) %o% rev(pvec) # n x p
    colorpal[rbin(p)]
  }
}

recolor <- function(
    streamlines,
    rfun = function(x, y) {
      sample(c("#2c365e", "#0a8754", "#92b5c6", "#e98a15", "#a8201a"),
             length(x), replace = TRUE)
    }
) {
  streamlines %>%
    group_by(z) %>%
    select(x, y, z) %>%
    slice_sample(n = 1) %>%
    ungroup() %>%
    mutate(color = rfun(x, y)) %>%
    select(z, color) %>%
    right_join(streamlines %>% select(-color))

}


