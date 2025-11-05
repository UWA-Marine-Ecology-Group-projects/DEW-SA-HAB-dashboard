half_donut_with_dial <- function(
    segments = c("Low", "Medium", "High"),
    values,                       # must be length 3
    mode = c("absolute", "proportion"),
    status = NULL,                 # can be 1..3 or "Low"/"Medium"/"High"
    r_inner = 0.5,
    r_outer = 1,
    show_labels = TRUE
){
  mode <- match.arg(mode)
  stopifnot(length(segments) == 3L, length(values) == 3L)
  
  # colours: High = red, Medium = yellow, Low = green
  tier_colors <- c("Low" = "#43A047", "Medium" = "#FDD835", "High" = "#E53935")
  
  # shares for half-donut
  shares <- switch(mode,
                   "absolute"   = values / sum(values),
                   "proportion" = values)
  if (mode == "proportion") stopifnot(abs(sum(shares) - 1) < 1e-8)
  
  # angles from -pi/2 to +pi/2
  cc <- cumsum(c(-pi/2, shares * pi))
  cc[length(cc)] <- pi/2
  mids <- colMeans(rbind(cc[-1], cc[-length(cc)]))
  
  # plotting extents
  xlim <- c(-1.3 * r_outer, 1.3 * r_outer)
  ylim <- c(0, 1.3 * r_outer)
  
  df <- data.frame(
    seg = factor(segments, levels = segments),
    start = cc[-length(cc)],
    end   = cc[-1]
  )
  
  p <- ggplot() +
    coord_fixed() +
    theme_void() +
    expand_limits(x = xlim, y = ylim) +
    theme(legend.position = "none") +
    geom_arc_bar(
      data = df,
      col = "white",
      linewidth = 2,
      aes(x0 = 0,
          y0 = 0,
          r0 = r_inner,
          r  = r_outer,
          start = start,
          end = end,
          fill = seg)
    ) +
    scale_fill_manual(values = tier_colors)
  
  # labels (optional, only once)
  if (show_labels) {
    label_r <- r_outer * 1.08
    lab_df <- data.frame(
      x = sin(mids) * label_r,
      y = pmax(cos(mids) * label_r, 0.015),
      lab = segments
    )
    p <- p + geom_text(data = lab_df,
                       aes(x = x, y = y, label = lab),
                       fontface = "bold", size = 5)
  }
  
  # dial (needle)
  if (!is.null(status)) {
    resolve_to_theta <- function(st) {
      if (is.numeric(st)) {
        idx <- as.integer(st)
        if (idx >= 1 && idx <= 3) return(mids[idx])
      } else {
        kw <- tolower(st)
        if (kw == "low") return(mids[1])
        if (kw == "medium" || kw == "med") return(mids[2])
        if (kw == "high") return(mids[3])
      }
      NA_real_
    }
    
    theta <- resolve_to_theta(status)
    if (is.na(theta)) {
      warning("`status` didn't match a segment or tier; skipping dial.")
    } else {
      needle_r <- r_outer * 0.6
      p <- p +
        geom_segment(aes(x = 0, y = 0,
                         xend = sin(theta) * needle_r,
                         yend = cos(theta) * needle_r),
                     linewidth = 3) +
        geom_point(aes(x = 0, y = 0), size = 10)
    }
  }
  
  p
}