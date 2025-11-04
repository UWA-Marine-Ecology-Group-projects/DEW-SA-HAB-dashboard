half_donut_with_dial <- function(
    segments,                   # e.g. c("Very Poor","Poor","Good","Very Good")
    values,                     # same length as segments
    colors = NULL,              # named vector, names(colors) == segments
    mode = c("absolute","proportion"),
    status = NULL,              # can be 1..4, a segment name, or "High"/"Med"/"Low"
    r_inner = 0.5,
    r_outer = 1,
    show_segment_labels = TRUE,
    show_tier_labels = TRUE     # adds "High", "Med", "Low" across the arc
){
  mode <- match.arg(mode)
  stopifnot(length(segments) == 4L, length(values) == 4L)
  
  # shares for half-donut
  shares <- switch(mode,
                   "absolute"   = values / sum(values),
                   "proportion" = values)
  if (mode == "proportion") stopifnot(abs(sum(shares) - 1) < 1e-8)
  
  # angles from -pi/2 to +pi/2
  cc <- cumsum(c(-pi/2, shares * pi))
  cc[length(cc)] <- pi/2                  # guard rounding drift
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
          r = r_outer,
          start = start, 
          end = end, 
          fill = seg)
    )
  
  if (!is.null(colors)) {
    if (is.null(names(colors))) names(colors) <- segments
    p <- p + scale_fill_manual(values = colors)
  }
  
  # segment labels (optional)
  if (show_segment_labels) {
    label_r <- r_outer * 1.08
    lab_df <- data.frame(
      x = sin(mids) * label_r,
      y = pmax(cos(mids) * label_r, 0.015),
      lab = segments
    )
    p <- p + geom_text(data = lab_df, aes(x = x, y = y, label = lab),
                       fontface = "bold", size = 4)
  }
  
  # tier labels: High (seg 1), Med (avg seg 2 & 3), Low (seg 4)
  if (show_tier_labels) {
    tier_r <- r_outer * 1.12  # <-- closer to rim (was 1.25)
    tier_theta <- c(mids[1], mids[2], mids[3], mids[4])
    tier_lab <- c("Very\nPoor", "Poor", "Good", "Very\nGood")
    tier_df <- data.frame(
      x = sin(tier_theta) * tier_r,
      y = pmax(cos(tier_theta) * tier_r, 0.01),
      lab = tier_lab
    )
    p <- p + geom_text(data = tier_df, 
                       aes(x = x, 
                           y = y, 
                           label = lab),
                       # fontface = "bold", 
                       size = 5)
  }
  
  # dial (needle)
  if (!is.null(status)) {
    # resolve status to an angle:
    resolve_to_theta <- function(st) {
      if (is.numeric(st)) {
        idx <- as.integer(st)
        if (idx >= 1 && idx <= 4) return(mids[idx])
      } else {
        # exact segment name?
        m <- match(st, segments, nomatch = 0)
        if (m > 0) return(mids[m])
        # tier keywords
        kw <- tolower(st)
        if (kw == "high") return(mids[1])
        if (kw == "low")  return(mids[4])
        if (kw == "med" || kw == "medium") return(mean(mids[2:3]))
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