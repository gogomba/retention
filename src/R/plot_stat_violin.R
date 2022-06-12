# make violin plot with statistics
plot_stat_violin <-
  function(DF,
           statistical_type = "np",
           x,
           y,
           xText = NULL,
           yText = NULL,
           titleText = NULL,
           custom_colors = NULL,
           showStats =TRUE) {
    # statistcal type
    #"p" (for parametric), "np" (for nonparametric), "r" (for robust), "bf" (for Bayes Factor)
    # title text
    if (length(titleText) == 0) {
      titleText <- as.character(y)
    }
    
    # x text
    if (length(xText) == 0) {
      xText <- as.character(x)
    }
    
    # y text
    if (length(yText) == 0) {
      yText <- as.character(y)
    }
    
    # rename DF
    colnames(DF)[colnames(DF) == x] <- "x_col"
    colnames(DF)[colnames(DF) == y] <- "y_col"
    
    # plot stat violin plot
    p <- ggstatsplot::ggbetweenstats(
      data  = DF,
      type = statistical_type,
      x     = x_col,
      y     = y_col,
      xlab = xText,
      ylab = yText,
      title = titleText,
      pairwise.comparisons = FALSE,
      var.equal =TRUE,
      subtitle = TRUE
    )
    
    # customize colors
    if (length(custom_colors) > 0) {
      p <- p + scale_color_manual(values = c(custom_colors))
    }
    
    if (showStats ==FALSE){
      p <- p+ theme(plot.subtitle = element_blank())
    }
    return(p)
    
  }