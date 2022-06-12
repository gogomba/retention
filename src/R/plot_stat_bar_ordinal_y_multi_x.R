plot_stat_bar_ordinal_y_multi_x <- function(DF,
                                            groupCol,
                                            ordinalCol) {
  # when more than 2 clusters
  ordinal_group_cols <- c(ordinalCol, groupCol)
  DF_feature  <- DF[, ordinal_group_cols]
  
  
  #######################################
  # 1) Kruskal-wallis rank sum test
  #######################################
  # integer type
  DF_feature [[ordinalCol]] <- as.integer(DF_feature [[ordinalCol]])
  
  # 1) summary of kruskal_wallis test
  stat.test_kw_summary <-
    stat_test_kruskal_wallis_h(
      DF = DF_feature ,
      ordinalCol = ordinalCol ,
      groupCol = groupCol,
      get_effectsize = TRUE,
      output = "text"
    )
  
  # 2) pairwise post hoc dunn test
  stat.test_dunn <- stat_test_dunn(
    DF = DF_feature ,
    ordinalCol = ordinalCol,
    groupCol = groupCol,
    p_adjust_method = "holm"
  ) %>%
    
    # manually add y.postion for plotting
    stat_test_add_y_position(
      test_result = . ,
      y_start = 105,
      increaseBy = 5,
      significant_only = TRUE,
      p_value_col = "p"
    )
  
  ################################################################################
  # 3) plot
  ################################################################################
  # calculate percentage of CNA using DF_feature
  DF_per <- DF_feature  %>%
    dplyr::count(!!sym(groupCol),!!sym(ordinalCol)) %>%
    dplyr::group_by(!!sym(groupCol)) %>%
    mutate(pct = prop.table(n) * 100) %>%
    ungroup() %>%
    dplyr::mutate(CNV_Type = !!sym(ordinalCol)) %>%
    as.data.frame()
  
  DF_per[["Fill_Type"]]  <- as.character(DF_per[[ordinalCol]])
  DF_per[["Fill_Type"]] <-
    factor(DF_per[["Fill_Type"]], levels = c("2", "0", "-2"))
  
  custom_colors = c("#EB3C96", "white", "#999999")
  
  # basic stacked bar plot
  bar_plot <- plot_stack_bar_plot(
    DF = DF_per,
    x = groupCol,
    y = "pct",
    fill_color_by = "Fill_Type",
    colorCode = custom_colors ,
    xText = "",
    yText = "Count (%)",
    titleText = ordinalCol,
    legendText = "CNA",
    add_y_annotation = TRUE,
    y_scale_percentage = TRUE,
    coord_flip = FALSE
  )
  
  
  ################################################################################
  # plot and stat result
  ################################################################################
  bar_plot_stat <- bar_plot +
    ggpubr::stat_pvalue_manual(stat.test_dunn,
                               label = "p.formatted",
                               tip.length = 0) +
    labs(subtitle = stat.test_kw_summary,
         caption = expression(paste("Pairwise test: ", bold("Dunn test")))) +
    theme(legend.position = "right",
          plot.subtitle = element_text(size = 10, vjust = 2))
  
  # return plot
  return(bar_plot_stat)
  
}
