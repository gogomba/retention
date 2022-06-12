# Kruskal–Wallis H test

stat_test_kruskal_wallis_h <-
  function(DF,
           ordinalCol,
           groupCol,
           get_effectsize = FALSE,
           get_effectsize_ci = FALSE,
           output ="table" ) {
    # output can be table of a text summary (can be used as subtitle in ggplot)
    
    # rename DF column name
    colnames(DF)[colnames(DF) == ordinalCol] <- "ordinalCol"
    colnames(DF)[colnames(DF) == groupCol] <- "groupCol"
    
    
    # test
    kwh_test <- DF %>%
      rstatix::kruskal_test(ordinalCol ~ groupCol) %>%
      as.data.frame()
    
    
    # calculate effect size
    if (get_effectsize == TRUE) {
      kwh_effect_size <- DF %>%
        rstatix::kruskal_effsize(ordinalCol ~ groupCol, ci = get_effectsize_ci) %>%
        as.data.frame()
      
      colnames(kwh_effect_size)[colnames(kwh_effect_size) == "method"] <-
        "effsize_method"
      
      
      kwh_result <- kwh_test %>%
        left_join(kwh_effect_size, by = c(".y.", "n"))
      
    } else {
      kwh_result <- kwh_test
    }
    
    
    # format
    is.num <- c("statistic", "effsize")
    kwh_result[is.num] <- lapply(kwh_result[is.num] , round, 3)
    
    
    kwh_result <- kwh_result %>%
      dplyr::mutate(p.formatted = case_when(p > 0.001   ~ paste0(format(
        round(p,3)
      )),
      # scientific
      p < 0.001 ~
        paste0(format(
          p, scientific = TRUE,
          digits = 3
        ))))
    
    
    # rename first column .y. to y
    colnames(kwh_result)[colnames(kwh_result) == ".y."] <- "y"
    
    
    kwh_result$y <- ordinalCol
    
    # output
    if (output == "table" ){
      return(kwh_result)
    } else if(output == "text"){
      kwh_result_summary <- paste0(
        "Kruskal-Wallis = ", kwh_result$statistic, "; ", 
        "p-value: ", kwh_result$p.formatted,"; ", 
        "Effect size: ", kwh_result$magnitude, " (", kwh_result$effsize_method, " = ",kwh_result$effsize, "); ",
        "n = ",kwh_result$n)
      return(kwh_result_summary)
      
    }
    
  }
