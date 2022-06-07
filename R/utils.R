# Util functions

count_and_prop <- function(df, var_name) {
  df %>% 
    count({{var_name}}) %>% 
    ungroup() %>% 
    mutate(n_all = sum(n),
           percentage = n / n_all * 100)
}

'%ni%' <- Negate('%in%')

plot_count_and_prop <- function(df, var_name, theme_apa = TRUE, x_title = NULL) {
  plot <-
    df %>% 
    count_and_prop({{var_name}}) %>%
    # arrange(desc(percentage)) %>% 
    mutate({{var_name}} := as.factor({{var_name}})) %>%
    ggplot2::ggplot() +
    ggplot2::aes(
      x = {{var_name}},
      y = percentage
    ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 100), labels = scales::label_percent(scale = 1)) +
    ggplot2::labs(
      y = "Percentage"
      )
  
  if(theme_apa) {
    plot <-
      plot +
      papaja::theme_apa()
  } else {
    plot <-
      plot +
      theme_classic()
  }
  
  if(!is.null(x_title)) {
    plot <-
      plot + 
      ggplot2::labs(x = x_title)
  }
  
  plot
}
