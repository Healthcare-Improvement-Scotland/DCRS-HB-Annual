# This is a function to calculate funnel plot limits (either adjusted for overdispersion or not)
# and plot the results as a ggplot using standard HIS formatting. See examples after the function code. 

library(dplyr)
library(ggplot2)

plot_funnel <- function(dat, # Dataframe
                        subgroup, # Name of each subgroup or area (e.g.an  NHS board)
                        denom, # Denominator numerical values
                        num, # Numerator numerical values
                        highlight, # Name of a subgroup to highlight
                        ref_line_type = "mean", # Type of reference line (i.e. "mean", otherwise target). Default is mean
                        target = NULL, # If ref_line_type is target, specify the target value
                        OD_type = "Laney", # Specify if adjustment for overdispersion should be applied ("Laney", otherwise none)
                        lblx= lblx, # x axis label. Default is "Denominator"
                        lbly = lbly, # y axis label/ Default is "Measure"
                        ttl= ttl,
                        sbttl= sbttl,
                        data_instead = FALSE # TRUE to get data output rather than plot
) {
  
  # Calculate no of areas and mean of all areas----------------------
  num_areas <- sum(dat[[ensym(denom)]])
  mean_areas <- sum(dat[[ensym(num)]]) / num_areas
  limit_min <- min(dat[[ensym(denom)]]) / 3
  limit_step <- (max(dat[[ensym(denom)]]) - limit_min) / 999
  limit_max <- max(dat[[ensym(denom)]])
  
  # calculate reference line
  if(ref_line_type == "mean"){ 
    ref_line <- mean_areas
    ref_line_label <- "Scotland Mean"
  } else {
    ref_line <- target/100
    ref_line_label <- paste0(target, "% target", collapse)
  }
  
  
  # Calculate extra columns------------------------------------------
  subgroup_data <<- dat %>%
    mutate(name = !!rlang::ensym(subgroup),
           denom = !!rlang::ensym(denom),
           area_prop = !!rlang::ensym(num) / !!rlang::ensym(denom), 
           SD_prop = sqrt(ref_line * (1 - ref_line) / !!rlang::ensym(denom)),
           z_score = (area_prop - ref_line) / SD_prop,
           z_win = if_else(percent_rank(z_score) <= 0.05, 
                           quantile(z_score, 0.05),
                           if_else(percent_rank(z_score) >= 0.95, 
                                   quantile(z_score, 0.95),
                                   z_score)),
           board_label = case_when(`Health Board` == "Ayrshire and Arran" ~ "AA",
                                   `Health Board` == "Borders" ~ "BO",
                                   `Health Board` == "Dumfries and Galloway" ~ "DG",
                                   `Health Board` == "Fife" ~ "FI",
                                   `Health Board` == "Forth Valley" ~ "FV",
                                   `Health Board` == "Grampian" ~ "GR",
                                   `Health Board` == "Greater Glasgow and Clyde" ~ "GG",
                                   `Health Board` == "National Golden Jubilee" ~ "GJ",
                                   `Health Board` == "Highland" ~ "HI",
                                   `Health Board` == "Lanarkshire" ~ "LA",
                                   `Health Board` == "Lothian" ~ "LO",
                                   `Health Board` == "Orkney" ~ "OR",
                                   `Health Board` == "Shetland" ~ "SH",
                                   `Health Board` == "Tayside" ~ "TA",
                                   `Health Board` == "Western Isles" ~ "WI")
    )
  
  # 
  if(OD_type == "Laney"){
    OD_factor <- sd(subgroup_data$z_win)
  } else {
    OD_factor <- 1
  }
  
  subgroup_data <- subgroup_data %>%
    mutate(adj_z_score = z_score / OD_factor
    )
  
  # Return data frame without plot if requested------------------------
  if(data_instead == TRUE){
    subgroup_data = subgroup_data %>%
      mutate(uwl = (ref_line + (2 * OD_factor * SD_prop)),
             lwl = (ref_line - (2 * OD_factor * SD_prop)),
             ucl = (ref_line + (3 * OD_factor * SD_prop)),
             lcl = (ref_line - (3 * OD_factor * SD_prop)),
             ref_line = ref_line)
    return(subgroup_data)
  }
  
  # Calculate limits and mean/target----------------------------------------
  limit_data <- data.frame(x_value = seq(limit_min, limit_max, limit_step)) %>%
    mutate(ref_line_plot = ref_line * 100,
           limit_SD_prop = sqrt(ref_line * (1 - ref_line) / x_value),
           uwl = (ref_line + (2 * OD_factor * limit_SD_prop)) * 100,
           lwl = (ref_line - (2 * OD_factor * limit_SD_prop)) * 100,
           ucl = (ref_line + (3 * OD_factor * limit_SD_prop)) * 100,
           lcl = (ref_line - (3 * OD_factor * limit_SD_prop)) * 100
    )
  
  limit_max_y = max(subgroup_data$area_prop) * 120
  limit_max_x = max(limit_data$x_value) * 1.02
  
  # Plot mean, control limits and warning limits---------------------------

    ggplot(limit_data, mapping = aes(x = x_value)) +
      geom_line(mapping = aes(y = ref_line_plot, linetype = "Scotland Mean"),
                linewidth = 1, colour = "#a6a6a6" #dark grey
      ) +
      geom_line(mapping = aes(y = ucl, linetype = "Control Limit"),
                linewidth = 0.6, colour = "#004380" #dark grey
      ) +
      geom_line(mapping = aes(y = lcl, linetype = "Control Limit"),
                linewidth = 0.6, colour = "#004380" #dark grey
      ) +
      geom_line(mapping = aes(y = uwl, linetype = "Warning Limit"),
                linewidth = 0.5, colour = "#4978a3" #dark grey
      ) +
      geom_line(mapping = aes(y = lwl, linetype = "Warning Limit"),
                linewidth = 0.5, colour = "#4978a3" #dark grey
      ) +
      # Plot Custody centres---------------------------------------------
    geom_point(data = subgroup_data,
               aes(x = !!rlang::ensym(denom), y = area_prop * 100),
               size = 1.90, stroke = 1.55, colour = "#a6a6a6", #HIS dark blue
    ) +
      # Highlight selected centre
      geom_point(data = subgroup_data %>% filter(name == highlight),
                 aes(x = !!rlang::ensym(denom), y = area_prop * 100),
                 size = 1.90, stroke = 1.55, colour = "#004380", #HIS dark blue
      )+
      # Label selected board
    geom_text_repel(data = subgroup_data %>% filter(name == highlight),
                aes(x = !!rlang::ensym(denom), y = area_prop * 100, label = board_label),
                 size = 2) +
      # create legend--------------------------------------------------------
    scale_shape_manual(name = "", values = c(subgroup = 21)) +
      scale_linetype_manual(
        name = "",
        breaks = c("Control Limit", "Warning Limit", "Scotland Mean"),
        values = c(
          "Control Limit" = "3232",
          "Warning Limit" = "1212",
          "Scotland Mean" = "solid"
        ),
        
      ) +
      # Change  titles-----------------------------------------------------
    labs(
      x= lblx,
      y = lbly,
      title = ttl,
      subtitle = sbttl) +
      # Add thousandth separator-------------------------------------------
  scale_x_continuous(n.breaks = 10, expand = c(0.00,0.00), labels = scales::comma) +
    scale_y_continuous(expand = c(0.00,0.00)) +
    coord_cartesian(ylim = c(0, limit_max_y), xlim = c(0, limit_max_x)) +
      # x axis numbers to K, M, B-------------------------------------------
    #labels = label_number(scale_cut = cut_short_scale())+
    # Change background--------------------------------------------------
    theme(
      # Hide panel borders, remove grid lines and background colour------
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      # Change axis lines-----------------------------------------------
      axis.line = element_line(colour = "dark grey"),
      # put box round legend and make background white--------------------
      legend.text = element_text(size = 8),
      legend.background = element_blank(),
      legend.margin = margin(t = -3, r = 4, b = -2, l = 0, unit = "pt"),
      legend.box.background = element_rect(colour = "light grey"),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.position="bottom"
    )

}
# # Create test data
# abc=data.frame(name=c("a","b","c","d","e","f"),xx=c(30,33,37,40,50,60), yy=c(4,14,8,12,10,9))
#
# # Accepts quoted or unquoted column names
# plot_funnel(dat=abc, subgroup=name, denom=xx, num=yy, highlight = "d")
# plot_funnel(dat=abc, subgroup="name", denom="xx", num="yy", highlight = "d")
# 
# # To get data outputted instead of plot
# plot_funnel(dat=abc, subgroup=name, denom=xx, num=yy, data_instead = TRUE)
# 
# # The data output can work with pipes (%>%)
# # e.g. This code calculates adjusted z scores for three sets of data, then plots as a spine chart. 
# # It uses the same data frame but more helpful if used with different data sets
# plot_funnel(dat=abc, subgroup="name", denom="xx", num="yy", data_instead = TRUE) %>%  filter(name == "c") %>%
#   bind_rows(plot_funnel(dat=abc, subgroup="name", denom="xx", num="yy", data_instead = TRUE) %>% filter(name == "e")) %>%
#   bind_rows(plot_funnel(dat=abc, subgroup="name", denom="xx", num="yy", data_instead = TRUE) %>% filter(name == "a")) %>%
#   ggplot() +
#   geom_point(aes(x = adj_z_score,
#                  y = name,
#                  size = 5)
#              ) +
#   geom_vline(xintercept = c(-3,-2,0,2,3),
#              linetype="dashed",
#              color = "gray",
#              size=1) +
#   theme(panel.background = element_blank(),
#         legend.position="none") +
#   scale_x_continuous(breaks = c(-3,-2,0,2,3)) +
#   labs(x = "Number of standard deviations from Scotland", y = NULL) +
#   ggtitle("Differentness from Scotland in standard deviations")
# 
# #plot_funnel(dat=gg.compare, subgroup="Police Station", denom="n_stays", num="n_hosp", highlight = "Dundee")
