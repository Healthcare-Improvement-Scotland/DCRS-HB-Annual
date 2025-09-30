#############  Wrangles the data and creates the charts and corresponding figures for the narrative used in part 4 of the report

Board <- hdr

#######Part 4 - Charts used in service breakdown and corresponding values for narrative#######

###Create function to flag if significant change is identified to use logical chart title script  
#Significant changes in both directions identified 
sigboth <- function(dataset) {dataset %>%
    group_by(dataset[2]) %>%
    mutate(count = n()) %>%
    ungroup() %>%
    filter(count == 2) %>%
    mutate(direction = paste(direction, collapse = ''),
           sig_flag = case_when(str_detect(direction , fixed("increase")) & str_detect(direction , fixed("decrease")) ~ 1, TRUE ~ 0)) %>%
    summarise(sig_flag = sum(sig_flag)) %>%
    pull(sig_flag)}

#significant increases only identified
sigup <- function(dataset) {dataset %>%
    group_by(dataset[2]) %>%
    mutate(count = n()) %>%
    ungroup() %>%
    filter(count == 2) %>%
    mutate(direction = paste(direction, collapse = ''),
           sig_flag = case_when(str_detect(direction , fixed("increase"))  ~ 1, TRUE ~ 0)) %>%
    summarise(sig_flag = sum(sig_flag)) %>%
    pull(sig_flag)}

#significant decreases only identified
sigdown <- function(dataset) {dataset %>%
    group_by(dataset[2]) %>%
    mutate(count = n()) %>%
    ungroup() %>%
    filter(count == 2) %>%
    mutate(direction = paste(direction, collapse = ''),
           sig_flag = case_when(str_detect(direction , fixed("decrease"))  ~ 1, TRUE ~ 0)) %>%
    summarise(sig_flag = sum(sig_flag)) %>%
    pull(sig_flag)}


###Function to add required caption to chart if significant change is identified
chart_titles <- function(chart_title, sig_both, sig_up, sig_down) {
  
  chart_lab <- if (sig_both >= 1) {
    labs(title = chart_title,   #set titles
         subtitle = year3,
         caption = "*^ significant increase from previous year \n *v significant decrease from previous year", #Caption if significant change is identified
         y = "",
         x = "")
  } else if (sig_up >= 1) { 
    labs(title = chart_title,   #set titles
         subtitle = year3,
         caption = "*^ significant increase from previous year", #Caption if significant change is identified
         y = "",
         x = "")
  } else if (sig_down >= 1) {
    labs(title = chart_title,   #set titles
         subtitle = year3,
         caption = "*v significant decrease from previous year", #Caption if significant change is identified
         y = "",
         x = "")
  } else {
    labs(title = chart_title,   #set titles
         subtitle = year3,
         y = "",
         x = "")
  }
}

# Chart 5 closure categories -----------------------------------------------------------------

##closure chart 5 - total breakdown##
#This chart is not selected for larger boards. More detailed primary/secondary care breakdown is used instead

#extract current and previous year denominator to use in CI test
HB_n1 <- dcrs_data_report_all %>% select(Year, `Health Board`, closure_category) %>% filter(Year == 2, `Health Board` == "Board") %>% pull(closure_category)
HB_n2 <- dcrs_data_report_all %>% select(Year, `Health Board`, closure_category) %>% filter(Year == 3, `Health Board` == "Board") %>% pull(closure_category)
Scot_n1 <- dcrs_data_report_all %>% select(Year, `Health Board`, closure_category) %>% filter(Year == 2, `Health Board` == "Scotland") %>% pull(closure_category)
Scot_n2 <- dcrs_data_report_all %>% select(Year, `Health Board`, closure_category) %>% filter(Year == 3, `Health Board` == "Scotland") %>% pull(closure_category)


if (HB_n1 > 0 & HB_n2 > 0) {  #CI test only works when both denominators are greater than zero, otherwise error occurs 
dcrs_data_wrangle_closure <- dcrs_data_report_all %>%
  select(Year, `Health Board`, causal_timscale, cause_of_death,
         conditions_omitted, disposal_hazard, sequence_of_cause, cause_too_vague ) %>%
  pivot_longer(cols = !c(Year, `Health Board`,),
               names_to = 'closure_category', 
               values_to = 'closure_num') %>%
  pivot_wider(names_from = Year, 
              values_from = 'closure_num') %>% 
  mutate(denom1 = case_when(`Health Board` == "Board" ~ HB_n1,
                            `Health Board` == "Scotland" ~ Scot_n1),
         denom2 = case_when(`Health Board` == "Board" ~ HB_n2,
                            `Health Board` == "Scotland" ~ Scot_n2),
         closure_rate = `3`/denom2, #rates used to check direction of change
         closure_rate_old = `2`/denom1,
         ci_mn = BinomDiffCI(x1 = `2`, n1 = denom1, x2 = `3`, n2 = denom2, method="mn"), #Apply Miettinen-Nurminen CI Method for significance between 2 years
         ci_sig = case_when(denom1 >= 10 & denom2 >= 10 ~ (case_when(ci_mn[, "lwr.ci"] < 0 & ci_mn[, "upr.ci"] > 0 ~ "", TRUE ~ "*")), #Flag with * if significance is found (if sample is at least 20 cases)
                            TRUE ~ ""),
         direction = case_when(ci_sig == "*" & closure_rate > closure_rate_old ~ "increase",  #identify direction of change
                               ci_sig == "*" & closure_rate < closure_rate_old ~ "decrease",
                               TRUE ~ ""),  
         ci_sig = case_when(ci_sig == "*" & closure_rate > closure_rate_old ~ "*^",  #update chart maker to show direction of change
                            ci_sig == "*" & closure_rate < closure_rate_old ~ "*v",
                            TRUE ~ ""), 
         closure_percent = paste0(percent(`3` / denom2, accuracy = 0.1),ci_sig),
         rank_cases = case_when(`Health Board` == 'Board' ~ min_rank(closure_rate), TRUE ~ 0)) %>% 
  filter(closure_rate != 0) %>%
  select(`Health Board`, closure_category, closure_rate, closure_rate_old, closure_percent, ci_sig, direction, rank_cases)

#Create flag if significant change is identified to use logical chart title script  
sig_flag_both <- sigboth(dcrs_data_wrangle_closure)
sig_flag_up <- sigup(dcrs_data_wrangle_closure)
sig_flag_down <- sigdown(dcrs_data_wrangle_closure)

#Add caption to chart if significant change is identified
chart_lab <- chart_titles("Chart 5: Breakdown of clinical closure categories for ‘not in order’",
                          sig_flag_both, sig_flag_up, sig_flag_down)
} else if (HB_n2 >0) {  #create dataframe with no CI test if first condition is not met
  dcrs_data_wrangle_closure <- dcrs_data_report_all %>%
    select(Year, `Health Board`, causal_timscale, cause_of_death,
           conditions_omitted, disposal_hazard, sequence_of_cause, cause_too_vague ) %>%
    pivot_longer(cols = !c(Year, `Health Board`,),
                 names_to = 'closure_category', 
                 values_to = 'closure_num') %>%
    pivot_wider(names_from = Year, 
                values_from = 'closure_num') %>% 
    mutate(denom2 = case_when(`Health Board` == "Board" ~ HB_n2,
                              `Health Board` == "Scotland" ~ Scot_n2),
           closure_rate = `3`/denom2,
           closure_percent = paste0(percent(`3` / denom2, accuracy = 0.1),ci_sig),
           rank_cases = case_when(`Health Board` == 'Board' ~ min_rank(closure_rate), TRUE ~ 0)) %>% 
    filter(closure_rate != 0) %>%
    select(`Health Board`, closure_category, closure_rate, closure_percent, rank_cases)  
  
  #Add title and captions for chart
  chart_lab <-  labs(title = "Chart 5: Breakdown of clinical closure categories for ‘not in order’",   #set titles
                     subtitle = year3,
                     y = "",
                     x = "")
} 

if (HB_n2 > 0 ) { #only create chart if there are cases for the current year
case_count <- dcrs_data_wrangle_closure %>%
  count(closure_category)

#find how many categories had cases at the board
case_count_board <- dcrs_data_wrangle_closure %>%
  summarise(rank_cases = sum(rank_cases)) %>% pull(rank_cases)

if(case_count_board >= 1) { #only run if there are cases at the board
  #Match back with wrangled dataset and filter out any rows with no data
  dcrs_data_wrangle_closure <- dcrs_data_wrangle_closure %>%
    left_join(case_count) %>%
    filter(n == 2) } else
    {dcrs_data_wrangle_closure}

#Create bar chart 5 in ranked order
dcrs_closure_plot <- ggplot(dcrs_data_wrangle_closure, aes(fct_reorder(closure_category, closure_rate), 
                                                            closure_rate,fill = `Health Board`)) +
  geom_bar(stat = "identity", position = 'dodge', width=0.8) + #set bar aesthetics
  scale_fill_manual(values = c("#8BB5E8", "#004578"), breaks=c('Scotland', 'Board'), name = "Location") +  #set bar aesthetics  
  chart_lab  +  #add chart title and captions set earlier
  scale_x_discrete(labels = c("causal_timscale" = "Causal timescales incorrect",   #rename variables
                              "cause_of_death" = "Cause of Death incorrect", 
                              "conditions_omitted" = "Conditions omitted",
                              "disposal_hazard" = "Disposal Hazard incorrect", 
                              "sequence_of_cause" = "Sequence of Cause of Death incorrect", 
                              "cause_too_vague" = "Cause of death too vague")) +
  scale_y_continuous(expand=c(0, 0), limits=c(0, max(dcrs_data_wrangle_closure$closure_rate)*1.15),
                     labels = scales::percent_format(accuracy = 1)) +  #set percentage                       #set dynamic axis limit
  geom_text(aes(label=closure_percent), position = position_dodge(width = .9), hjust=-0.2, size = 2.5) +  #set labels
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),  #set simple theme
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        legend.title=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) + 
  coord_flip()     #flip chart on side

dcrs_closure_plot        
#dcrs_data_wrangle_closure
} else {dcrs_closure_plot <- "not enough data"}  #create message if no chart is created to use place in the report

# Chart 5 primary/secondary closure breakdown-----------------------------------------------


##closure chart 5 - primary/secondary care breakdown##
#This chart is not selected for smaller boards. More previous less detailed breakdown is used instead

#extract current and previous year denominator to use in CI test
HB_n1 <- dcrs_data %>% filter(`Health Board` == Board, `Case Type` == "Standard", primary_secondary != "Uknown", Year == 1) %>%
  summarise(closure_category = sum(`Closure Category - Clinical1` == "Yes", na.rm = TRUE)) %>% pull(closure_category)
HB_n2 <- dcrs_data_report_all %>% select(Year, `Health Board`, closure_category) %>% filter(Year == 2, `Health Board` == "Board") %>% pull(closure_category)
HB_n3 <- dcrs_data_report_all %>% select(Year, `Health Board`, closure_category) %>% filter(Year == 3, `Health Board` == "Board") %>% pull(closure_category)

#set data for case of death primary/secondary case breakdown
if (HB_n1 > 0 & HB_n2 > 0& HB_n3 > 0) { #CI test only works when both denominators are greater than zero, otherwise error occurs 
#Chart 6a data - percentages and significance test, wrangling into suitable layout
dcrs_data_cause_ps <- dcrs_data %>%
  filter(`Health Board` == Board,
         `Case Type` == "Standard",
         `Created On` < end_date,
         primary_secondary != "Uknown") %>%
  group_by(Year, primary_secondary) %>%
  summarise(cause_too_vague = sum(`Cause of death too vague1` == "Yes", na.rm = TRUE),
            causal_timscale = sum(`Causal timescales incorrect1` == "Yes", na.rm = TRUE),
            cause_of_death = sum(`Cause of Death incorrect1` == "Yes", na.rm = TRUE),
            conditions_omitted = sum(`Conditions omitted1` == "Yes", na.rm = TRUE),
            disposal_hazard = sum(`Disposal Hazard incorrect1` == "Yes", na.rm = TRUE),
            sequence_of_cause = sum(`Sequence of Cause of Death incorrect1` == "Yes", na.rm = TRUE)
            , .groups = "rowwise") %>%
  select(Year, primary_secondary, causal_timscale, cause_of_death,
         conditions_omitted, disposal_hazard, sequence_of_cause, cause_too_vague) %>%
  filter(Year == 1 | Year == 2 | Year == 3) %>%
  pivot_longer(cols = !c(Year, primary_secondary), 
               names_to = 'closure_category', 
               values_to = 'closure_num')

#run CI test to check significant changes
dcrs_data_cause_ps_sig <- dcrs_data_cause_ps %>%
  pivot_wider(names_from = Year, 
              values_from = 'closure_num') %>%
  mutate(denom1 = HB_n1, denom2 = HB_n2, denom3 = HB_n3,
         closure_rate1 = `1`/denom2,
         closure_rate2 = `2`/denom2,
         closure_rate3 = `3`/denom3,
         ci_mn1 = BinomDiffCI(x1 = `1`, n1 = denom1, x2 = `2`, n2 = denom2, method="mn"), #Apply Miettinen-Nurminen CI Method for significance between 2 years
         ci_mn2 = BinomDiffCI(x1 = `2`, n1 = denom2, x2 = `3`, n2 = denom3, method="mn"), 
         ci_sig1 = case_when(denom1 >= 10 & denom2 >= 10 ~ (case_when(ci_mn1[, "lwr.ci"] < 0 & ci_mn1[, "upr.ci"] > 0 ~ "", TRUE ~ "*")), #Flag with * if significance is found (if sample is at least 20 cases)
                            TRUE ~ ""),
         ci_sig2 = case_when(denom2 >= 10 & denom3 >= 10 ~ (case_when(ci_mn2[, "lwr.ci"] < 0 & ci_mn2[, "upr.ci"] > 0 ~ "", TRUE ~ "*")), #Flag with * if significance is found (if sample is at least 20 cases)
                             TRUE ~ ""),
         direction1 = case_when(ci_sig1 == "*" & closure_rate2 > closure_rate1 ~ "increase",
                               ci_sig1 == "*" & closure_rate2 < closure_rate1 ~ "decrease",
                               TRUE ~ ""),       
         ci_sig1 = case_when(ci_sig1 == "*" & closure_rate2 > closure_rate1 ~ "*^",
                            ci_sig1 == "*" & closure_rate2 < closure_rate1 ~ "*v",
                            TRUE ~ ""), 
         direction2 = case_when(ci_sig2 == "*" & closure_rate3 > closure_rate2 ~ "increase",
                                ci_sig2 == "*" & closure_rate3 < closure_rate2 ~ "decrease",
                                TRUE ~ ""),       
         ci_sig2 = case_when(ci_sig2 == "*" & closure_rate3 > closure_rate2 ~ "*^",
                             ci_sig2 == "*" & closure_rate3 < closure_rate2 ~ "*v",
                             TRUE ~ "")) %>%
  select(primary_secondary, closure_category, closure_rate1, closure_rate2, closure_rate3, ci_sig1, ci_sig2, direction1, direction2)

#match back on any signficant flags and rename cases
dcrs_data_wranlge_ps <- dcrs_data_cause_ps %>%
  left_join(dcrs_data_cause_ps_sig, by = c("primary_secondary", "closure_category")) %>%
  mutate(closure_rate = case_when(Year == 1 ~ closure_rate1, Year == 2 ~ closure_rate2, Year == 3 ~ closure_rate3),
         closure_rate_old = case_when(Year == 1 ~ 0, Year == 2 ~ closure_rate1, Year == 3 ~ closure_rate2),
         closure_percent = case_when(Year == 1 ~ percent(closure_rate, accuracy = 1),
                                     Year == 2 ~ paste0(percent(closure_rate, accuracy = 1),ci_sig1),
                                     Year == 3 ~ paste0(percent(closure_rate, accuracy = 1),ci_sig2)),
         closure_category = str_replace(closure_category, "causal_timscale", "Causal timescales incorrect"), 
         closure_category = str_replace(closure_category, "cause_of_death", "Cause of death incorrect"),
         closure_category = str_replace(closure_category, "conditions_omitted", "Conditions omitted"), 
         closure_category = str_replace(closure_category, "disposal_hazard", "Disposal hazard incorrect"),
         closure_category = str_replace(closure_category, "sequence_of_cause", "Sequence of cause of death incorrect"),
         closure_category = str_replace(closure_category, "cause_too_vague", "Cause of death too vague"),
         closure_category = str_wrap(closure_category, width = 10),
         direction = case_when(Year == 2 ~ direction1,
                               Year == 3 ~ direction2),
         closure_percent = case_when(closure_percent == "0%" ~ "", # remove some percent labels to decrease clutter on chart
                                     closure_rate < 0.03 & direction == "" ~ "",
                                     TRUE ~ closure_percent),
         Year = case_when(Year == 1 ~ y1,  #rename year to match current periods
                          Year == 2 ~ y2,
                          Year == 3 ~ y3)) %>%
  group_by(Year, closure_category) %>%
  mutate(total_rate = sum(closure_rate),
         total_rate_rank = sum(closure_rate3),
         direction = case_when(is.na(direction) ~ "", TRUE ~ direction)) %>%
  ungroup() %>%
  select(Year, primary_secondary, closure_category, closure_rate, closure_rate_old, closure_percent, total_rate, total_rate_rank, direction)

dcrs_data_wranlge_ps$Year <- factor(dcrs_data_wranlge_ps$Year, levels=c("8", "9", "10"))

#Create flag if significant change is identified to use logical chart title script  
sig_flag_both <- dcrs_data_wranlge_ps %>%
    mutate(direction = paste(direction, collapse = ''),
           sig_flag = case_when(str_detect(direction , fixed("increase")) & str_detect(direction , fixed("decrease")) ~ 1, TRUE ~ 0)) %>%
    summarise(sig_flag = sum(sig_flag)) %>%
    pull(sig_flag)

sig_flag_up <- dcrs_data_wranlge_ps %>%
    mutate(direction = paste(direction, collapse = ''),
           sig_flag = case_when(str_detect(direction , fixed("increase"))  ~ 1, TRUE ~ 0)) %>%
    summarise(sig_flag = sum(sig_flag)) %>%
    pull(sig_flag)

sig_flag_down <- dcrs_data_wranlge_ps %>%
    mutate(direction = paste(direction, collapse = ''),
           sig_flag = case_when(str_detect(direction , fixed("decrease"))  ~ 1, TRUE ~ 0)) %>%
    summarise(sig_flag = sum(sig_flag)) %>%
    pull(sig_flag)

#Add caption to chart if significant change is identified
# update year references --------------------------------------------------
chart_lab <- if (sig_flag_both >= 1) {
  labs(title = paste0("Chart 5: Breakdown of clinical closure categories ‘not in order’ for years ", y1, ", ", y2, " and ", y3, " by \nprimary and secondary care NHS ", Board),   #set titles
       caption = "*^ significant increase from previous year \n *v significant decrease from previous year", #Caption if significant change is identified
       y = "",
       x = "Note: Year 8 - 2022/23     Year 9 - 2023/24     Year 10 - 2024/25")
} else if (sig_flag_up >= 1) { 
  labs(title = paste0("Chart 5: Breakdown of clinical closure categories ‘not in order’ for years ", y1, ", ", y2, " and ", y3, " by \nprimary and secondary care NHS ", Board),   #set titles
       caption = "*^ significant increase from previous year", #Caption if significant change is identified
       y = "",
       x = "Note: Year 8 - 2022/23     Year 9 - 2023/24     Year 10 - 2024/25")
} else if (sig_flag_down >= 1) {
  labs(title = paste0("Chart 5: Breakdown of clinical closure categories ‘not in order’ for years ", y1, ", ", y2, " and ", y3, " by \nprimary and secondary care NHS ", Board),   #set titles
       caption = "*v significant decrease from previous year", #Caption if significant change is identified
       y = "",
       x = "Note: Year 8 - 2022/23     Year 9 - 2023/24     Year 10 - 2024/25")
} else {
  labs(title = paste0("Chart 5: Breakdown of clinical closure categories ‘not in order’ for years ", y1, ", ", y2, " and ", y3, " by \nprimary and secondary care NHS ", Board),   #set titles
       y = "",
       x = "Note: Year 8 - 2022/23     Year 9 - 2023/24     Year 10 - 2024/25")
}

dcrs_closure_ps_plot <- ggplot(dcrs_data_wranlge_ps, aes(Year, closure_rate, fill = primary_secondary)) +
  geom_col(width = 0.96) +
  facet_wrap(~ fct_reorder(closure_category, total_rate_rank), strip.position = "bottom", nrow = 1) +
  scale_y_continuous(expand=c(0, 0), limits=c(0, max(dcrs_data_wranlge_ps$total_rate)+0.02),
                     labels = scales::percent_format(accuracy = 1)) +  #set percentage
  chart_lab +
  scale_fill_manual(values = c("#143965", "#8BB5E8")) +
  geom_text(aes(label=closure_percent), size = 2.5, colour = "white", position = position_stack(vjust = .5)) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(vjust = 0.5, hjust=0.2),
        strip.placement = "outside",
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.1,0.81),
        legend.box.background = element_rect(colour = "grey"),
        panel.background = element_blank(),
        plot.title = element_text(size = 10),
        axis.title = element_text(size = 8,face="italic", hjust = 0.02),
        strip.background = element_blank(),
        panel.spacing = unit(0, 'points'))

dcrs_closure_ps_plot

} else {dcrs_closure_ps_plot <- "not enough data"}  #create message if no chart is created to use place in the report

#ggsave(dcrs_closure_plot, 
#       filename = "Charts/dcrs_closure_plot.png",
#       device = "png",
#       height = 3.2, width = 6, units = "in")


# chart 6 death too vague -------------------------------------------------

##death too vague chart 6##

#extract current and previous year denominator to use in CI test
HB_n1 <- dcrs_data_report_all %>% select(Year, `Health Board`, cause_too_vague) %>% filter(Year == 2, `Health Board` == "Board") %>% pull(cause_too_vague)
HB_n2 <- dcrs_data_report_all %>% select(Year, `Health Board`, cause_too_vague) %>% filter(Year == 3, `Health Board` == "Board") %>% pull(cause_too_vague)
Scot_n1 <- dcrs_data_report_all %>% select(Year, `Health Board`, cause_too_vague) %>% filter(Year == 2, `Health Board` == "Scotland") %>% pull(cause_too_vague)
Scot_n2 <- dcrs_data_report_all %>% select(Year, `Health Board`, cause_too_vague) %>% filter(Year == 3, `Health Board` == "Scotland") %>% pull(cause_too_vague)

if (HB_n1 > 0 & HB_n2 > 0) {  #CI test only works when both denominators are greater than zero, otherwise error occurs 
#chart 6b data and significance test, wrangling into suitable layout
dcrs_data_wrangle_death_cause <- dcrs_data_report_all %>%
  select(Year, `Health Board`, cause_too_vague, histology, psite_metastatic_missing, pneumonia_subtype, dementia_subtype, microbiology, sepsis_source, diabetes_subtype, lifestyle_factor, stroke) %>%
  filter(Year == 2 | Year == 3) %>%
  pivot_longer(cols = !c(Year, `Health Board`),
               names_to = 'cause_too_vague', 
               values_to = 'cause_too_vague_num') %>%
  pivot_wider(names_from = Year, 
              values_from = 'cause_too_vague_num') %>% 
  mutate(denom1 = case_when(`Health Board` == "Board" ~ HB_n1,
                            `Health Board` == "Scotland" ~ Scot_n1),
         denom2 = case_when(`Health Board` == "Board" ~ HB_n2,
                            `Health Board` == "Scotland" ~ Scot_n2),
         cause_too_vague_rate = `3`/denom2,
         cause_too_vague_rate_old = `2`/denom1,
         ci_mn = BinomDiffCI(x1 = `2`, n1 = denom1, x2 = `3`, n2 = denom2, method="mn"), #Apply Miettinen-Nurminen CI Method for significance between 2 years
         ci_sig = case_when(denom1 >= 10 & denom2 >= 10 ~ (case_when(ci_mn[, "lwr.ci"] < 0 & ci_mn[, "upr.ci"] > 0 ~ "", TRUE ~ "*")), #Flag with * if significance is found (if sample is at least 20 cases)
                            TRUE ~ ""),
         direction = case_when(ci_sig == "*" & cause_too_vague_rate > cause_too_vague_rate_old ~ "increase",
                               ci_sig == "*" & cause_too_vague_rate < cause_too_vague_rate_old ~ "decrease",
                               TRUE ~ ""),
         ci_sig = case_when(ci_sig == "*" & cause_too_vague_rate > cause_too_vague_rate_old ~ "*^",
                            ci_sig == "*" & cause_too_vague_rate < cause_too_vague_rate_old ~ "*v",
                            TRUE ~ ""), 
         cause_too_vague_percent = paste0(percent(`3` / denom2, accuracy = 0.1),ci_sig),
         rank_cases = case_when(`Health Board` == 'Board' ~ min_rank(cause_too_vague_rate),
                                TRUE ~ 0)) %>%
  filter(cause_too_vague_rate != 0, cause_too_vague != 'cause_too_vague') %>%
  select(`Health Board`, cause_too_vague, cause_too_vague_rate, cause_too_vague_rate_old, cause_too_vague_percent, direction, ci_sig, rank_cases)

#Create flag if significant change is identified to use logical chart title script  
sig_flag_both <- sigboth(dcrs_data_wrangle_death_cause)
sig_flag_up <- sigup(dcrs_data_wrangle_death_cause)
sig_flag_down <- sigdown(dcrs_data_wrangle_death_cause)

#Add caption to chart if significant change is identified
chart_lab <- chart_titles("Chart 6: Breakdown of Cause of Death too vague",
                          sig_flag_both, sig_flag_up, sig_flag_down)

} else if (HB_n2 > 0 ) {  #create dataframe with no CI test if first condition is not met
  dcrs_data_wrangle_death_cause <- dcrs_data_report_all %>%
    select(Year, `Health Board`, cause_too_vague, histology, psite_metastatic_missing, pneumonia_subtype, dementia_subtype, microbiology, sepsis_source, diabetes_subtype, lifestyle_factor, stroke) %>%
    filter(Year == 2 | Year == 3) %>%
    pivot_longer(cols = !c(Year, `Health Board`),
                 names_to = 'cause_too_vague', 
                 values_to = 'cause_too_vague_num') %>%
    pivot_wider(names_from = Year, 
                values_from = 'cause_too_vague_num') %>% 
    mutate(denom2 = case_when(`Health Board` == "Board" ~ HB_n2,
                              `Health Board` == "Scotland" ~ Scot_n2),
           cause_too_vague_rate = `3`/denom2,
           cause_too_vague_percent = percent(`3` / denom2, accuracy = 0.1),
           rank_cases = case_when(`Health Board` == 'Board' ~ min_rank(cause_too_vague_rate),
                                  TRUE ~ 0)) %>%
    filter(cause_too_vague_rate != 0, cause_too_vague != 'cause_too_vague') %>%
    select(`Health Board`, cause_too_vague, cause_too_vague_rate, cause_too_vague_percent, rank_cases)
  
  #Add title and captions for chart
  chart_lab <-  labs(title = "Chart 6: Breakdown of Cause of Death too vague",   #set titles
                     subtitle = year3,
                     y = "",
                     x = "")  
  
}

if (HB_n2 > 0 ) {   #only create chart if there are cases for the current year
case_count <- dcrs_data_wrangle_death_cause %>%
  count(cause_too_vague)

#find how many categories had cases at the board
case_count_board <- dcrs_data_wrangle_death_cause %>%
  summarise(rank_cases = sum(rank_cases)) %>% pull(rank_cases)

if(case_count_board >= 1) {
#Match back with wrangled dataset and filter out any rows with no data
dcrs_data_wrangle_death_cause <- dcrs_data_wrangle_death_cause %>%
  left_join(case_count) %>%
  filter(n == 2) } else
  {dcrs_data_wrangle_death_cause}

#Create bar chart 6 in ranked order
dcrs_closure_plot_death <- ggplot(dcrs_data_wrangle_death_cause, aes(fct_reorder(cause_too_vague, rank_cases), 
                                                                       cause_too_vague_rate,  fill = `Health Board`)) +
  geom_bar(stat = "identity", position = 'dodge', width=0.8) + #set bar aesthetics
  scale_fill_manual(values = c("#8BB5E8", "#004578"), breaks=c('Scotland', 'Board'), name = "Location") +  #set bar aesthetics
  chart_lab +
  scale_x_discrete(labels = c("histology" = "Histology",   #rename variables
                              "psite_metastatic_missing" = "Primary site or metastatic site(s) missing", 
                              "pneumonia_subtype" = "Pneumonia sub-type",
                              "dementia_subtype" = "Dementia sub-type", 
                              "microbiology" = "Microbiology",
                              "sepsis_source" = "Source of sepsis",
                              "diabetes_subtype" = "Diabetes sub-type", 
                              "stroke" = "Stroke",
                              "lifestyle_factor_rate" = "Lifestyle factors (smoking, obesity, alcohol")) +                                   
  scale_y_continuous(expand=c(0, 0), limits=c(0, max(dcrs_data_wrangle_death_cause$cause_too_vague_rate)*1.25),
                     labels = scales::percent_format(accuracy = 1)) +  #set percentage                       #set dynamic axis limit
  geom_text(aes(label=cause_too_vague_percent), position = position_dodge(width = .9), hjust=-0.2, size = 3) +  #set labels
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),  #set simple theme
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        legend.title=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) + 
  coord_flip()     #flip chart on side


dcrs_closure_plot_death
#dcrs_data_wrangle__death_cause_2

ggsave(dcrs_closure_plot_death, 
       filename = "Charts/dcrs_closure_plot_death.png",
       device = "png",
       height = 3.2, width = 6, units = "in")
} else {dcrs_closure_plot_death <- "not enough data"}  #create message if no chart is created to use place in the report

# Chart 7 closure admin ---------------------------------------------------
##closure admin chart 7##

#extract current and previous year denominator to use in CI test
HB_n1 <- dcrs_data_report_all %>% select(Year, `Health Board`, closure_category_admin) %>% filter(Year == 2, `Health Board` == "Board") %>% pull(closure_category_admin)
HB_n2 <- dcrs_data_report_all %>% select(Year, `Health Board`, closure_category_admin) %>% filter(Year == 3, `Health Board` == "Board") %>% pull(closure_category_admin)
Scot_n1 <- dcrs_data_report_all %>% select(Year, `Health Board`, closure_category_admin) %>% filter(Year == 2, `Health Board` == "Scotland") %>% pull(closure_category_admin)
Scot_n2 <- dcrs_data_report_all %>% select(Year, `Health Board`, closure_category_admin) %>% filter(Year == 3, `Health Board` == "Scotland") %>% pull(closure_category_admin)

if (HB_n1 > 0 & HB_n2 >0) {  #CI test only works when both denominators are greater than zero, otherwise error occurs 
#chart 6b data and significance test, wrangling into suitable layout
dcrs_data_wrangle_admin_closure <- dcrs_data_report_all %>%
  select(Year, `Health Board`, closure_category_admin, spelling_error, deceased_details_incorrect,  doctors_details_incorrect, abbreviations_used, time_incorrect, incorrectly_complete, 
         attendance_incorrect, address_incorrect, pm_information_incorrect, legibility, consultant_incorrect, other_incorrect) %>%
  filter(Year == 2 | Year == 3) %>%
  pivot_longer(cols = !c(Year, `Health Board`),
               names_to = 'closure_category_admin', 
               values_to = 'closure_rate_admin') %>%
  pivot_wider(names_from = Year, 
              values_from = 'closure_rate_admin') %>% 
  mutate(denom1 = case_when(`Health Board` == "Board" ~ HB_n1,
                            `Health Board` == "Scotland" ~ Scot_n1),
         denom2 = case_when(`Health Board` == "Board" ~ HB_n2,
                            `Health Board` == "Scotland" ~ Scot_n2),
         admin_rate = `3`/denom2,
         admin_rate_old = `2`/denom1,
         ci_mn = BinomDiffCI(x1 = `2`, n1 = denom1, x2 = `3`, n2 = denom2, method="mn"), #Apply Miettinen-Nurminen CI Method for significance between 2 years
         ci_sig = case_when(denom1 >= 10 & denom2 >= 10 ~ (case_when(ci_mn[, "lwr.ci"] < 0 & ci_mn[, "upr.ci"] > 0 ~ "", TRUE ~ "*")), #Flag with * if significance is found (if sample is at least 20 cases)
                            TRUE ~ ""),
         direction = case_when(ci_sig == "*" & admin_rate > admin_rate_old ~ "increase",
                               ci_sig == "*" & admin_rate < admin_rate_old ~ "decrease",
                               TRUE ~ ""),
         ci_sig = case_when(ci_sig == "*" & admin_rate > admin_rate_old ~ "*^",
                            ci_sig == "*" & admin_rate < admin_rate_old ~ "*v",
                            TRUE ~ ""), 
         admin_percent = paste0(percent(`3` / denom2, accuracy = 0.1),ci_sig),
         rank_cases = case_when(`Health Board` == 'Board' ~ min_rank(admin_rate),
                                TRUE ~ 0)) %>%
  filter(admin_rate != 0, closure_category_admin != 'closure_category_admin') %>%
  select(`Health Board`, closure_category_admin, admin_rate, admin_rate_old, admin_percent, direction, ci_sig, rank_cases)

#Create flag if significant change is identified to use logical chart title script  
sig_flag_both <- sigboth(dcrs_data_wrangle_admin_closure)
sig_flag_up <- sigup(dcrs_data_wrangle_admin_closure)
sig_flag_down <- sigdown(dcrs_data_wrangle_admin_closure)

#Add caption to chart if significant change is identified
chart_lab <- chart_titles("Chart 7: Breakdown of administrative closure categories for ‘not in order’",
                          sig_flag_both, sig_flag_up, sig_flag_down)
} else if (HB_n2 > 0 ) {  #create dataframe with no CI test if first condition is not met
  dcrs_data_wrangle_admin_closure <- dcrs_data_report_all %>%
    select(Year, `Health Board`, closure_category_admin, spelling_error, deceased_details_incorrect,  doctors_details_incorrect, abbreviations_used, time_incorrect, incorrectly_complete, 
           attendance_incorrect, address_incorrect, pm_information_incorrect, legibility, consultant_incorrect, other_incorrect) %>%
    filter(Year == 2 | Year == 3) %>%
    pivot_longer(cols = !c(Year, `Health Board`),
                 names_to = 'closure_category_admin', 
                 values_to = 'closure_rate_admin') %>%
    pivot_wider(names_from = Year, 
                values_from = 'closure_rate_admin') %>% 
    mutate(denom2 = case_when(`Health Board` == "Board" ~ HB_n2,
                              `Health Board` == "Scotland" ~ Scot_n2),
           admin_rate = `3`/denom2,
           admin_percent = percent(`3` / denom2, accuracy = 0.1),
           rank_cases = case_when(`Health Board` == 'Board' ~ min_rank(admin_rate),
                                  TRUE ~ 0)) %>%
    filter(admin_rate != 0, closure_category_admin != 'closure_category_admin') %>%
    select(`Health Board`, closure_category_admin, admin_rate, admin_percent, rank_cases)  
  
  #Add title and captions for chart
  chart_lab <-  labs(title = "Chart 7: Breakdown of administrative closure categories for ‘not in order’",   #set titles
                     subtitle = year3,
                     y = "",
                     x = "")  
  
} else {dcrs_data_wrangle_admin_closure <- data.frame(hb = "Board", closure_category_admin = NA, admin_rate = NA, admin_percent = NA)%>%
  rename(`Health Board` = 'hb')}

if (HB_n2 > 0) {  #only create chart if there are cases for the current year
case_count <- dcrs_data_wrangle_admin_closure %>%
  count(closure_category_admin)

#find how many categories had cases at the board
case_count_board <- dcrs_data_wrangle_admin_closure %>%
  summarise(rank_cases = sum(rank_cases)) %>% pull(rank_cases)

if(case_count_board >= 1) {
  #Match back with wrangled dataset and filter out any rows with no data
  dcrs_data_wrangle_admin_closure <- dcrs_data_wrangle_admin_closure %>%
    left_join(case_count) %>%
    filter(n == 2) } else
    {dcrs_data_wrangle_admin_closure}


#Create bar chart 7 in ranked order
dcrs_closure_plot_admin <- ggplot(dcrs_data_wrangle_admin_closure, aes(fct_reorder(closure_category_admin, rank_cases), 
                                                                       admin_rate,  fill = `Health Board`)) +
  geom_bar(stat = "identity", position = 'dodge', width=0.8) + #set bar aesthetics
  scale_fill_manual(values = c("#8BB5E8", "#004578"), breaks=c('Scotland', 'Board'), name = "Location") +  #set bar aesthetics
  chart_lab +
  scale_x_discrete(labels = c("spelling_error" = "Certifying Doctor spelling error",   #rename variables
                              "deceased_details_incorrect" = "Deceased details incorrect", 
                              "doctors_details_incorrect" = "Certifying Doctor's details incorrect",
                              "abbreviations_used" = "Abbreviations used", 
                              "time_incorrect" = "Date or time of death incorrect",
                              "incorrectly_complete" = "Extra information (X box) incorrectly complete",
                              "attendance_incorrect" = "Attendance on the deceased incorrect", 
                              "address_incorrect" = "Place of death address incorrect",
                              "pm_information_incorrect" = "PM information incorrect",
                              "legibility" = "Legibility", 
                              "consultant_incorrect" = "Consultant's name incorrect",
                              "other_incorrect" = "Other additional information incorrect")) +
  scale_y_continuous(expand=c(0, 0), limits=c(0, max(dcrs_data_wrangle_admin_closure$admin_rate)*1.15),
                     labels = scales::percent_format(accuracy = 1)) +  #set percentage                        #set dynamic axis limit
  geom_text(aes(label=admin_percent), position = position_dodge(width = .9), hjust=-0.2, size = 2) +  #set labels
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),  #set simple theme
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        legend.title=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) + 
  coord_flip()     #flip chart on side

dcrs_closure_plot_admin
#dcrs_data_wrangle__two_closure

ggsave(dcrs_closure_plot_admin, 
       filename = "Charts/dcrs_closure_plot_admin.png",
       device = "png",
       height = 3.2, width = 6, units = "in")
} else { dcrs_closure_plot_admin <- "not enough data"}  #create message if no chart is created to use place in the report

# chart 8 PF breakdown ----------------------------------------------------
##PF breakdown chart 8##

#extract current and previous year denominator to use in CI test
HB_n1 <- dcrs_data_report_all %>% select(Year, `Health Board`, total_report_to_pf) %>% filter(Year == 2, `Health Board` == "Board") %>% pull(total_report_to_pf)
HB_n2 <- dcrs_data_report_all %>% select(Year, `Health Board`, total_report_to_pf) %>% filter(Year == 3, `Health Board` == "Board") %>% pull(total_report_to_pf)
Scot_n1 <- dcrs_data_report_all %>% select(Year, `Health Board`, total_report_to_pf) %>% filter(Year == 2, `Health Board` == "Scotland") %>% pull(total_report_to_pf)
Scot_n2 <- dcrs_data_report_all %>% select(Year, `Health Board`, total_report_to_pf) %>% filter(Year == 3, `Health Board` == "Scotland") %>% pull(total_report_to_pf)

if (HB_n1 > 0 & HB_n2 > 0 ) {  #CI test only works when both denominators are greater than zero, otherwise error occurs 
#chart 6b data and significance test, wrangling into suitable layout
  PF_data_wrangle <- dcrs_data_report_all %>%
    select(Year,`Health Board`, total_report_to_pf, choking, concerns_over_care, drug_related, neglect_exposure, 
           fracture_trauma, industrial_disease, infectious_disease, legal_order, flagged_error, other_report_pf) %>%
  filter(Year == 2 | Year == 3) %>%
  pivot_longer(cols = !c(Year, `Health Board`),
               names_to = 'total_report_to_pf', 
               values_to = 'total_report_to_pf_rate') %>%
  pivot_wider(names_from = Year, 
              values_from = 'total_report_to_pf_rate') %>% 
  mutate(denom1 = case_when(`Health Board` == "Board" ~ HB_n1,
                            `Health Board` == "Scotland" ~ Scot_n1),
         denom2 = case_when(`Health Board` == "Board" ~ HB_n2,
                            `Health Board` == "Scotland" ~ Scot_n2),
         pf_rate = `3`/denom2,
         pf_rate_old = `2`/denom1,
         ci_mn = BinomDiffCI(x1 = `2`, n1 = denom1, x2 = `3`, n2 = denom2, method="mn"), #Apply Miettinen-Nurminen CI Method for significance between 2 years
         ci_sig = case_when(denom1 >= 10 & denom2 >= 10 ~ (case_when(ci_mn[, "lwr.ci"] < 0 & ci_mn[, "upr.ci"] > 0 ~ "", TRUE ~ "*")), #Flag with * if significance is found (if sample is at least 20 cases)
                            TRUE ~ ""),
         direction = case_when(ci_sig == "*" & pf_rate > pf_rate_old ~ "increase",
                               ci_sig == "*" & pf_rate < pf_rate_old ~ "decrease",
                               TRUE ~ ""),
         ci_sig = case_when(ci_sig == "*" & pf_rate > pf_rate_old ~ "*^",
                            ci_sig == "*" & pf_rate < pf_rate_old ~ "*v",
                            TRUE ~ ""), 
         pf_percent = paste0(percent(`3` / denom2, accuracy = 0.1),ci_sig),
         rank_cases = case_when(`Health Board` == 'Board' ~ min_rank(pf_rate),
                                TRUE ~ 0)) %>%
  filter(pf_rate != 0, total_report_to_pf != 'total_report_to_pf') %>%
  select(`Health Board`, total_report_to_pf, pf_rate, pf_rate_old, pf_percent, direction, ci_sig, rank_cases)
  
  #Create flag if significant change is identified to use logical chart title script  
  sig_flag_both <- sigboth(PF_data_wrangle)
  sig_flag_up <- sigup(PF_data_wrangle)
  sig_flag_down <- sigdown(PF_data_wrangle)
  
  #Add caption to chart if significant change is identified
  chart_lab <- chart_titles("Chart 8: Breakdown of cases reported to Procurator Fiscal",
                            sig_flag_both, sig_flag_up, sig_flag_down)
} else if (HB_n2 >0) {  #create dataframe with no CI test if first condition is not met
  PF_data_wrangle <- dcrs_data_report_all %>%
    select(Year,`Health Board`, total_report_to_pf, choking, concerns_over_care, drug_related, neglect_exposure, 
           fracture_trauma, industrial_disease, infectious_disease, legal_order, flagged_error, other_report_pf) %>%
    filter(Year == 2 | Year == 3) %>%
    pivot_longer(cols = !c(Year, `Health Board`),
                 names_to = 'total_report_to_pf', 
                 values_to = 'total_report_to_pf_rate') %>%
    pivot_wider(names_from = Year, 
                values_from = 'total_report_to_pf_rate') %>% 
    mutate(denom2 = case_when(`Health Board` == "Board" ~ HB_n2,
                              `Health Board` == "Scotland" ~ Scot_n2),
           pf_rate = `3`/denom2,
           pf_percent = percent(`3` / denom2, accuracy = 0.1),
           rank_cases = case_when(`Health Board` == 'Board' ~ min_rank(pf_rate),
                                  TRUE ~ 0)) %>%
    filter(pf_rate != 0, total_report_to_pf != 'total_report_to_pf') %>%
    select(`Health Board`, total_report_to_pf, pf_rate, pf_percent, rank_cases)
  
  #Add title and captions for chart
  chart_lab <-  labs(title = "Chart 8: Breakdown of cases reported to Procurator Fiscal",   #set titles
       subtitle = year3,
       y = "",
       x = "")
} else { PF_data_wrangle <- data.frame(hb = "Board", total_report_to_pf = NA, pf_rate = NA, pf_percent = NA)%>%
  rename(`Health Board` = 'hb') }

if (HB_n2 >0) {  #only create chart if there are cases for the current year
#Find cases with no data to be filtered out
case_count <- PF_data_wrangle %>%
  count(total_report_to_pf)

#find how many categories had cases at the board
case_count_board <- PF_data_wrangle %>%
  summarise(rank_cases = sum(rank_cases)) %>% pull(rank_cases)

if(case_count_board >= 1) {
  #Match back with wrangled dataset and filter out any rows with no data
  PF_data_wrangle <- PF_data_wrangle %>%
    left_join(case_count) %>%
    filter(n == 2) } else
    {PF_data_wrangle}

#Create bar chart 8 in ranked order
dcrs_PF_plot <- ggplot(PF_data_wrangle, aes(fct_reorder(total_report_to_pf, rank_cases), 
                                              pf_rate,  fill = `Health Board`)) +
  geom_bar(stat = "identity", position = 'dodge', width=0.8) + #set bar aesthetics
  scale_fill_manual(values = c("#8BB5E8", "#004578"), breaks=c('Scotland', 'Board'), name = "Location") +
  chart_lab +
  scale_x_discrete(labels = c("choking" = "Choking",   #rename variables
                              "concerns_over_care" = "Concerns Over Care",
                              "drug_related" = "Drug Related", 
                              "neglect_exposure" = "Neglect or Exposure",
                              "fracture_trauma" = "Fracture or Trauma",
                              "industrial_disease" = "Industrial Disease",
                              "infectious_disease" = "Infectious Disease",
                              "legal_order" = "Legal Order",
                              "flagged_error" = "Flagged in Error",
                              "other_report_pf" = "Other Report to PF")) +
  scale_y_continuous(expand=c(0, 0), limits=c(0, max(PF_data_wrangle$pf_rate)*1.15),
                     labels = scales::percent_format(accuracy = 1)) +  #set percentage                      #set dynamic axis limit
  geom_text(aes(label=pf_percent), position = position_dodge(width = .9), hjust=-0.2, size = 3) +  #set labels
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),  #set simple theme
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        legend.title=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) + 
  coord_flip()     #flip chart on side

#If no data avialble for the board provide message instead of chart    

dcrs_PF_plot
#dcrs_data_wrangle_enquiry

ggsave(dcrs_PF_plot, 
       filename = "Charts/dcrs_PF_plot.png",
       device = "png",
       height = 3.2, width = 6, units = "in")

} else {dcrs_PF_plot <- "Not enough data"}  #create message if no chart is created to use place in the report

# chart 9 enquiry line ----------------------------------------------------
##enquiry line chart 9##

#extract current and previous year denominator to use in CI test

HB_n1 <- dcrs_data_enquiry_all %>% select(Year, `Health Board`, enquiry_category_total) %>% filter(Year == 2, `Health Board` == "Board") %>% pull(enquiry_category_total)
HB_n2 <- dcrs_data_enquiry_all %>% select(Year, `Health Board`, enquiry_category_total) %>% filter(Year == 3, `Health Board` == "Board") %>% pull(enquiry_category_total)
Scot_n1 <- dcrs_data_enquiry_all %>% select(Year, `Health Board`, enquiry_category_total) %>% filter(Year == 2, `Health Board` == "Scotland") %>% pull(enquiry_category_total)
Scot_n2 <- dcrs_data_enquiry_all %>% select(Year, `Health Board`, enquiry_category_total) %>% filter(Year == 3, `Health Board` == "Scotland") %>% pull(enquiry_category_total)

if (HB_n1 > 0 & HB_n2 > 0 ) {  #CI test only works when both denominators are greater than zero, otherwise error occurs 
#chart 9 data and significance test, wrangling into suitable layout
enquiry_data_wrangle <- dcrs_data_enquiry_all %>%
  select(Year, `Health Board`, enquiry_category_total, gp_clinical_advice, gp_process_advice, hospital_clinical_advice, 
         hospital_process_advice, hospice_clinical_advice, hospice_process_advice, funeral_director, informant_or_family, 
         procurator_fiscal, signposted, other) %>%
    filter(Year == 2 | Year == 3) %>%
    pivot_longer(cols = !c(Year, `Health Board`),
                 names_to = 'enquiry_category_total', 
                 values_to = 'enquiry_rate') %>%
    pivot_wider(names_from = Year, 
                values_from = 'enquiry_rate') %>% 
    mutate(denom1 = case_when(`Health Board` == "Board" ~ HB_n1,
                              `Health Board` == "Scotland" ~ Scot_n1),
           denom2 = case_when(`Health Board` == "Board" ~ HB_n2,
                              `Health Board` == "Scotland" ~ Scot_n2),
           enquiry_rate = `3`/denom2,
           enquiry_rate_old = `2`/denom1,
           ci_mn = BinomDiffCI(x1 = `2`, n1 = denom1, x2 = `3`, n2 = denom2, method="mn"), #Apply Miettinen-Nurminen CI Method for significance between 2 years
           ci_sig = case_when(denom1 >= 10 & denom2 >= 10 ~ (case_when(ci_mn[, "lwr.ci"] < 0 & ci_mn[, "upr.ci"] > 0 ~ "", TRUE ~ "*")), #Flag with * if significance is found (if sample is at least 20 cases)
                              TRUE ~ ""),
           direction = case_when(ci_sig == "*" & enquiry_rate > enquiry_rate_old ~ "increase",
                                 ci_sig == "*" & enquiry_rate < enquiry_rate_old ~ "decrease",
                                 TRUE ~ ""),
           ci_sig = case_when(ci_sig == "*" & enquiry_rate > enquiry_rate_old ~ "*^",
                              ci_sig == "*" & enquiry_rate < enquiry_rate_old ~ "*v",
                              TRUE ~ ""), 
           enquiry_percent = paste0(percent(`3` / denom2, accuracy = 0.1),ci_sig),
           rank_cases = case_when(`Health Board` == 'Board' ~ min_rank(enquiry_rate),
                                  TRUE ~ 0)) %>%
    filter(enquiry_rate != 0, enquiry_category_total != 'enquiry_category_total') %>%
    select(`Health Board`, enquiry_category_total, enquiry_rate, enquiry_rate_old, enquiry_percent, direction, ci_sig, rank_cases)
  
#Create flag if significant change is identified to use logical chart title script  
sig_flag_both <- sigboth(enquiry_data_wrangle)
sig_flag_up <- sigup(enquiry_data_wrangle)
sig_flag_down <- sigdown(enquiry_data_wrangle)

#Add caption to chart if significant change is identified
chart_lab <- chart_titles("Chart 9: Enquiry line calls - Type of advice required",
                          sig_flag_both, sig_flag_up, sig_flag_down)

} else if (HB_n2 > 0) {  #create dataframe with no CI test if first condition is not met
  enquiry_data_wrangle <- dcrs_data_enquiry_all %>%
    select(Year, `Health Board`, enquiry_category_total, gp_clinical_advice, gp_process_advice, hospital_clinical_advice, 
           hospital_process_advice, hospice_clinical_advice, hospice_process_advice, funeral_director, informant_or_family, 
           procurator_fiscal, signposted, other) %>%
    filter(Year == 2 | Year == 3) %>%
    pivot_longer(cols = !c(Year, `Health Board`),
                 names_to = 'enquiry_category_total', 
                 values_to = 'enquiry_rate') %>%
    pivot_wider(names_from = Year, 
                values_from = 'enquiry_rate') %>% 
    mutate(denom2 = case_when(`Health Board` == "Board" ~ HB_n2,
                              `Health Board` == "Scotland" ~ Scot_n2),
           enquiry_rate = `3`/denom2,
           enquiry_percent = percent(`3` / denom2, accuracy = 0.1),
           rank_cases = case_when(`Health Board` == 'Board' ~ min_rank(enquiry_rate),
                                  TRUE ~ 0)) %>%
    filter(enquiry_rate != 0, enquiry_category_total != 'enquiry_category_total') %>%
    select(`Health Board`, enquiry_category_total, enquiry_rate, enquiry_percent, rank_cases)  
  
  #Add title and captions for chart
  chart_lab <-  labs(title = "Chart 9: Enquiry line calls - Type of advice required",   #set titles
                     subtitle = year3,
                     y = "",
                     x = "")  
} 

if (HB_n2 > 0) {  #only create chart if there are cases for the current year

#Find cases with no data to be filtered out
case_count <- enquiry_data_wrangle %>%
    count(enquiry_category_total)

#find how many categories had cases at the board
case_count_board <- enquiry_data_wrangle %>%
  summarise(rank_cases = sum(rank_cases)) %>% pull(rank_cases)

if(case_count_board >= 1) {
  #Match back with wrangled dataset and filter out any rows with no data
  enquiry_data_wrangle <- enquiry_data_wrangle %>%
    left_join(case_count) %>%
    filter(n == 2) } else
    {enquiry_data_wrangle}

#Create bar chart 9 in ranked order
dcrs_enquiry_plot <- ggplot(enquiry_data_wrangle, aes(fct_reorder(enquiry_category_total, rank_cases), 
                                                          enquiry_rate,  fill = `Health Board`)) +
    geom_bar(stat = "identity", position = 'dodge', width=0.8) + #set bar aesthetics
    scale_fill_manual(values = c("#8BB5E8", "#004578"), breaks=c('Scotland', 'Board'), name = "Location") +  #set bar aesthetics
    chart_lab +
    scale_x_discrete(labels = c("gp_clinical_advice" = "GP Clinical Advice",   #rename variables
                                "gp_process_advice" = "GP Process Advice",
                                "hospital_clinical_advice" = "Hospital Clinical Advice", 
                                "hospital_process_advice" = "Hospital Process Advice",
                                "hospice_clinical_advice" = "Hospice Clinical Advice",
                                "hospice_process_advice" = "Hospice Process Advice",
                                "funeral_director" = "Funeral Director",
                                "informant_or_family" = "Informant or Family",
                                "procurator_fiscal" = "Procurator Fiscal",
                                "signposted" = "Signposted", 
                                "other" = "Other")) +
    scale_y_continuous(expand=c(0, 0), limits=c(0, max(enquiry_data_wrangle$enquiry_rate)*1.15),
                       labels = scales::percent_format(accuracy = 1)) +  #set percentage                     #set dynamic axis limit
    geom_text(aes(label=enquiry_percent), position = position_dodge(width = .9), hjust=-0.2, size = 2.5) +  #set labels
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),  #set simple theme
          axis.text = element_text(size = 8),
          axis.title = element_text(size = 8),
          legend.title=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) + 
    coord_flip()     #flip chart on side


dcrs_enquiry_plot
#dcrs_data_wrangle_enquiry

ggsave(dcrs_enquiry_plot, 
       filename = "Charts/dcrs_enquiry_plot.png",
       device = "png",
       height = 3.2, width = 6, units = "in")
} else {dcrs_enquiry_plot <- "not enough data"}  #create message if no chart is created to use place in the report


# chart 10 hospital review ------------------------------------------------
##set hospital chart review data chart 10 - Done in 3 parts for each year then combined##

#First year data wrangle
DCRS_Data_Hosp_chart <- DCRS_DATA_Loc_DIAG %>%
  filter(Health_Board == Board,
         instgrp2 == "NHS Hospital",
         YEAR == paste0("Year ", y1) | YEAR == paste0("Year ", y2) | YEAR == paste0("Year ", y3)) %>%
  group_by(YEAR, Locname, Review) %>%
  summarise(cases = n()) %>% ungroup() %>%
  mutate(YEAR = case_when(YEAR == paste0("Year ", y1) ~ "Year1",
                          YEAR == paste0("Year ", y2) ~ "Year2",
                          YEAR == paste0("Year ", y3) ~ "Year3"))

hosp_check <- DCRS_Data_Hosp_chart %>% filter(cases >= 10)

if (nrow(hosp_check != 0)) {  #CI test only works when denominators are greater than zero, otherwise error occurs 
DCRS_Data_Hosp_chart_sig <- DCRS_Data_Hosp_chart %>%
  pivot_wider(names_from = YEAR, 
              values_from = 'cases') %>%
  group_by(Review) %>%
  mutate(ranking = rank(desc(Year3)),
         ranking = case_when(ranking > 3 ~ 4,  #Find top 3 hospitals and set all others as "other" (request from DCRS)
                             TRUE ~ ranking)) %>%
  filter(ranking <= 3) %>%
  ungroup() %>%
  group_by(Locname) %>%
  mutate(Year1 = case_when(is.na(Year1) ~ 0, TRUE ~ Year1),
         Year2 = case_when(is.na(Year2) ~ 0, TRUE ~ Year2),
         Year3 = case_when(is.na(Year3) ~ 0, TRUE ~ Year3),
         total_Year1 = sum(Year1),
         total_Year2 = sum(Year2),
         total_Year3 = sum(Year3)) %>%
  filter(total_Year2 >= 10 | total_Year3 >= 10) %>%
  ungroup() %>%
  mutate(case_rate1 = Year1/total_Year1,
         case_rate2 = Year2/total_Year2,
         case_rate3 = Year3/total_Year3,
         ci_mn1 = BinomDiffCI(x1 = Year1, n1 = total_Year1, x2 = Year2, n2 = total_Year2, method="mn"), #Apply Miettinen-Nurminen CI Method for significance between 2 years
         ci_mn2 = BinomDiffCI(x1 = Year2, n1 = total_Year2, x2 = Year3, n2 = total_Year3, method="mn"), 
         ci_sig1 = case_when(total_Year1 >= 10 & total_Year2 >= 10 ~ (case_when(ci_mn1[, "lwr.ci"] < 0 & ci_mn1[, "upr.ci"] > 0 ~ "", TRUE ~ "*")), #Flag with * if significance is found (if sample is at least 20 cases)
                             TRUE ~ ""),
         ci_sig2 = case_when(total_Year2 >= 10 & total_Year3 >= 10 ~ (case_when(ci_mn2[, "lwr.ci"] < 0 & ci_mn2[, "upr.ci"] > 0 ~ "", TRUE ~ "*")), #Flag with * if significance is found (if sample is at least 20 cases)
                             TRUE ~ ""),
         direction1 = case_when(ci_sig1 == "*" & case_rate2 > case_rate1 ~ "increase",
                                ci_sig1 == "*" & case_rate2 < case_rate1 ~ "decrease",
                                TRUE ~ ""),       
         ci_sig1 = case_when(ci_sig1 == "*" & case_rate2 > case_rate1 ~ "*^",
                             ci_sig1 == "*" & case_rate2 < case_rate1 ~ "*v",
                             TRUE ~ ""), 
         direction2 = case_when(ci_sig2 == "*" & case_rate3 > case_rate2 ~ "increase",
                                ci_sig2 == "*" & case_rate3 < case_rate2 ~ "decrease",
                                TRUE ~ ""),       
         ci_sig2 = case_when(ci_sig2 == "*" & case_rate3 > case_rate2 ~ "*^",
                             ci_sig2 == "*" & case_rate3 < case_rate2 ~ "*v",
                             TRUE ~ "")) %>%
  select(Locname, Review, case_rate1, case_rate2, case_rate3, ci_sig1, ci_sig2, direction1, direction2)
} else {DCRS_Data_Hosp_chart_sig <- data.frame(Locname = Board, Review = "", case_rate1 = NA, case_rate2 = NA, case_rate3 = NA,   #create dummy dataframe with no CI test if first condition is not met
                                               ci_sig1 = "", ci_sig2 = "", direction1 = "", direction2 = "")}

  
#Get ranking for latest year
hosp_ranking <- DCRS_Data_Hosp_chart %>%
  filter(YEAR == "Year3") %>%
  group_by(Locname) %>%
  summarise(case_total = sum(cases)) %>%
  ungroup() %>%
  mutate(ranking = rank(desc(case_total)),
         ranking = case_when(ranking > 3 ~ 4,  #Find top 3 hospitals and set all others as "other" (request from DCRS)
                             case_total < 10 ~ 4, #suppress any hospitals with a total of less than 10 (request from DCRS)
                             TRUE ~ ranking)) %>%
  arrange(desc(case_total)) %>% #Set order from highest to lowest ranked
  select(Locname, ranking)

#Attach rankings for latest year, aggregate figures naming top 3 and "other", and get percentages
DCRS_Data_Hosp_chart2 <-DCRS_Data_Hosp_chart %>%
  left_join(DCRS_Data_Hosp_chart_sig, by = c("Locname", "Review")) %>%
  left_join(hosp_ranking, by = "Locname") %>%
  mutate(ranking = case_when(is.na(ranking) ~ 4,  #Find top 3 hospitals and set all others as "other"
                             TRUE ~ ranking),
         Locname = case_when(ranking > 3 | is.na(ranking) ~ "Other",
                             TRUE ~ Locname)) %>%
  group_by(YEAR, Locname, Review) %>%   #Aggregate all hospitals identified as "other"
  summarise(cases = sum(cases)) %>%
  group_by(YEAR, Locname) %>%
  mutate(case_total = sum(cases)) %>%
  left_join(hosp_ranking, by = "Locname") %>%
  left_join(DCRS_Data_Hosp_chart_sig, by = c("Locname", "Review")) %>%
  ungroup() %>%
  mutate(ranking = case_when(is.na(ranking) ~ 4, TRUE ~ ranking),
         ci_sig1 = case_when(is.na(ci_sig1) ~ "", TRUE ~ ci_sig1),
         ci_sig2 = case_when(is.na(ci_sig2) ~ "", TRUE ~ ci_sig2),
         direction1 = case_when(is.na(direction1) ~ "", TRUE ~ direction1),
         direction2 = case_when(is.na(direction2) ~ "", TRUE ~ direction2),
         case_rate = cases/case_total,
         label_flag = case_when(cases/max(cases) >= 0.07 ~ 1, TRUE ~ 0), #Only show label if more than 7% otherwise the label is too compressed
         case_percentage = case_when(YEAR == "Year2" & ci_sig1 != "" ~ paste0(percent(case_rate, accuracy = 1), ci_sig1),
                                     YEAR == "Year3" & ci_sig2 != "" ~ paste0(percent(case_rate, accuracy = 1), ci_sig2),
                                     label_flag == 1 ~ percent(case_rate, accuracy = 1),
                                     TRUE ~ ""),
         YEAR = case_when(YEAR == "Year1" ~ y1, YEAR == "Year2" ~ y2, YEAR == "Year3" ~ y3),
         Review = factor(Review, levels=c("Reported to PF", "Case not in Order", "Case in Order")),
         Locname = str_wrap(Locname , width = 18))

DCRS_Data_Hosp_chart2$YEAR <- factor(DCRS_Data_Hosp_chart2$YEAR, levels=c("8", "9", "10"))

#Create flag if significant change is identified to use logical chart title script  
sig_flag_both <- DCRS_Data_Hosp_chart2 %>%
  mutate(direction = paste(direction1, direction2),
         direction = paste(direction, collapse = ''),
         sig_flag = case_when(str_detect(direction , fixed("increase")) & str_detect(direction , fixed("decrease")) ~ 1, TRUE ~ 0)) %>%
  summarise(sig_flag = sum(sig_flag)) %>%
  pull(sig_flag)

sig_flag_up <- DCRS_Data_Hosp_chart2 %>%
  mutate(direction = paste(direction1, direction2),
         direction = paste(direction, collapse = ''),
         sig_flag = case_when(str_detect(direction , fixed("increase"))  ~ 1, TRUE ~ 0)) %>%
  summarise(sig_flag = sum(sig_flag)) %>%
  pull(sig_flag)

sig_flag_down <- DCRS_Data_Hosp_chart2 %>%
  mutate(direction = paste(direction1, direction2),
         direction = paste(direction, collapse = ''),
         sig_flag = case_when(str_detect(direction , fixed("decrease"))  ~ 1, TRUE ~ 0)) %>%
  summarise(sig_flag = sum(sig_flag)) %>%
  pull(sig_flag)

#Add caption to chart if significant change is identified
# update year references --------------------------------------------------
chart_lab <- if (sig_flag_both >= 1) {
  labs(title = paste0("Chart 10: Number of standard reviews by hospital and closure category for years ", y1, ", ", y2, "\nand ",y3),   #set titles
       caption = "*^ significant increase from previous year \n *v significant decrease from previous year", #Caption if significant change is identified
       y = "",
       x = "Note: Year 8 - 2022/23     Year 9 - 2023/24     Year 10 - 2024/25")
} else if (sig_flag_up >= 1) { 
  labs(title = paste0("Chart 10: Number of standard reviews by hospital and closure category for years ", y1, ", ", y2, "\nand ",y3),   #set titles
       caption = "*^ significant increase from previous year", #Caption if significant change is identified
       y = "",
       x = "Note: Year 8 - 2022/23     Year 9 - 2023/24     Year 10 - 2024/25")
} else if (sig_flag_down >= 1) {
  labs(title = paste0("Chart 10: Number of standard reviews by hospital and closure category for years ", y1, ", ", y2, "\nand ",y3),   #set titles
       caption = "*v significant decrease from previous year", #Caption if significant change is identified
       y = "",
       x = "Note: Year 8 - 2022/23     Year 9 - 2023/24     Year 10 - 2024/25")
} else {
  labs(title = paste0("Chart 10: Number of standard reviews by hospital and closure category for years ", y1, ", ", y2, "\nand ",y3),   #set titles
       y = "",
       x = "Note: Year 8 - 2022/23     Year 9 - 2023/24     Year 10 - 2024/25")
}

#find values to set bettwe y-axis
max_value <- DCRS_Data_Hosp_chart2 %>% summarise(max_value = max(case_total)) %>% pull(max_value)

##create hospital bar chart 10##
hosp_review_chart <- ggplot(DCRS_Data_Hosp_chart2, aes(YEAR, cases, fill=Review)) + 
  geom_col(width = 0.97) +
  facet_wrap(~ fct_reorder(Locname, ranking), strip.position = "bottom", nrow = 1) +
  scale_x_discrete(labels = function(Review) str_wrap(Review, width = 10)) + 
  chart_lab +
  # scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("#097480", "#8BB5E8", "#0F3D6B")) +
  scale_y_continuous(expand=c(0, 0), limits=c(0, max_value+0.07)) +
  geom_text(aes(label=case_percentage), size = 2.5, check_overlap = FALSE, position = position_stack(vjust = 0.5), colour = "white") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),  #set simple theme
        axis.text = element_text(size = 8),
        strip.placement = "outside",
        strip.text = element_text(size = 8),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        legend.title = element_blank(),
        legend.position="bottom",
        legend.box.background = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 10),
        axis.title = element_text(size = 8,face="italic", hjust = 0.01),
        strip.background = element_blank(),
        panel.spacing = unit(0, 'points')) 

hosp_review_chart

ggsave(hosp_review_chart, 
       filename = "Charts/hosp_review_chart.png",
       device = "png",
       height = 3.2, width = 6, units = "in")

# chart 11 hospice review -------------------------------------------------

##set hospice chart review data chart 11 

#First year data wrangle
DCRS_Data_Hospice_chart <- DCRS_DATA_Loc_DIAG %>%
  filter(Health_Board == Board,
         instgrp2 == "Hospice",
         YEAR == paste0("Year ", y1) | YEAR == paste0("Year ", y2) | YEAR == paste0("Year ", y3)) %>%
  group_by(YEAR, Locname, Review) %>%
  summarise(cases = n()) %>% ungroup() %>%
  mutate(YEAR = case_when(YEAR == paste0("Year ", y1) ~ "Year1",
                          YEAR == paste0("Year ", y2) ~ "Year2",
                          YEAR == paste0("Year ", y3) ~ "Year3"))

hospice_check <- DCRS_Data_Hospice_chart %>% filter(cases >= 10)

if (nrow(hospice_check != 0)) {  #CI test only works when denominators are greater than zero, otherwise error occurs 
  DCRS_Data_Hospice_chart_sig <- DCRS_Data_Hospice_chart %>%
    pivot_wider(names_from = YEAR, 
                values_from = 'cases') %>%
    group_by(Review) %>%
    mutate(ranking = rank(desc(Year3)),
           ranking = case_when(ranking > 3 ~ 4,  #Find top 3 hospitals and set all others as "other" (request from DCRS)
                               TRUE ~ ranking)) %>%
    filter(ranking <= 3) %>%
    ungroup() %>%
    group_by(Locname) %>%
    mutate(Year1 = case_when(is.na(Year1) ~ 0, TRUE ~ Year1),
           Year2 = case_when(is.na(Year2) ~ 0, TRUE ~ Year2),
           Year3 = case_when(is.na(Year3) ~ 0, TRUE ~ Year3),
           total_Year1 = sum(Year1),
           total_Year2 = sum(Year2),
           total_Year3 = sum(Year3)) %>%
    filter(total_Year2 >= 10 | total_Year3 >= 10) %>%
    ungroup() %>%
    mutate(case_rate1 = Year1/total_Year1,
           case_rate2 = Year2/total_Year2,
           case_rate3 = Year3/total_Year3,
           ci_mn1 = BinomDiffCI(x1 = Year1, n1 = total_Year1, x2 = Year2, n2 = total_Year2, method="mn"), #Apply Miettinen-Nurminen CI Method for significance between 2 years
           ci_mn2 = BinomDiffCI(x1 = Year2, n1 = total_Year2, x2 = Year3, n2 = total_Year3, method="mn"), 
           ci_sig1 = case_when(total_Year1 >= 10 & total_Year2 >= 10 ~ (case_when(ci_mn1[, "lwr.ci"] < 0 & ci_mn1[, "upr.ci"] > 0 ~ "", TRUE ~ "*")), #Flag with * if significance is found (if sample is at least 20 cases)
                               TRUE ~ ""),
           ci_sig2 = case_when(total_Year2 >= 10 & total_Year3 >= 10 ~ (case_when(ci_mn2[, "lwr.ci"] < 0 & ci_mn2[, "upr.ci"] > 0 ~ "", TRUE ~ "*")), #Flag with * if significance is found (if sample is at least 20 cases)
                               TRUE ~ ""),
           direction1 = case_when(ci_sig1 == "*" & case_rate2 > case_rate1 ~ "increase",
                                  ci_sig1 == "*" & case_rate2 < case_rate1 ~ "decrease",
                                  TRUE ~ ""),       
           ci_sig1 = case_when(ci_sig1 == "*" & case_rate2 > case_rate1 ~ "*^",
                               ci_sig1 == "*" & case_rate2 < case_rate1 ~ "*v",
                               TRUE ~ ""), 
           direction2 = case_when(ci_sig2 == "*" & case_rate3 > case_rate2 ~ "increase",
                                  ci_sig2 == "*" & case_rate3 < case_rate2 ~ "decrease",
                                  TRUE ~ ""),       
           ci_sig2 = case_when(ci_sig2 == "*" & case_rate3 > case_rate2 ~ "*^",
                               ci_sig2 == "*" & case_rate3 < case_rate2 ~ "*v",
                               TRUE ~ "")) %>%
    select(Locname, Review, case_rate1, case_rate2, case_rate3, ci_sig1, ci_sig2, direction1, direction2)
} else {DCRS_Data_Hospice_chart_sig <- data.frame(Locname = Board, Review = "", case_rate1 = NA, case_rate2 = NA, case_rate3 = NA,  #create dummy dataframe with no CI test if first condition is not met
                                               ci_sig1 = "", ci_sig2 = "", direction1 = "", direction2 = "")}


#Get ranking for latest year
hospice_ranking <- DCRS_Data_Hospice_chart %>%
  filter(YEAR == "Year3") %>%
  group_by(Locname) %>%
  summarise(case_total = sum(cases)) %>%
  ungroup() %>%
  mutate(ranking = rank(desc(case_total)),
         ranking = case_when(ranking > 3 ~ 4,  #Find top 3 hospitals and set all others as "other" (request from DCRS)
                             TRUE ~ ranking)) %>%
  arrange(desc(case_total)) %>% #Set order from highest to lowest ranked
  select(Locname, ranking)

#Attach rankings for latest year, aggregate figures naming top 3 and "other", and get percentages
DCRS_Data_Hospice_chart2 <-DCRS_Data_Hospice_chart %>%
  left_join(DCRS_Data_Hospice_chart_sig, by = c("Locname", "Review")) %>%
  left_join(hospice_ranking, by = "Locname") %>%
  mutate(ranking = case_when(is.na(ranking) ~ 4,  #Find top 3 hospitals and set all others as "other"
                             TRUE ~ ranking),
         Locname = case_when(ranking > 3 | is.na(ranking) ~ "Other",
                             TRUE ~ Locname)) %>%
  group_by(YEAR, Locname, Review) %>%   #Aggregate all hospitals identified as "other"
  summarise(cases = sum(cases)) %>%
  group_by(YEAR, Locname) %>%
  mutate(case_total = sum(cases)) %>%
  left_join(hospice_ranking, by = "Locname") %>%
  left_join(DCRS_Data_Hospice_chart_sig, by = c("Locname", "Review")) %>%
  ungroup() %>%
  mutate(ranking = case_when(is.na(ranking) ~ 4, TRUE ~ ranking),
         ci_sig1 = case_when(is.na(ci_sig1) ~ "", TRUE ~ ci_sig1),
         ci_sig2 = case_when(is.na(ci_sig2) ~ "", TRUE ~ ci_sig2),
         direction1 = case_when(is.na(direction1) ~ "", TRUE ~ direction1),
         direction2 = case_when(is.na(direction2) ~ "", TRUE ~ direction2),
         case_rate = cases/case_total,
         label_flag = case_when(cases/max(cases) >= 0.07 ~ 1, TRUE ~ 0), #Only show label if more than 7% otherwise the label is too compressed
         case_percentage = case_when(YEAR == "Year2" & ci_sig1 != "" ~ paste0(percent(case_rate, accuracy = 1), ci_sig1),
                                     YEAR == "Year3" & ci_sig2 != "" ~ paste0(percent(case_rate, accuracy = 1), ci_sig2),
                                     label_flag == 1 ~ percent(case_rate, accuracy = 1),
                                     TRUE ~ ""),
         YEAR = case_when(YEAR == "Year1" ~ y1, YEAR == "Year2" ~ y2, YEAR == "Year3" ~ y3),
         Review = factor(Review, levels=c("Reported to PF", "Case not in Order", "Case in Order")),
         Locname = str_wrap(Locname , width = 18))

DCRS_Data_Hospice_chart2$YEAR <- factor(DCRS_Data_Hospice_chart2$YEAR, levels=c("8", "9", "10"))

#pull max cases value to set y-axis limit in chart
max_value <- DCRS_Data_Hospice_chart2 %>% summarise(max_value = max(case_total)) %>% pull(max_value)

#Create flag if significant change is identified to use logical chart title script  
sig_flag_both <- DCRS_Data_Hospice_chart2 %>%
  mutate(direction = paste(direction1, direction2),
         direction = paste(direction, collapse = ''),
         sig_flag = case_when(str_detect(direction , fixed("increase")) & str_detect(direction , fixed("decrease")) ~ 1, TRUE ~ 0)) %>%
  summarise(sig_flag = sum(sig_flag)) %>%
  pull(sig_flag)

sig_flag_up <- DCRS_Data_Hospice_chart2 %>%
  mutate(direction = paste(direction1, direction2),
         direction = paste(direction, collapse = ''),
         sig_flag = case_when(str_detect(direction , fixed("increase"))  ~ 1, TRUE ~ 0)) %>%
  summarise(sig_flag = sum(sig_flag)) %>%
  pull(sig_flag)

sig_flag_down <- DCRS_Data_Hospice_chart2 %>%
  mutate(direction = paste(direction1, direction2),
         direction = paste(direction, collapse = ''),
         sig_flag = case_when(str_detect(direction , fixed("decrease"))  ~ 1, TRUE ~ 0)) %>%
  summarise(sig_flag = sum(sig_flag)) %>%
  pull(sig_flag)

#Add caption to chart if significant change is identified
# update year references --------------------------------------------------
chart_lab <- if (sig_flag_both >= 1) {
  labs(title = paste0("Chart 11: Number of standard reviews by hospice and closure category for years ", y1, ", ", y2, "\nand ", y3),   #set titles
       caption = "*^ significant increase from previous year \n *v significant decrease from previous year", #Caption if significant change is identified
       y = "",
       x = "Note: Year 8 - 2022/23     Year 9 - 2023/24     Year 10 - 2024/25")
} else if (sig_flag_up >= 1) { 
  labs(title = paste0("Chart 11: Number of standard reviews by hospice and closure category for years ", y1, ", ", y2, "\nand ", y3),   #set titles
       caption = "*^ significant increase from previous year", #Caption if significant change is identified
       y = "",
       x = "Note: Year 8 - 2022/23     Year 9 - 2023/24     Year 10 - 2024/25")
} else if (sig_flag_down >= 1) {
  labs(title = paste0("Chart 11: Number of standard reviews by hospice and closure category for years ", y1, ", ", y2, "\nand ", y3),   #set titles
       caption = "*v significant decrease from previous year", #Caption if significant change is identified
       y = "",
       x = "Note: Year 8 - 2022/23     Year 9 - 2023/24     Year 10 - 2024/25")
} else {
  labs(title = paste0("Chart 11: Number of standard reviews by hospice and closure category for years ", y1, ", ", y2, "\nand ", y3),   #set titles
       y = "",
       x = "Note: Year 8 - 2022/23     Year 9 - 2023/24     Year 10 - 2024/25")
}


if (nrow(DCRS_Data_Hospice_chart2 != 0)) { #only create chart if there are cases for the current year

##create hospice bar chart 11##
hospice_review_chart <- ggplot(DCRS_Data_Hospice_chart2, aes(YEAR, cases, fill=Review)) + 
  geom_col(width = 0.97) +
  facet_wrap(~ fct_reorder(Locname, ranking), strip.position = "bottom", nrow = 1) +
  scale_x_discrete(labels = function(Review) str_wrap(Review, width = 10)) + 
# update year references --------------------------------------------------
  chart_lab +
  # scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("#097480", "#8BB5E8", "#0F3D6B")) +
  scale_y_continuous(expand=c(0, 0), limits=c(0, max_value +0.07)) +
  geom_text(aes(label=case_percentage), size = 2.5, check_overlap = TRUE, position = position_stack(vjust = 0.5), colour = "white") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),  #set simple theme
        axis.text = element_text(size = 8),
        strip.placement = "outside",
        strip.text = element_text(size = 8),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        legend.title = element_blank(),
        legend.position="bottom",
        legend.box.background = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 10),
        axis.title = element_text(size = 8,face="italic", hjust = 0.01),
        strip.background = element_blank(),
        panel.spacing = unit(0, 'points')) 

hospice_review_chart

ggsave(hospice_review_chart, 
       filename = "Charts/hospice_review_chart.png",
       device = "png",
       height = 3.2, width = 6, units = "in")

} else {"Not enough data"}  #create message if no chart is created to use place in the report

# chart 12 cause and place of death ------------------------------------------------------

##set cause and place data chart 12##

#Get cause of death data and percentages
DCRS_Data_Chapter <- DCRS_DATA_Loc_DIAG %>%
  filter(prmccdhealthboard == Board,
         YEAR == paste0("Year ", y2) | YEAR == paste0("Year ", y3),
         Review == "Case not in Order",
         DiagGrp != "NA",
         instgrp2 != "NA") %>%
  group_by(YEAR, DiagGrp, instgrp2) %>%
  summarise(case_total = n()) %>%
  mutate(YEAR = case_when(YEAR == paste0("Year ", y2) ~ "Year2",
                          YEAR == paste0("Year ", y3) ~ "Year3"))

diag_check <- DCRS_Data_Chapter %>% filter(case_total >= 10)

if (nrow(diag_check != 0)) {  #CI test only works when denominators are greater than zero, otherwise error occurs 
  DCRS_Data_Chapter_sig <- DCRS_Data_Chapter %>%
    pivot_wider(names_from = YEAR, 
                values_from = 'case_total') %>%
    group_by(DiagGrp) %>%
    mutate(Year2 = case_when(is.na(Year2) ~ 0, TRUE ~ Year2),
           Year3 = case_when(is.na(Year3) ~ 0, TRUE ~ Year3),
           total_Year2 = sum(Year2),
           total_Year3 = sum(Year3)) %>%
    filter(total_Year2 >= 10 | total_Year3 >= 10) %>%
    ungroup() %>%
    mutate(case_rate2 = Year2/total_Year2,
           case_rate3 = Year3/total_Year3,
           case_rate2 = case_when(is.na(case_rate2) ~ 0, TRUE ~ case_rate2),
           case_rate3 = case_when(is.na(case_rate3) ~ 0, TRUE ~ case_rate3),
           ci_mn = BinomDiffCI(x1 = Year2, n1 = total_Year2, x2 = Year3, n2 = total_Year3, method="mn"),  #Apply Miettinen-Nurminen CI Method for significance between 2 years
           ci_sig = case_when(total_Year2 >= 10 & total_Year3 >= 10 ~ (case_when(ci_mn[, "lwr.ci"] < 0 & ci_mn[, "upr.ci"] > 0 ~ "", TRUE ~ "*")), #Flag with * if significance is found (if sample is at least 20 cases)
                               TRUE ~ ""),
           direction = case_when(ci_sig == "*" & case_rate3 > case_rate2 ~ "increase",
                                 ci_sig == "*" & case_rate3 < case_rate2 ~ "decrease",
                                 TRUE ~ ""),       
           ci_sig = case_when(ci_sig == "*" & case_rate3 > case_rate2 ~ "*^",
                              ci_sig == "*" & case_rate3 < case_rate2 ~ "*v",
                              TRUE ~ "")) %>%
    select(DiagGrp, instgrp2, case_rate2, case_rate3, ci_sig, direction)
} else {DCRS_Data_Chapter_sig <- data.frame(DiagGrp = "", instgrp2 = "", case_rate2 = NA, case_rate3 = NA,  #create dummy dataframe with no CI test if first condition is not met
                                                  ci_sig = "", direction = "")}



#mutate(case_percent = case_total / sum(case_total),
#       case_percentage = percent(case_percent, accuracy = 1)) 

#Rank values
DCRS_Data_Chapter_rank <- DCRS_Data_Chapter %>%
  filter(YEAR == "Year3") %>%
  group_by(DiagGrp) %>%
  summarise(case_total = sum(case_total)) %>%
  mutate(top5 = min_rank(desc(case_total))) %>%
  select(DiagGrp, top5)

DCRS_Data_Chapter2 <- DCRS_Data_Chapter %>%
  left_join(DCRS_Data_Chapter_sig, by = c("DiagGrp", "instgrp2")) %>%
  left_join(DCRS_Data_Chapter_rank, by="DiagGrp") %>%
  filter(YEAR == "Year3", top5 <= 5) %>%
  group_by(DiagGrp) %>%   #Aggregate all hospitals identified as "other"
  mutate(diag_total = sum(case_total)) %>%
  ungroup() %>%
  mutate(case_rate2 = case_when(is.na(case_rate2) ~ 0, TRUE ~ case_rate2),
         case_rate3 = case_when(is.na(case_rate3) ~ 0, TRUE ~ case_rate3),
         ci_sig = case_when(is.na(ci_sig) ~ "", TRUE ~ ci_sig),
         direction = case_when(is.na(direction) ~ "", TRUE ~ direction),
         case_rate = case_total/diag_total,
         case_percentage = paste0(percent(case_rate, accuracy = 1), ci_sig),
         YEAR = case_when(YEAR == "Year2" ~ y2, YEAR == "Year3" ~ y3))

sig_flag_both <- DCRS_Data_Chapter2 %>%
  mutate(direction = paste(direction, collapse = ''),
         sig_flag = case_when(str_detect(direction , fixed("increase")) & str_detect(direction , fixed("decrease")) ~ 1, TRUE ~ 0)) %>%
  summarise(sig_flag = sum(sig_flag)) %>%
  pull(sig_flag)

sig_flag_up <- DCRS_Data_Chapter2 %>%
  mutate(direction = paste(direction, collapse = ''),
         sig_flag = case_when(str_detect(direction , fixed("increase"))  ~ 1, TRUE ~ 0)) %>%
  summarise(sig_flag = sum(sig_flag)) %>%
  pull(sig_flag)

sig_flag_down <- DCRS_Data_Chapter2 %>%
  mutate(direction = paste(direction, collapse = ''),
         sig_flag = case_when(str_detect(direction , fixed("decrease"))  ~ 1, TRUE ~ 0)) %>%
  summarise(sig_flag = sum(sig_flag)) %>%
  pull(sig_flag)

#Add caption to chart if significant change is identified
# update year references --------------------------------------------------
chart_lab <- if (sig_flag_both >= 1) {
  labs(title = "Chart 12: Top 5 Number (and percentage) of not in order cases by cause and place of death",   #set titles
       caption = "*^ significant increase from previous year \n *v significant decrease from previous year", #Caption if significant change is identified
       subtitle = year3,
       y = "Number",
       x = "")
} else if (sig_flag_up >= 1) { 
  labs(title = "Chart 12: Top 5 Number (and percentage) of not in order cases by cause and place of death",   #set titles
       caption = "*^ significant increase from previous year", #Caption if significant change is identified
       subtitle = year3,
       y = "Number",
       x = "")
} else if (sig_flag_down >= 1) {
  labs(title = "Chart 12: Top 5 Number (and percentage) of not in order cases by cause and place of death",   #set titles
       caption = "*v significant decrease from previous year", #Caption if significant change is identified
       subtitle = year3,
       y = "Number",
       x = "")
} else {
  labs(title = "Chart 12: Top 5 Number (and percentage) of not in order cases by cause and place of death",   #set titles
       subtitle = year3,
       y = "Number",
       x = "")
}

if (nrow(DCRS_Data_Chapter2 != 0)) { #only create chart if there are cases for the current year

##create cause and place bar chart 12##
diag_chart <- ggplot(DCRS_Data_Chapter2, aes(fill=instgrp2, fct_reorder(DiagGrp, desc(case_total)), case_total)) + 
  geom_bar(position=position_dodge2(preserve="single"), stat="identity") +
  scale_x_discrete(labels = function(DiagGrp) str_wrap(DiagGrp, width = 10)) + 
  chart_lab +
  scale_fill_manual(values = c("#0F3D6B", "#E3CEF6", "#097480", "#A9D0F5")) +
  scale_y_continuous(expand=c(0, 0), limits=c(0, max(DCRS_Data_Chapter$case_total)*1.08)) +
  geom_text(aes(label=case_percentage), size = 2.5, vjust = -0.2, position = position_dodge(width = 0.9), check_overlap = TRUE) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),  #set simple theme
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        axis.title.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank(),
        legend.position="bottom",
        legend.key.size = unit(0.5, 'cm'),
        legend.box.background = element_blank(),
        text = element_text(size = 10),
        panel.background = element_blank())   

diag_chart

ggsave(diag_chart, 
       filename = "Charts/diag_chart.png",
       device = "png",
       height = 3.2, width = 6, units = "in")

} else {"Not enough data"}  #create message if no chart is created to use place in the report



###Create basic pie charts and summary narrative to use in the summary info graphic

##Number of cases summary and proportion of Scotland total

#Get HB and Scotland total for pie proportion
hb <- dcrs_table_data_total %>% filter(event == "Total") %>% pull(`3_Board`)
scot <- dcrs_table_data_total %>% filter(event == "Total") %>% pull(`3_Scotland`)
scot_for_pie <- scot - hb #Remove HB total from Scotland total, so when both are shown on the pie the total add up to the Scotland total (correct HB proportion)

#Create dataset for pie chart
all_pie_data <- dcrs_data_total_all %>%
  ungroup() %>%
  filter(Year == 3) %>%
  mutate(total_all = total_standard + total_interested + total_registrar + total_for_cause,
         total_disp = case_when(`Health Board` == "Board" ~ hb, TRUE ~ scot_for_pie), #create column with correct levels to display segment correctly
         percent_disp = case_when(`Health Board` == "Board" ~ percent(total_disp/sum(total_disp), accuracy = 0.1), TRUE ~ "")) %>%
  select(`Health Board`, total_all, total_disp, percent_disp)

board_percent <- all_pie_data %>% filter(`Health Board` == "Board") %>% pull(percent_disp) #pull percentage of Scotland total to use in summary narrative

#Create basic pie chart with just segments
pie_all <- ggplot(all_pie_data, aes(x = "", y = total_disp, fill = `Health Board`)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) + theme_void() +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#A9D0F5", "#0F3D6B")) +
  theme(legend.position = "none") 

pie_all

#Combine pie chart with summary text then save as image
pie_all2 <- annotate_figure(pie_all, 
                        right = text_grob(paste0("There were a total of ", hb, " cases from \n NHS ", Board, ", representing ", board_percent, " \n of the Scotland total \n
                                                                                    "), 
                                          color = "black", size = 11))
pie_all2 

ggsave(pie_all2, 
       filename = "Charts/pie_all.png",
       device = "png",
       height = 1.5, width = 5, units = "in")




total_pie_data <- dcrs_table_data_total %>%
  filter(event != "Total") %>%
  select(event, `3_Board`)

total_pie_percent <- dcrs_table_data_total %>% filter(event == "total_standard") %>% mutate(HB_percent_year3 = gsub("\\*", "", HB_percent_year3)) %>% pull(HB_percent_year3)

pie_total <- ggplot(total_pie_data, aes(x = "", y = `3_Board`, fill = event)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) + theme_void() +
 # geom_text(aes(label = `3_Board`),
  #          position = position_stack(vjust = 0.5), colour = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#E3CEF6", "#097480", "#A9D0F5", "#0F3D6B")) +
  theme(legend.position = "none") 

pie_total

pie_total2 <- annotate_figure(pie_total, 
                            right = text_grob(paste0(total_pie_percent, " of cases were standard L1 and L2 \n 
                                                                                    "), 
                                              color = "black", size = 11))
pie_total2 

ggsave(pie_total2, 
       filename = "Charts/pie_total.png",
       device = "png",
       height = 1.5, width = 5, units = "in")


percent_io <- dcrs_table_data %>% filter(event == "total_in_order") %>% mutate(HB_percent_year3 = gsub("\\*", "", HB_percent_year3)) %>% pull(HB_percent_year3)
percent_nio <- dcrs_table_data %>% filter(event == "total_not_in_order") %>% mutate(HB_percent_year3 = gsub("\\*", "", HB_percent_year3)) %>% pull(HB_percent_year3)
percent_pf <- dcrs_table_data %>% filter(event == "total_report_to_pf") %>% mutate(HB_percent_year3 = gsub("\\*", "", HB_percent_year3)) %>% pull(HB_percent_year3)


io_pie_data <- dcrs_table_data1 %>%
  select(event, `3_Board`)

pie_io <- ggplot(io_pie_data, aes(x = "", y = `3_Board`, fill = event)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) + theme_void() +
#  geom_text(aes(label = `3_Board`),
 #           position = position_stack(vjust = 0.5), colour = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#0F3D6B", "#E3CEF6", "#A9D0F5")) +
  theme(legend.position = "none") 

pie_io

if(percent_nio == "0.0%" & percent_pf == "0.0%") {
  message <- paste0(percent_io, " of cases were 'in order' \n 
                                                                                    ") 
} else if(percent_pf == "0.0%") {
  message <- paste0(percent_io, " of cases were 'in order', \n and ", percent_nio, " were 'not in order' \n
                                                                                    ") 
} else if(percent_nio == "0.0%") {
  message <- paste0(percent_io, " of cases were 'in order', \n and ", percent_pf, " were reported to PF \n
                                                                                    ") 
} else {
  message <- paste0(percent_io, " of cases were 'in order', \n ", percent_nio, " were 'not in order', \n and ", percent_pf, " were reported to PF \n
                                                                                    ") 
}

pie_io2 <- annotate_figure(pie_io, 
                              right = text_grob(message, 
                                                color = "black", size = 11))
pie_io2 

ggsave(pie_io2, 
       filename = "Charts/pie_io.png",
       device = "png",
       height = 1.5, width = 5, units = "in")


percent_emccd <- dcrs_table_data2 %>% filter(event == "total_emccd") %>% mutate(HB_percent_year3 = gsub("\\*", "", HB_percent_year3)) %>% pull(HB_percent_year3)
percent_mccd <- dcrs_table_data2 %>% filter(event == "total_mccd") %>% mutate(HB_percent_year3 = gsub("\\*", "", HB_percent_year3)) %>% pull(HB_percent_year3)



mccd_pie_data <- dcrs_table_data2 %>%
  select(event, `3_Board`)

pie_mccd <- ggplot(mccd_pie_data, aes(x = "", y = `3_Board`, fill = event)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) + theme_void() +
 # geom_text(aes(label = `3_Board`),
  #          position = position_stack(vjust = 0.5), colour = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#0F3D6B", "#A9D0F5")) +
  theme(legend.position = "none") 

pie_mccd

if(percent_emccd == "0.0%") {
  message <- paste0(percent_mccd, " of cases were paper MCCDs \n
                                                                                    ")
} else if(percent_mccd == "0.0%") {
  message <- paste0(percent_emccd, " of cases were electronic MCCDs \n
                                                                                    ")
} else {
  message <- paste0(percent_emccd, " of cases were electronic MCCDs \n while ", percent_mccd, " were paper MCCDs \n
                                                                                    ")
}

pie_mccd2 <- annotate_figure(pie_mccd, 
                           right = text_grob(message, 
                                             color = "black", size = 11))
pie_mccd2 

ggsave(pie_mccd2, 
       filename = "Charts/pie_mccd.png",
       device = "png",
       height = 1.5, width = 5, units = "in")
