##########  Sets up the data and creates board level funnel plots for MCCDs no in order and replacement MCCDs required used in part 3 of the report


library(dplyr)
library(ggplot2)
library(HISfunnel)
library(ggrepel)
# chart 3 - 6 monthly not in order funnel ---------------------------------------------

#Open function for creating funnel plots
#source("Functions/20230320 plot_funnel.R") 
#devtools::install('N:/Evidence/DMBI/HIS Approach to Measurement/11 Github/HISfunnel')

#Find start of last 6 months for correct period ###Make annual if the reports are all annual now?###
six_month_date <- end_date - 182

# Set Constants to be used by the funnel plot------------------------------
ref_line_type <<- "mean"
target <<- 90
OD_type <<- "Laney"

#set nio funnel data
dcrs_data_cases <- dcrs_data %>%
  filter(`Case Type` == "Standard") %>%
  filter(`Created On` >= six_month_date & `Created On` < end_date) %>%
  group_by(`Health Board`) %>%
  summarise(total_not_in_order = sum(`Case Status` == "Case not in Order"),
            case_total = sum(`Case Status` == "Case in Order" | `Case Status` == "Case not in Order"),
            proportions = total_not_in_order/case_total)

dcrs_nio_funnel_data <- HISfunnel::plot_funnel(data=dcrs_data_cases, subgroup=`Health Board`, denom=case_total, num=total_not_in_order, highlight = Board, 
                                               data_instead = TRUE) %>% filter(`Health Board` == Board)    

#Use function to create nio funnel plot
dcrs_nio_funnel <- HISfunnel::plot_funnel(data=dcrs_data_cases, subgroup=`Health Board`, denom=case_total, num=total_not_in_order, 
                                          SPC_type = "p", lblx="Number of Reviews", lbly="% Not in Order") + 
  labs(title = "Chart 3: Funnel plot of percentage MCCDs not in order for randomised cases",
    subtitle = sixmonth
  ) +  
  # Label selected board
  geom_text_repel(data = dcrs_nio_funnel_data,
                  aes(x = case_total, y = area_prop * 100, label = `Health Board`),
                  size = 3) +
  # Highlight selected board
  geom_point(data = dcrs_nio_funnel_data, 
             aes(x = case_total, y = area_prop * 100), 
             size = 1.5, stroke = 1.5, colour = "#0F3D6B", #HIS dark blue
  ) +
  scale_x_continuous(expand = c(0.00,0.00)) +
  scale_y_continuous(expand = c(0.00,0.00)) +
  coord_cartesian(ylim = c(0, max(dcrs_data_cases$proportions * 120)), xlim = c(0, max(dcrs_data_cases$case_total) * 1.02)) +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 8))   


dcrs_nio_funnel
     
ggsave(dcrs_nio_funnel, 
       filename = "Charts/dcrs_nio_funnel.png",
       device = "png",
       height = 3.2, width = 6, units = "in")

# summary text not in order funnel ----------------------------------------

#nio funnel narrative - identify where a point is in relation to limits
nio_outliers <- dcrs_nio_funnel_data %>%
  mutate(outlier_text = case_when(z_score > 2 ~ "high compared to the Scotland average",
                                  z_score < -2 ~ "low compared to the Scotland average",
                                  z_score > 3 ~ "a high outlier compared to the Scotland average",
                                  z_score < -3 ~ "a low outlier compared to the Scotland average",
                                  z_score >= -2 & z_score <= 2 ~ "within a normal range of deviation from the mean")) %>%
  filter(`Health Board` == Board) %>%
  pull(outlier_text)

#Note for key messages if any outlying signals are found
nio_outliers_summary <- dcrs_nio_funnel_data %>%
  mutate(outlier_text = case_when(z_score > 3 ~ paste0("The board was a high outlier compared to the Scotland average for cases 'not in order' in ",sixmonth,"."),
                                  z_score < -3 ~ paste0("The board was a low outlier compared to the Scotland average for cases 'not in order' in ",sixmonth,"."),
                                  TRUE ~ "")) %>%
  filter(`Health Board` == Board) %>%
  pull(outlier_text)

# chart 4 replacement required funnel -------------------------------------

#Find start of financial year for correct period
start_date <- as.Date(paste(year = as.numeric(substr(year3, 7, 11)), 4, 1, sep = "-"))

#set replacement funnel data
dcrs_data_replace <- dcrs_data %>%
  filter(`Case Type` == "Standard") %>%
  filter(`Created On` >= start_date & `Created On` < end_date) %>%
  group_by(`Health Board`) %>%
  summarise(total_replacement = sum(outcome == "Replacement"),
            total_outcome = sum(outcome == "Replacement" | outcome == "Email" | outcome == "No issues" |
                                  outcome == "Reported to PF"),
            proportions = total_replacement/total_outcome)

dcrs_replace_funnel_data <- HISfunnel::plot_funnel(data=dcrs_data_replace, subgroup=`Health Board`, denom=total_outcome, num=total_replacement, highlight = Board, 
                                                   data_instead = TRUE) %>% filter(`Health Board` == Board)    


#Use function to create replacement funnel plot
dcrs_replace_funnel <- plot_funnel(data=dcrs_data_replace, subgroup=`Health Board`, denom=total_outcome, num=total_replacement, 
                                   SPC_type = "p", lblx="Number of Reviews", lbly="% replacement certificates") + 
  labs(title = "Chart 4: Funnel chart of percentage of MCCDs requiring a replacement",
       subtitle = year3
  ) +
  # Label selected board
  geom_text_repel(data = dcrs_replace_funnel_data,
                  aes(x = total_outcome, y = area_prop * 100, label = `Health Board`),
                  size = 3) +
  # Highlight selected board
  geom_point(data = dcrs_replace_funnel_data, 
             aes(x = total_outcome, y = area_prop * 100), 
             size = 1.5, stroke = 1.5, colour = "#0F3D6B", #HIS dark blue
  ) +
  scale_x_continuous(expand = c(0.00,0.00)) +
  scale_y_continuous(expand = c(0.00,0.00)) +
  coord_cartesian(ylim = c(0, max(dcrs_data_replace$proportions * 120)), xlim = c(0, max(dcrs_data_replace$total_outcome) * 1.02)) +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 8))  

dcrs_replace_funnel

ggsave(dcrs_replace_funnel, 
       filename = "Charts/dcrs_replace_funnel.png",
       device = "png",
       height = 3.2, width = 6, units = "in")

# summary text replacement required ---------------------------------------

#replace funnel narrative - identify where a point is in relation to limits
replace_outliers <- dcrs_replace_funnel_data %>%
  mutate(outlier_text = case_when(z_score > 2 ~ "high compared to the Scotland mean",
                                  z_score < -2 ~ "low compared to the Scotland mean",
                                  z_score > 3 ~ "a high outlier compared to the Scotland mean",
                                  z_score < -3 ~ "a low outlier compared to the Scotland mean",
                                  z_score >= -2 & z_score <= 2 ~ "within a normal range of deviation from the Scotland mean")) %>%
  filter(`Health Board` == Board) %>%
  pull(outlier_text)

#Note for key messages if any outlying signals are found
replace_outliers_summary <- dcrs_replace_funnel_data %>%
  mutate(outlier_text = case_when(z_score > 3 ~ paste0("The funnel chart shows that the board was a high outlier compared to the Scotland mean for cases requiring replacement in ",year3,"."),
                                  z_score < -3 ~ paste0("The funnel chart shows that the board was a low outlier compared to the Scotland mean for cases requiring replacement in ",year3,"."),
                                  TRUE ~ "")) %>%
  filter(`Health Board` == Board) %>%
  pull(outlier_text)


###Key messages note###

#Combine all messages around significant outcomes and identify if any are present
key_message_note <- data.frame(key_message_note = c(hb_new_change_status, nio_outliers_summary, replace_outliers_summary)) %>%
  mutate(note_flag = case_when(key_message_note == "" ~ 0, TRUE ~ 1))

#Find how many, if any, significant outcome messages are present
count_key <- key_message_note %>% summarise(count_key = sum(note_flag)) %>% pull(count_key)

#If any are present then include in note for key messages, otherwise state that there are no significant outcomes
if (count_key > 0) {
key_message <- key_message_note %>%
  filter(note_flag == 1) %>%
  pull(key_message_note)

} else {key_message = "The board had no significant signals in run chart or funnel plot analysis."}
