##########  Sets up the data and creates board level funnel plots for MCCDs no in order and replacement MCCDs required used in part 3 of the report


library(dplyr)
library(ggplot2)
# chart 3 - 6 monthly not in order funnel ---------------------------------------------

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
            case_total = sum(`Case Status` == "Case in Order" | `Case Status` == "Case not in Order"))

#Open function for creating funnel plots
source("Functions/20230320 plot_funnel.R") 

#Use function to create nio funnel plot
dcrs_nio_funnel <- plot_funnel(dat=dcrs_data_cases, subgroup=`Health Board`, denom=case_total, num=total_not_in_order, highlight = Board, 
                                 lblx="Number of Reviews", lbly="% Not in Order", 
                                 ttl="Chart 3: Funnel plot of percentage MCCDs ‘not in order’ for randomised cases ", sbttl=sixmonth)
            

# summary text not in order funnel ----------------------------------------

#nio funnel narrative - identify where a point is in relation to limits
nio_outliers <- subgroup_data %>%
  mutate(outlier_text = case_when(z_score > 2 ~ "high compared to the Scotland average",
                                  z_score < -2 ~ "low compared to the Scotland average",
                                  z_score > 3 ~ "a high outlier compared to the Scotland average",
                                  z_score < -3 ~ "a low outlier compared to the Scotland average",
                                  z_score >= -2 & z_score <= 2 ~ "within a normal range of deviation from the mean")) %>%
  filter(`Health Board` == Board) %>%
  pull(outlier_text)

#Note for key messages if any outlying signals are found
nio_outliers_summary <- subgroup_data %>%
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
                                  outcome == "Reported to PF"))

#Use function to create replacement funnel plot
dcrs_replace_funnel <- plot_funnel(dat=dcrs_data_replace, subgroup=`Health Board`, denom=total_outcome, num=total_replacement, highlight = Board, 
                                 lblx="Number of Reviews", lbly="% replacement certificates", 
                                 ttl="Chart 4: Funnel chart of percentage of MCCDs requiring a replacement", sbttl=year3)



# summary text replacement required ---------------------------------------

#replace funnel narrative - identify where a point is in relation to limits
replace_outliers <- subgroup_data %>%
  mutate(outlier_text = case_when(z_score > 2 ~ "high compared to the Scotland average",
                                  z_score < -2 ~ "low compared to the Scotland average",
                                  z_score > 3 ~ "a high outlier compared to the Scotland average",
                                  z_score < -3 ~ "a low outlier compared to the Scotland average",
                                  z_score >= -2 & z_score <= 2 ~ "within a normal range of deviation from the mean")) %>%
  filter(`Health Board` == Board) %>%
  pull(outlier_text)

#Note for key messages if any outlying signals are found
replace_outliers_summary <- subgroup_data %>%
  mutate(outlier_text = case_when(z_score > 3 ~ paste0("The board was a high outlier compared to the Scotland average for cases requiring replacement in ",year3,"."),
                                  z_score < -3 ~ paste0("The board was a low outlier compared to the Scotland average for cases requiring replacement in ",year3,"."),
                                  TRUE ~ "")) %>%
  filter(`Health Board` == Board) %>%
  pull(outlier_text)


###Key messages note###

#Combine all messages around significant outcomes and identify if any are present
key_message_note <- data.frame(key_message_note = c(board_new_change_status, nio_outliers_summary, replace_outliers_summary)) %>%
  mutate(note_flag = case_when(key_message_note == "" ~ 0, TRUE ~ 1))

#Find how many, if any, significant outcome messages are present
count_key <- key_message_note %>% summarise(count_key = sum(note_flag)) %>% pull(count_key)

#If any are present then include in note for key messages, otherwise state that there are no significant outcomes
key_message <- key_message_note %>% 
  mutate(key_message = case_when(count_key != 0 ~ key_message_note,
                                 TRUE ~ "The board had no significant outcomes in the analysis."),
         duplicate_flag = !duplicated(key_message)) %>%
  filter(duplicate_flag == TRUE) %>%
  pull(key_message)

