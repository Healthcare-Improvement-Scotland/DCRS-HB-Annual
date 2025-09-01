# summary text closure categories -----------------------------------------

##Chart 5 closure categories summary text pt1##

#Percent of all not in order
closure_nio <- dcrs_data_report_all %>%
  select(Year, `Health Board`, closure_category, total_not_in_order) %>%
  filter(Year == 3 & `Health Board` == "Board") %>%
  mutate(closure_nio_percent = percent(closure_category / total_not_in_order, accuracy = 0.1)) %>%
  pull(closure_nio_percent)

#Set full names if data is present
if (!is.na(closure_nio)) {
  dcrs_data_wrangle_closure <-  dcrs_data_wrangle_closure %>%
    mutate(closure_category = str_replace(closure_category, "causal_timscale", "'causal timescales incorrect'"),
           closure_category = str_replace(closure_category, "cause_of_death", "'cause of death incorrect'"),
           closure_category = str_replace(closure_category, "conditions_omitted", "'conditions omitted'"), 
           closure_category = str_replace(closure_category, "disposal_hazard", "'disposal hazard incorrect'"),
           closure_category = str_replace(closure_category, "sequence_of_cause", "'sequence of cause of death incorrect'"), 
           closure_category = str_replace(closure_category, "cause_too_vague", "'cause of death too vague'"),
           `Health Board` = case_when(`Health Board` == "Board" ~ "Board", TRUE ~ ""))
} else { #if no data is present create dummy dataframe
  dcrs_data_wrangle_closure <- data.frame(hb = "Board", closure_category = NA, closure_rate = NA, closure_rate_old = NA,
                                          closure_percent = NA, direction = "") %>% 
    rename(`Health Board` = hb)
}

#Find max closure percent
max_closure_value <- dcrs_data_wrangle_closure %>% filter(`Health Board` == "Board") %>% filter(closure_rate == max(closure_rate)) %>% 
  filter(closure_category == max(closure_category)) %>% pull(closure_percent)

#Find name of max closure category
#max_closure_name <- dcrs_data_wrangle_closure2 %>% filter(closure_rate == max(closure_rate)) %>% 
#  filter(closure_category == max(closure_category)) %>% pull(closure_category)



##Chart 5 closure categories summary text pt2##

#SETTING UP FOR MORE DYNAMIC NARRATIVE
#Find max closure category
max_closure <- dcrs_data_wrangle_closure %>% 
  filter(`Health Board` == "Board") %>% filter(closure_rate == max(closure_rate))

#Combine names if multiple have same max value
max_closure_multiple <- max_closure %>%
  select(closure_category) %>%
  mutate(closure_names = paste(closure_category, collapse = ' and ')) %>%
  filter(closure_category == max(closure_category)) %>% 
  pull(closure_names)

if (nrow(max_closure > 0)) { #only run is data is present
  #Test if multiple have same max values and assign correct name(s)
  max_closure_name <- max_closure %>%
    mutate(alt_value = case_when(nrow(max_closure) == 1 ~ closure_category,
                                 nrow(max_closure) >= 2 ~ max_closure_multiple)) %>%
    filter(closure_category == max(closure_category)) %>% 
    pull(alt_value)
} else {max_closure_name = NA}

#Test if multiple have same max value and set correct plural for category  
closure_value1 <- max_closure %>%
  mutate(alt_value = case_when(nrow(max_closure) == 1 ~ "y",
                               nrow(max_closure) >= 2 ~ "ies")) %>%
  filter(closure_category == max(closure_category)) %>% 
  pull(alt_value)

#Test if multiple have same max value and set intro for percent
closure_value2 <- max_closure %>%
  mutate(alt_value = case_when(nrow(max_closure) == 1 ~ "",
                               nrow(max_closure) >= 2 ~ "each")) %>%
  filter(closure_category == max(closure_category)) %>% 
  pull(alt_value)

#Text if primary/secondary care chart is used
care_breakdown_text <- if(Board != "Borders" & Board != "Dumfries and Galloway" & Board != "Shetland" & Board != "Orkney" 
                          & Board != "Western Isles" & Board != "National Golden Jubilee"){
  "Over the three years displayed, 'cause of death too vague' has been consistently the most common reason for a clinical error being recorded. A larger proportion of these certificates come from primary care compared to secondary care." 
} else { "" }

#significant text depending on chart used
if(Board == "Borders" | Board == "Dumfries and Galloway" | Board == "Shetland" | Board == "Orkney" 
   | Board == "Western Isles" | Board == "National Golden Jubilee"){
  
  #Bar Scotland significant text extract
  increase_count <- dcrs_data_wrangle_closure %>% filter(`Health Board` == "Scotland") %>% 
    summarise(count_cases = sum(direction == 'increase')) %>% pull(count_cases)
  decrease_count <- dcrs_data_wrangle_closure %>% filter(`Health Board` == "Scotland") %>% 
    summarise(count_cases = sum(direction == 'decrease')) %>% pull(count_cases)
  
#narrative if significant increase for Scotland is found  
  case_increase <- dcrs_data_wrangle_closure %>%
    filter(direction == "increase",
           `Health Board` == "Scotland") %>%
    select(closure_category, closure_rate, closure_rate_old) %>%
    mutate(case_percent = percent(closure_rate, accuracy = 0.1),
           case_percent_old = percent(closure_rate_old, accuracy = 0.1),
           case_increase = case_when(increase_count >= 1 ~ paste0(tolower(closure_category), collapse = ', ')),
           case_change = paste("from", case_percent_old,  "to", case_percent, collapse = ', '),
           case_increase = case_when(increase_count >= 1 ~ paste(case_increase, "saw a significant increase", case_change),
                                     TRUE ~ "")) %>%
    filter(closure_category == max(closure_category)) %>% 
    pull(case_increase)
  
#narrative if significant decrease for Scotland is found  
  case_decrease <- dcrs_data_wrangle_closure %>%
    filter(direction == "decrease",
           `Health Board` == "Scotland") %>%
    select(closure_category, closure_rate, closure_rate_old) %>%
    mutate(case_percent = percent(closure_rate, accuracy = 0.1),
           case_percent_old = percent(closure_rate_old, accuracy = 0.1),
           case_decrease = case_when(decrease_count >= 1 ~ paste0(tolower(closure_category), collapse = ', ')),
           case_change = paste("from", case_percent_old,  "to", case_percent, collapse = ', '),
           case_decrease = case_when(decrease_count >= 1 ~ paste(case_decrease, "saw a significant decrease", case_change),
                                     TRUE ~ "")) %>%
    filter(closure_category == max(closure_category)) %>% 
    pull(case_decrease)

#combine increase and decrease narrative for full narrative    
  if (increase_count >= 1 & decrease_count >= 1) {
    scot_closure_sig = paste0("+ Scotland, ", case_increase, ", and ", case_decrease, " compared to ", year2, ".")
  } else if (increase_count >= 1) {
    scot_closure_sig = paste0("+ Scotland, ", case_increase, " compared to ", year2, ".")
  } else if (decrease_count >= 1) {
    scot_closure_sig = paste0("+ Scotland, ", case_decrease, " compared to ", year2, ".")
  } else { scot_closure_sig = "" }
  
  #HB
  increase_count <- dcrs_data_wrangle_closure %>% filter(`Health Board` == "Board") %>% 
    summarise(count_cases = sum(direction == 'increase')) %>% pull(count_cases)
  decrease_count <- dcrs_data_wrangle_closure %>% filter(`Health Board` == "Board") %>% 
    summarise(count_cases = sum(direction == 'decrease')) %>% pull(count_cases)

#narrative if significant increase for board is found    
  case_increase <- dcrs_data_wrangle_closure %>%
    filter(direction == "increase",
           `Health Board` == "Board") %>%
    select(closure_category, closure_rate, closure_rate_old) %>%
    mutate(case_percent = percent(closure_rate, accuracy = 0.1),
           case_percent_old = percent(closure_rate_old, accuracy = 0.1),
           case_increase = case_when(increase_count >= 1 ~ paste0(tolower(closure_category), collapse = ', ')),
           case_change = paste("from", case_percent_old,  "to", case_percent, collapse = ', '),
           case_increase = case_when(increase_count >= 1 ~ paste(case_increase, "saw a significant increase", case_change),
                                     TRUE ~ "")) %>%
    filter(closure_category == max(closure_category)) %>% 
    pull(case_increase)

#narrative if significant decrease for board is found    
  case_decrease <- dcrs_data_wrangle_closure %>%
    filter(direction == "decrease",
           `Health Board` == "Board") %>%
    select(closure_category, closure_rate, closure_rate_old) %>%
    mutate(case_percent = percent(closure_rate, accuracy = 0.1),
           case_percent_old = percent(closure_rate_old, accuracy = 0.1),
           case_decrease = case_when(decrease_count >= 1 ~ paste0(tolower(closure_category), collapse = ', ')),
           case_change = paste("from", case_percent_old,  "to", case_percent, collapse = ', '),
           case_decrease = case_when(decrease_count >= 1 ~ paste(case_decrease, "saw a significant decrease", case_change),
                                     TRUE ~ "")) %>%
    filter(closure_category == max(closure_category)) %>% 
    pull(case_decrease)
  
#combine increase and decrease narrative for full narrative  
  if (increase_count >= 1 & decrease_count >= 1) {
    hb_closure_sig = paste0("+ At NHS ", Board, ", ", case_increase, ", and ", case_decrease, " compared to ", year2, ".")
  } else if (increase_count >= 1) {
    hb_closure_sig = paste0("+ At NHS ", Board, ", ",case_increase, " compared to ", year2, ".")
  } else if (decrease_count >= 1) {
    hb_closure_sig = paste0("+ At NHS ", Board, ", ",case_decrease, " compared to ", year2, ".")
  } else { hb_closure_sig = "" }
  
} else {
  
#  Year 1 vs year 2 primary vs secondary care - find if significant changes present
  increase_count_p <- dcrs_data_wranlge_ps %>% filter(primary_secondary == 'Primary', Year == 9) %>% 
    mutate(direction = case_when(is.na(direction) ~ "", TRUE ~ direction)) %>%
    summarise(count_cases = sum(direction == 'increase')) %>% pull(count_cases)
  increase_count_s <- dcrs_data_wranlge_ps %>% filter(primary_secondary == 'Secondary', Year == 9) %>% 
    mutate(direction = case_when(is.na(direction) ~ "", TRUE ~ direction)) %>%
    summarise(count_cases = sum(direction == 'increase')) %>% pull(count_cases)
  decrease_count_p <- dcrs_data_wranlge_ps %>% filter(primary_secondary == 'Primary', Year == 9) %>% 
    mutate(direction = case_when(is.na(direction) ~ "", TRUE ~ direction)) %>%
    summarise(count_cases = sum(direction == 'decrease')) %>% pull(count_cases)
  decrease_count_s <- dcrs_data_wranlge_ps %>% filter(primary_secondary == 'Secondary', Year == 9) %>% 
    mutate(direction = case_when(is.na(direction) ~ "", TRUE ~ direction)) %>%
    summarise(count_cases = sum(direction == 'decrease')) %>% pull(count_cases)
 
#narrative for significant primary care increase   
  closure_increase_p <- dcrs_data_wranlge_ps %>%
    filter(direction == "increase", primary_secondary == "Primary") %>%
    select(closure_category) %>%
    mutate(closure_increase_p = case_when(increase_count_p >= 1 ~ paste0("'",tolower(closure_category),"'", collapse = ', '),
                                          TRUE ~ ""),
           closure_increase_p = str_replace_all(closure_increase_p, "\n", " ")) %>%
    filter(closure_category == max(closure_category)) %>% 
    pull(closure_increase_p)
  
#narrative for significant secondary care increase     
  closure_increase_s <- dcrs_data_wranlge_ps %>%
    filter(direction == "increase", primary_secondary == "Secondary") %>%
    select(closure_category) %>%
    mutate(closure_increase_s = case_when(increase_count_s >= 1 ~ paste0("'",tolower(closure_category),"'", collapse = ', '),
                                          TRUE ~ ""),
           closure_increase_s = str_replace_all(closure_increase_s, "\n", " ")) %>%
    filter(closure_category == max(closure_category)) %>% 
    pull(closure_increase_s)

#narrative for significant primary care decrease       
  closure_decrease_p <- dcrs_data_wranlge_ps %>%
    filter(direction == "decrease", primary_secondary == "Primary") %>%
    select(closure_category) %>%
    mutate(closure_decrease_p = case_when(decrease_count_p >= 1 ~ paste0("'",tolower(closure_category),"'", collapse = ', '),
                                          TRUE ~ ""),
           closure_decrease_p = str_replace_all(closure_decrease_p, "\n", " ")) %>%
    filter(closure_category == max(closure_category)) %>% 
    pull(closure_decrease_p)
  
#narrative for significant secondary care decrease     
  closure_decrease_s <- dcrs_data_wranlge_ps %>%
    filter(direction == "decrease", primary_secondary == "Secondary") %>%
    select(closure_category) %>%
    mutate(closure_decrease_s = case_when(decrease_count_s >= 1 ~ paste0("'",tolower(closure_category),"'", collapse = ', '),
                                          TRUE ~ ""),
           closure_decrease_s = str_replace_all(closure_decrease_s, "\n", " ")) %>%
    filter(closure_category == max(closure_category)) %>% 
    pull(closure_decrease_s)
  
#  closure_increase_change_p <- dcrs_data_wranlge_ps %>%
#    filter(direction == "increase", primary_secondary == "Primary") %>%
#    select(closure_category, closure_rate, closure_rate_old) %>%
#    mutate(closure_percent = percent(closure_rate, accuracy = 0.1),
 #          closure_percent_old = percent(closure_rate_old, accuracy = 0.1),
 #          closure_increase_change_p = paste("from", closure_percent_old,  "to", closure_percent, collapse = ', ')) %>%
  #  filter(closure_category == max(closure_category)) %>% 
  #  pull(closure_increase_change_p)
  
#create full narrative for required combination for parimary and seoncary care significant changes
  if (increase_count_p >= 1 & increase_count_s >= 1 & decrease_count_p >= 1 & decrease_count_s >= 1) {
    hb_closure_sig1 = paste0("+ In primary care ", closure_increase_p, ", and in secondary care ", closure_increase_s, 
                             " saw a significant increase while in primary care ",
                            closure_decrease_p, ", and in secondary care ", closure_decrease_s, 
                            "saw a singificant decrease compared to ", year1, ".")
  } else if (increase_count_p >= 1 & increase_count_s >= 1  & decrease_count_p >= 1) {
    hb_closure_sig1 = paste0("+ In primary care ", closure_increase_p, ", and in secondary care ", closure_increase_s, 
                             " saw a significant increase while in primary care ",
                            closure_decrease_p, " saw a significant decrease ", " compared to ", year1, ".")
  } else if (increase_count_p >= 1 & increase_count_s >= 1  & decrease_count_s >= 1) {
    hb_closure_sig1 = paste0("+ In primary care ", closure_increase_p, ", and in secondary care ", closure_increase_s, 
                             " saw a significant increase while in secondary care ",
                            closure_decrease_s, " saw a significant decrease ", " compared to ", year1, ".")
  } else if (increase_count_s >= 1 & decrease_count_p >= 1  & decrease_count_s >= 1) {
    hb_closure_sig1 = paste0("+ In secondary care ", closure_increase_s, " saw a significant increase while in primary care ", 
                             closure_decrease_p, ", and in secondary care ",
                             closure_decrease_s, " saw a significant decrease compared to ", year1, ".")
  } else if (increase_count_p >= 1 & increase_count_s >= 1) {
    hb_closure_sig1 = paste0("+ In primary care ", closure_increase_p, ", and in secondary care ", closure_increase_s, 
                             " saw a significant increase compared to ", year1, ".")
  } else if (decrease_count_p >= 1 & decrease_count_s >= 1) {
    hb_closure_sig1 = paste0("+ In primary care ", closure_decrease_p, ", and in secondary care ", closure_decrease_s, 
                             " saw a significant decrease compared to ", year1, ".")
  } else if (increase_count_p >= 1 & decrease_count_s >= 1) {
    hb_closure_sig1 = paste0("+ In primary care ", closure_increase_p, " saw a significant increase while in secondary care ", 
                             closure_decrease_s, " saw a singificant decrease compared to ", year1, ".")
  } else if (increase_count_s >= 1 & decrease_count_p >= 1) {
    hb_closure_sig1 = paste0("+ In secondary care ", closure_increase_s, " saw a significant increase, while in primary care ", 
                            closure_decrease_p, " saw a singificant decrease compared to ", year1, ".")
  } else if (increase_count_p >= 1) {
    hb_closure_sig1 = paste0("+ In primary care ", closure_increase_p, " saw a significant increase compared to ", year1, ".")
  } else if (increase_count_s >= 1) {
    hb_closure_sig1 = paste0("+ In secondary care ", closure_increase_s, " saw a significant increase compared to ", year1, ".")
  } else if (decrease_count_p >= 1) {
    hb_closure_sig1 = paste0("+ In primary care ", closure_decrease_p, " saw a significant decrease compared to ", year1, ".")
  } else if (decrease_count_s >= 1) {
    hb_closure_sig1 = paste0("+ In secondary care ", closure_decrease_s, " saw a significant decrease compared to ", year1, ".")
  } else { hb_closure_sig1 = "" }
  
#  Year 2 vs year 3  - find if significant changes present
  increase_count_p <- dcrs_data_wranlge_ps %>% filter(primary_secondary == 'Primary', Year == 10) %>% 
    mutate(direction = case_when(is.na(direction) ~ "", TRUE ~ direction)) %>%
    summarise(count_cases = sum(direction == 'increase')) %>% pull(count_cases)
  increase_count_s <- dcrs_data_wranlge_ps %>% filter(primary_secondary == 'Secondary', Year == 10) %>% 
    mutate(direction = case_when(is.na(direction) ~ "", TRUE ~ direction)) %>%
    summarise(count_cases = sum(direction == 'increase')) %>% pull(count_cases)
  decrease_count_p <- dcrs_data_wranlge_ps %>% filter(primary_secondary == 'Primary', Year == 10) %>% 
    mutate(direction = case_when(is.na(direction) ~ "", TRUE ~ direction)) %>%
    summarise(count_cases = sum(direction == 'decrease')) %>% pull(count_cases)
  decrease_count_s <- dcrs_data_wranlge_ps %>% filter(primary_secondary == 'Secondary', Year == 10) %>% 
    mutate(direction = case_when(is.na(direction) ~ "", TRUE ~ direction)) %>%
    summarise(count_cases = sum(direction == 'decrease')) %>% pull(count_cases)

  #narrative for significant primary care increase     
  closure_increase_p <- dcrs_data_wranlge_ps %>%
    filter(direction == "increase", primary_secondary == "Primary") %>%
    select(closure_category) %>%
    mutate(closure_increase_p = case_when(increase_count_p >= 1 ~ paste0("'",tolower(closure_category),"'", collapse = ', '),
                                          TRUE ~ ""),
           closure_increase_p = str_replace_all(closure_increase_p, "\n", " ")) %>%
    filter(closure_category == max(closure_category)) %>% 
    pull(closure_increase_p)
  
  #narrative for significant secondary care increase   
  closure_increase_s <- dcrs_data_wranlge_ps %>%
    filter(direction == "increase", primary_secondary == "Secondary") %>%
    select(closure_category) %>%
    mutate(closure_increase_s = case_when(increase_count_s >= 1 ~ paste0("'",tolower(closure_category),"'", collapse = ', '),
                                          TRUE ~ ""),
           closure_increase_s = str_replace_all(closure_increase_s, "\n", " ")) %>%
    filter(closure_category == max(closure_category)) %>% 
    pull(closure_increase_s)
  
  #narrative for significant primary care decrease   
  closure_decrease_p <- dcrs_data_wranlge_ps %>%
    filter(direction == "decrease", primary_secondary == "Primary") %>%
    select(closure_category) %>%
    mutate(closure_decrease_p = case_when(decrease_count_p >= 1 ~ paste0("'",tolower(closure_category),"'", collapse = ', '),
                                          TRUE ~ ""),
           closure_decrease_p = str_replace_all(closure_decrease_p, "\n", " ")) %>%
    filter(closure_category == max(closure_category)) %>% 
    pull(closure_decrease_p)
  
  #narrative for significant secondary care decrease   
  closure_decrease_s <- dcrs_data_wranlge_ps %>%
    filter(direction == "decrease", primary_secondary == "Secondary") %>%
    select(closure_category) %>%
    mutate(closure_decrease_s = case_when(decrease_count_s >= 1 ~ paste0("'",tolower(closure_category),"'", collapse = ', '),
                                          TRUE ~ ""),
           closure_decrease_s = str_replace_all(closure_decrease_s, "\n", " ")) %>%
    filter(closure_category == max(closure_category)) %>% 
    pull(closure_decrease_s)
  
  #create full narrative for required combination for parimary and seoncary care significant changes  
  if (increase_count_p >= 1 & increase_count_s >= 1 & decrease_count_p >= 1 & decrease_count_s >= 1) {
    hb_closure_sig2 = paste0("+ In primary care ", closure_increase_p, ", and in secondary care ", closure_increase_s, 
                             " saw a significant increase while in primary care ",
                             closure_decrease_p, ", and in secondary care ", closure_decrease_s, 
                             "saw a singificant decrease compared to ", year2, ".")
  } else if (increase_count_p >= 1 & increase_count_s >= 1  & decrease_count_p >= 1) {
    hb_closure_sig2 = paste0("+ In primary care ", closure_increase_p, ", and in secondary care ", closure_increase_s, 
                             " saw a significant increase while in primary care ",
                             closure_decrease_p, " saw a significant decrease ", " compared to ", year2, ".")
  } else if (increase_count_p >= 1 & increase_count_s >= 1  & decrease_count_s >= 1) {
    hb_closure_sig2 = paste0("+ In primary care ", closure_increase_p, ", and in secondary care ", closure_increase_s, 
                             " saw a significant increase while in secondary care ",
                             closure_decrease_s, " saw a significant decrease ", " compared to ", year2, ".")
  } else if (increase_count_s >= 1 & decrease_count_p >= 1  & decrease_count_s >= 1) {
    hb_closure_sig2 = paste0("+ In secondary care ", closure_increase_s, " saw a significant increase while in primary care ", 
                             closure_decrease_p, ", and in secondary care ",
                             closure_decrease_s, " saw a significant decrease compared to ", year2, ".")
  } else if (increase_count_p >= 1 & increase_count_s >= 1) {
    hb_closure_sig2 = paste0("+ In primary care ", closure_increase_p, ", and in secondary care ", closure_increase_s, 
                             " saw a significant increase compared to ", year2, ".")
  } else if (decrease_count_p >= 1 & decrease_count_s >= 1) {
    hb_closure_sig2 = paste0("+ In primary care ", closure_decrease_p, ", and in secondary care ", closure_decrease_s, 
                             " saw a significant decrease compared to ", year2, ".")
  } else if (increase_count_p >= 1 & decrease_count_s >= 1) {
    hb_closure_sig2 = paste0("+ In primary care ", closure_increase_p, " saw a significant increase while in secondary care ", 
                             closure_decrease_s, " saw a singificant decrease compared to ", year2, ".")
  } else if (increase_count_s >= 1 & decrease_count_p >= 1) {
    hb_closure_sig2 = paste0("+ In secondary care ", closure_increase_s, " saw a significant increase, while in primary care ", 
                             closure_decrease_p, " saw a singificant decrease compared to ", year2, ".")
  } else if (increase_count_p >= 1) {
    hb_closure_sig2 = paste0("+ In primary care ", closure_increase_p, " saw a significant increase compared to ", year2, ".")
  } else if (increase_count_s >= 1) {
    hb_closure_sig2 = paste0("+ In secondary care ", closure_increase_s, " saw a significant increase compared to ", year2, ".")
  } else if (decrease_count_p >= 1) {
    hb_closure_sig2 = paste0("+ In primary care ", closure_decrease_p, " saw a significant decrease compared to ", year2, ".")
  } else if (decrease_count_s >= 1) {
    hb_closure_sig2 = paste0("+ In secondary care ", closure_decrease_s, " saw a significant decrease compared to ", year2, ".")
  } else { hb_closure_sig2 = "" }
 
  #combine both year for final narrative used in report 
  hb_closure_sig <- paste0(hb_closure_sig1, hb_closure_sig2)
  
  #create dummy narrative for Scotland as there is no Scotland level chart
  scot_closure_sig <- ""
}

# summary text cause too vague --------------------------------------------

##Chart 6 cause too vague summary text pt1##

#Percent cause too vague not in order
closure_too_vauge_nio <- dcrs_data_report_all %>%
  select(Year, `Health Board`, cause_too_vague, total_not_in_order) %>%
  filter(Year == 3 & `Health Board` == "Board") %>%
  mutate(closure_nio_percent = percent(cause_too_vague / total_not_in_order, accuracy = 0.1)) %>%
  pull(closure_nio_percent)

if (!is.na(closure_too_vauge_nio)) {
  #Set full names too vague if data is present
  dcrs_data_wrangle_death_cause <-  dcrs_data_wrangle_death_cause %>%
    mutate(cause_too_vague = str_replace(cause_too_vague, 
                                         "histology", "'histology'"),
           cause_too_vague = str_replace(cause_too_vague, 
                                         "psite_metastatic_missing", "'primary site or metastatic site(s) missing'"),
           cause_too_vague = str_replace(cause_too_vague, 
                                         "pneumonia_subtype", "'pneumonia sub-type'"),
           cause_too_vague = str_replace(cause_too_vague, 
                                         "dementia_subtype", "'dementia sub-type'"),
           cause_too_vague = str_replace(cause_too_vague, 
                                         "microbiology", "'microbiology'"),
           cause_too_vague = str_replace(cause_too_vague, 
                                         "sepsis_source", "'source of sepsis'"),
           cause_too_vague = str_replace(cause_too_vague, 
                                         "diabetes_subtype", "'diabetes sub-type'"),
           cause_too_vague = str_replace(cause_too_vague, 
                                         "stroke", "'stroke'"),
           cause_too_vague = str_replace(cause_too_vague, 
                                         "lifestyle_factor", "'lifestyle factors (smoking, obesity, alcohol'"),
           hb = case_when(`Health Board` == "Board" ~ "Board", TRUE ~ ""))
} else { #if no data is present create dummy dataframe
  dcrs_data_wrangle_death_cause <- data.frame(hb = "Board", cause_too_vague = NA, cause_too_vague_rate = NA, cause_too_vague_percent = NA)}

#Find max vague percent
max_vague_value <- dcrs_data_wrangle_death_cause %>% 
  filter(hb == "Board") %>% filter(cause_too_vague_rate == max(cause_too_vague_rate)) %>% 
  filter(cause_too_vague == max(cause_too_vague)) %>% pull(cause_too_vague_percent)

#Second highest value
max_vague_value2 <- dcrs_data_wrangle_death_cause %>% 
  filter(hb == "Board") %>% filter(cause_too_vague_rate == max(cause_too_vague_rate[cause_too_vague_rate != max(cause_too_vague_rate)])) %>% pull(cause_too_vague_percent)

#Second hightest cause name
max_vague_name2 <- dcrs_data_wrangle_death_cause %>% 
  filter(hb == "Board") %>% filter(cause_too_vague_rate == max(cause_too_vague_rate[cause_too_vague_rate != max(cause_too_vague_rate)])) %>% pull(cause_too_vague)


##Chart 6 cause too vague summary text pt2##

#Find max too vague breakdown

#Highest value rate
max_too_vague <- dcrs_data_wrangle_death_cause %>% 
  filter(hb == "Board") %>% filter(cause_too_vague_rate == max(cause_too_vague_rate))

#Highest value cause name
max_too_vague_multiple <- max_too_vague %>%
  select(cause_too_vague) %>%
  mutate(vague_names = paste(cause_too_vague, collapse = ' and ')) %>%
  filter(cause_too_vague == max(cause_too_vague)) %>% 
  pull(vague_names)

#Test if individual cause is max or multiple cause share same max value
if (nrow(max_too_vague > 0)) {
  max_vague_name <- max_too_vague %>%
    mutate(alt_value = case_when(nrow(max_too_vague) == 1 ~ cause_too_vague,
                                 nrow(max_too_vague) >= 2 ~ max_too_vague_multiple)) %>%
    filter(cause_too_vague == max(cause_too_vague)) %>% 
    pull(alt_value)
} else {max_vague_name = NA}
#Extra syntax if single or mutliple cause identified for max
vague_value1 <- max_too_vague %>%
  mutate(alt_value = case_when(nrow(max_too_vague) == 1 ~ "y",
                               nrow(max_too_vague) >= 2 ~ "ies")) %>%
  filter(cause_too_vague == max(cause_too_vague)) %>% 
  pull(alt_value)

#Extra syntax if single or mutliple cause identified for max
vague_value2 <- max_too_vague %>%
  mutate(alt_value = case_when(nrow(max_too_vague) == 1 ~ "",
                               nrow(max_too_vague) >= 2 ~ "each")) %>%
  filter(cause_too_vague == max(cause_too_vague)) %>% 
  pull(alt_value)

#find denominators to test if signifcant narrative is required
HB_n1 <- dcrs_data_report_all %>% select(Year, `Health Board`, cause_too_vague) %>% filter(Year == 2, `Health Board` == "Board") %>% pull(cause_too_vague)
HB_n2 <- dcrs_data_report_all %>% select(Year, `Health Board`, cause_too_vague) %>% filter(Year == 3, `Health Board` == "Board") %>% pull(cause_too_vague)


if (HB_n1 > 0 & HB_n2 > 0) { #only create CI narrative if board has data present for both years
  #Bar Scotland significant text extract
  increase_count <- dcrs_data_wrangle_death_cause %>% filter(`Health Board` == "Scotland") %>% 
    summarise(count_cases = sum(direction == 'increase')) %>% pull(count_cases)
  decrease_count <- dcrs_data_wrangle_death_cause %>% filter(`Health Board` == "Scotland") %>% 
    summarise(count_cases = sum(direction == 'decrease')) %>% pull(count_cases)
 
  #narrative if significant increase for Scotland is found  
  case_increase <- dcrs_data_wrangle_death_cause %>%
    filter(direction == "increase",
           `Health Board` == "Scotland") %>%
    select(cause_too_vague, cause_too_vague_rate, cause_too_vague_rate_old) %>%
    mutate(case_percent = percent(cause_too_vague_rate, accuracy = 0.1),
           case_percent_old = percent(cause_too_vague_rate_old, accuracy = 0.1),
           case_increase = case_when(increase_count >= 1 ~ paste0(tolower(cause_too_vague), collapse = ', ')),
           case_change = paste("from", case_percent_old,  "to", case_percent, collapse = ', '),
           case_increase = case_when(increase_count >= 1 ~ paste(case_increase, "saw a significant increase", case_change),
                                     TRUE ~ "")) %>%
    filter(cause_too_vague == max(cause_too_vague)) %>% 
    pull(case_increase)
  
  #narrative if significant decrease for Scotland is found  
  case_decrease <- dcrs_data_wrangle_death_cause %>%
    filter(direction == "decrease",
           `Health Board` == "Scotland") %>%
    select(cause_too_vague, cause_too_vague_rate, cause_too_vague_rate_old) %>%
    mutate(case_percent = percent(cause_too_vague_rate, accuracy = 0.1),
           case_percent_old = percent(cause_too_vague_rate_old, accuracy = 0.1),
           case_decrease = case_when(decrease_count >= 1 ~ paste0(tolower(cause_too_vague), collapse = ', ')),
           case_change = paste("from", case_percent_old,  "to", case_percent, collapse = ', '),
           case_decrease = case_when(decrease_count >= 1 ~ paste(case_decrease, "saw a significant decrease", case_change),
                                     TRUE ~ "")) %>%
    filter(cause_too_vague == max(cause_too_vague)) %>% 
    pull(case_decrease)
  
  #combine increase and decrease for full singificant narrative
  if (increase_count >= 1 & decrease_count >= 1) {
    scot_cause_sig = paste0("+ At Scotland, ", case_increase, ", and ", case_decrease, " compared to ", year2, ".")
  } else if (increase_count >= 1) {
    scot_cause_sig = paste0("+ At Scotland, ", case_increase, " compared to ", year2, ".")
  } else if (decrease_count >= 1) {
    scot_cause_sig = paste0("+ At Scotland, ", case_decrease, " compared to ", year2, ".")
  } else { scot_cause_sig = "" }
  
  #HB
  increase_count <- dcrs_data_wrangle_death_cause %>% filter(`Health Board` == "Board") %>% 
    summarise(count_cases = sum(direction == 'increase')) %>% pull(count_cases)
  decrease_count <- dcrs_data_wrangle_death_cause %>% filter(`Health Board` == "Board") %>% 
    summarise(count_cases = sum(direction == 'decrease')) %>% pull(count_cases)
 
  #narrative if significant increase for board is found   
  case_increase <- dcrs_data_wrangle_death_cause %>%
    filter(direction == "increase",
           `Health Board` == "Board") %>%
    select(cause_too_vague, cause_too_vague_rate, cause_too_vague_rate_old) %>%
    mutate(case_percent = percent(cause_too_vague_rate, accuracy = 0.1),
           case_percent_old = percent(cause_too_vague_rate_old, accuracy = 0.1),
           case_increase = case_when(increase_count >= 1 ~ paste0(tolower(cause_too_vague), collapse = ', ')),
           case_change = paste("from", case_percent_old,  "to", case_percent, collapse = ', '),
           case_increase = case_when(increase_count >= 1 ~ paste(case_increase, "saw a significant increase", case_change),
                                     TRUE ~ "")) %>%
    filter(cause_too_vague == max(cause_too_vague)) %>% 
    pull(case_increase)
  
  #narrative if significant decrease for board is found  
  case_decrease <- dcrs_data_wrangle_death_cause %>%
    filter(direction == "decrease",
           `Health Board` == "Board") %>%
    select(cause_too_vague, cause_too_vague_rate, cause_too_vague_rate_old) %>%
    mutate(case_percent = percent(cause_too_vague_rate, accuracy = 0.1),
           case_percent_old = percent(cause_too_vague_rate_old, accuracy = 0.1),
           case_decrease = case_when(decrease_count >= 1 ~ paste0(tolower(cause_too_vague), collapse = ', ')),
           case_change = paste("from", case_percent_old,  "to", case_percent, collapse = ', '),
           case_decrease = case_when(decrease_count >= 1 ~ paste(case_decrease, "saw a significant decrease", case_change),
                                     TRUE ~ "")) %>%
    filter(cause_too_vague == max(cause_too_vague)) %>% 
    pull(case_decrease)
  
  #combine increase and decrease for full significant narrative
  if (increase_count >= 1 & decrease_count >= 1) {
    hb_cause_sig = paste0("+ At NHS ", Board, ", ",case_increase, ", and ", case_decrease, " compared to ", year2, ".")
  } else if (increase_count >= 1) {
    hb_cause_sig = paste0("+ At NHS ", Board, ", ",case_increase, " compared to ", year2, ".")
  } else if (decrease_count >= 1) {
    hb_cause_sig = paste0("+ At NHS ", Board, ", ",case_decrease, " compared to ", year2, ".")
  } else { hb_cause_sig = "" }
} else { scot_cause_sig = "" 
hb_cause_sig = ""}


# summary text admin ------------------------------------------------------

##Chart 7 admin summary text pt1##

#Percent admin not in order
closure_admin_nio <- dcrs_data_report_all %>%
  select(Year, `Health Board`, closure_category_admin, total_not_in_order) %>%
  filter(Year == 3 & `Health Board` == "Board") %>%
  mutate(closure_nio_percent = percent(closure_category_admin / total_not_in_order, accuracy = 0.1)) %>%
  pull(closure_nio_percent)

#Set full names if data is present
if (!is.na(closure_admin_nio)) {
  dcrs_data_wrangle_admin_closure <-  dcrs_data_wrangle_admin_closure %>%
    mutate(closure_category_admin = str_replace(closure_category_admin, 
                                                "spelling_error", "'certifying doctor spelling error'"),
           closure_category_admin = str_replace(closure_category_admin, 
                                                "deceased_details_incorrect", "'deceased details incorrect'"),
           closure_category_admin = str_replace(closure_category_admin, 
                                                "doctors_details_incorrect", "'certifying doctor's details incorrect'"),
           closure_category_admin = str_replace(closure_category_admin, 
                                                "abbreviations_used", "'abbreviations used'"),
           closure_category_admin = str_replace(closure_category_admin, 
                                                "time_incorrect", "'date or time of death incorrect'"),
           closure_category_admin = str_replace(closure_category_admin, 
                                                "incorrectly_complete", "'extra information (X box) incorrectly complete'"),
           closure_category_admin = str_replace(closure_category_admin, 
                                                "attendance_incorrect", "'attendance on the deceased incorrect'"),
           closure_category_admin = str_replace(closure_category_admin, 
                                                "address_incorrect", "'place of death address incorrect'"),
           closure_category_admin = str_replace(closure_category_admin, 
                                                "pm_information_incorrect", "'PM information incorrect'"),
           closure_category_admin = str_replace(closure_category_admin, 
                                                "legibility", "'legibility'"),
           closure_category_admin = str_replace(closure_category_admin, 
                                                "consultant_incorrect", "'consultant's name incorrect'"),
           closure_category_admin = str_replace(closure_category_admin, 
                                                "other_incorrect", "'other additional information incorrect'"),
           hb = case_when(`Health Board` == "Board" ~ "Board", TRUE ~ ""))
} else {#if no data is present create dummy dataframe
  dcrs_data_wrangle_admin_closure <- data.frame(hb = "Board", closure_category_admin = NA, admin_rate = NA, admin_percent = NA)}

#Find max admin percent
max_admin_value <- dcrs_data_wrangle_admin_closure %>% filter(hb == "Board") %>% filter(admin_rate == max(admin_rate)) %>% 
  filter(closure_category_admin == max(closure_category_admin)) %>% pull(admin_percent)


##Chart 7 admin summary text pt2##

#Find max admin category
max_admin <- dcrs_data_wrangle_admin_closure %>% 
  filter(hb == "Board") %>% filter(admin_rate == max(admin_rate))

max_admin_multiple <- max_admin %>%
  select(closure_category_admin) %>%
  mutate(admin_names = paste(closure_category_admin, collapse = ' and ')) %>%
  filter(closure_category_admin == max(closure_category_admin)) %>% 
  pull(admin_names)

#Test if individual cause is max or multiple cause share same max value
if (nrow(max_admin >0)) {
  max_admin_name <- max_admin %>%
    mutate(alt_value = case_when(nrow(max_admin) == 1 ~ closure_category_admin,
                                 nrow(max_admin) >= 2 ~ max_admin_multiple)) %>%
    filter(closure_category_admin == max(closure_category_admin)) %>% 
    pull(alt_value)
} else {max_admin_name = NA}

#Extra syntax if single or mutliple cause identified for max
admin_value1 <- max_admin %>%
  mutate(alt_value = case_when(nrow(max_admin) == 1 ~ "y",
                               nrow(max_admin) >= 2 ~ "ies")) %>%
  filter(closure_category_admin == max(closure_category_admin)) %>% 
  pull(alt_value)

#Extra syntax if single or mutliple cause identified for max
admin_value2 <- max_admin %>%
  mutate(alt_value = case_when(nrow(max_admin) == 1 ~ "",
                               nrow(max_admin) >= 2 ~ "each")) %>%
  filter(closure_category_admin == max(closure_category_admin)) %>% 
  pull(alt_value)

#set denominator to check is significant testing was possible
HB_n1 <- dcrs_data_report_all %>% select(Year, `Health Board`, closure_category_admin) %>% filter(Year == 2, `Health Board` == "Board") %>% pull(closure_category_admin)
HB_n2 <- dcrs_data_report_all %>% select(Year, `Health Board`, closure_category_admin) %>% filter(Year == 3, `Health Board` == "Board") %>% pull(closure_category_admin)

if (HB_n1 > 0 & HB_n2 >0) { #only create CI narrative if board has data for both years
  #Bar Scotland significant text extract
  increase_count <- dcrs_data_wrangle_admin_closure %>% filter(`Health Board` == "Scotland") %>% 
    summarise(count_cases = sum(direction == 'increase')) %>% pull(count_cases)
  decrease_count <- dcrs_data_wrangle_admin_closure %>% filter(`Health Board` == "Scotland") %>% 
    summarise(count_cases = sum(direction == 'decrease')) %>% pull(count_cases)
  
  #narrative if significant increase for Scotland is found  
  case_increase <- dcrs_data_wrangle_admin_closure %>%
    filter(direction == "increase",
           `Health Board` == "Scotland") %>%
    select(closure_category_admin, admin_rate, admin_rate_old) %>%
    mutate(case_percent = percent(admin_rate, accuracy = 0.1),
           case_percent_old = percent(admin_rate_old, accuracy = 0.1),
           case_increase = case_when(increase_count >= 1 ~ paste0(tolower(closure_category_admin), collapse = ', ')),
           case_change = paste("from", case_percent_old,  "to", case_percent, collapse = ', '),
           case_increase = case_when(increase_count >= 1 ~ paste(case_increase, "saw a significant increase", case_change),
                                     TRUE ~ "")) %>%
    filter(closure_category_admin == max(closure_category_admin)) %>% 
    pull(case_increase)
  
  #narrative if significant decrease for Scotland is found  
  case_decrease <- dcrs_data_wrangle_admin_closure %>%
    filter(direction == "decrease",
           `Health Board` == "Scotland") %>%
    select(closure_category_admin, admin_rate, admin_rate_old) %>%
    mutate(case_percent = percent(admin_rate, accuracy = 0.1),
           case_percent_old = percent(admin_rate_old, accuracy = 0.1),
           case_decrease = case_when(decrease_count >= 1 ~ paste0(tolower(closure_category_admin), collapse = ', ')),
           case_change = paste("from", case_percent_old,  "to", case_percent, collapse = ', '),
           case_decrease = case_when(decrease_count >= 1 ~ paste(case_decrease, "saw a significant decrease", case_change),
                                     TRUE ~ "")) %>%
    filter(closure_category_admin == max(closure_category_admin)) %>% 
    pull(case_decrease)
  
  #combine increase and decrease for full sigficance narrative
  if (increase_count >= 1 & decrease_count >= 1) {
    scot_admin_sig = paste0("+ At Scotland, ", case_increase, ", and ", case_decrease, " compared to ", year2, ".")
  } else if (increase_count >= 1) {
    scot_admin_sig = paste0("+ At Scotland, ", case_increase, " compared to ", year2, ".")
  } else if (decrease_count >= 1) {
    scot_admin_sig = paste0("+ At Scotland, ", case_decrease, " compared to ", year2, ".")
  } else { scot_admin_sig = "" }
  
  #HB
  increase_count <- dcrs_data_wrangle_admin_closure %>% filter(`Health Board` == "Board") %>% 
    summarise(count_cases = sum(direction == 'increase')) %>% pull(count_cases)
  decrease_count <- dcrs_data_wrangle_admin_closure %>% filter(`Health Board` == "Board") %>% 
    summarise(count_cases = sum(direction == 'decrease')) %>% pull(count_cases)
  
  #narrative if significant increase for board is found  
  case_increase <- dcrs_data_wrangle_admin_closure %>%
    filter(direction == "increase",
           `Health Board` == "Board") %>%
    select(closure_category_admin, admin_rate, admin_rate_old) %>%
    mutate(case_percent = percent(admin_rate, accuracy = 0.1),
           case_percent_old = percent(admin_rate_old, accuracy = 0.1),
           case_increase = case_when(increase_count >= 1 ~ paste0(tolower(closure_category_admin), collapse = ', ')),
           case_change = paste("from", case_percent_old,  "to", case_percent, collapse = ', '),
           case_increase = case_when(increase_count >= 1 ~ paste(case_increase, "saw a significant increase", case_change),
                                     TRUE ~ "")) %>%
    filter(closure_category_admin == max(closure_category_admin)) %>% 
    pull(case_increase)
  
  #narrative if significant decrease for board is found  
  case_decrease <- dcrs_data_wrangle_admin_closure %>%
    filter(direction == "decrease",
           `Health Board` == "Board") %>%
    select(closure_category_admin, admin_rate, admin_rate_old) %>%
    mutate(case_percent = percent(admin_rate, accuracy = 0.1),
           case_percent_old = percent(admin_rate_old, accuracy = 0.1),
           case_decrease = case_when(decrease_count >= 1 ~ paste0(tolower(closure_category_admin), collapse = ', ')),
           case_change = paste("from", case_percent_old,  "to", case_percent, collapse = ', '),
           case_decrease = case_when(decrease_count >= 1 ~ paste(case_decrease, "saw a significant decrease", case_change),
                                     TRUE ~ "")) %>%
    filter(closure_category_admin == max(closure_category_admin)) %>% 
    pull(case_decrease)
  
  #combine increase and decrease for full significance narrative
  if (increase_count >= 1 & decrease_count >= 1) {
    hb_admin_sig = paste0("+ At NHS ", Board, ", ",case_increase, ", and ", case_decrease, " compared to ", year2, ".")
  } else if (increase_count >= 1) {
    hb_admin_sig = paste0("+ At NHS ", Board, ", ",case_increase, " compared to ", year2, ".")
  } else if (decrease_count >= 1) {
    hb_admin_sig = paste0("+ At NHS ", Board, ", ",case_decrease, " compared to ", year2, ".")
  } else { hb_admin_sig = "" }
} else { scot_admin_sig = "" 
hb_admin_sig = ""}

# summary text PF ---------------------------------------------------------

##Chart 8 PF summary text pt1##

#PF total
total_PF <- dcrs_data_report_all %>% select(`Health Board`, total_report_to_pf) %>%
  filter(Year == 3, `Health Board` == "Board") %>% pull(total_report_to_pf)

#Set full names PF if data is present
if (!is.na(total_PF)) {
  PF_data_wrangle  <-  PF_data_wrangle  %>%
    mutate(total_report_to_pf = str_replace(total_report_to_pf, 
                                            "choking", "'choking'"),
           total_report_to_pf = str_replace(total_report_to_pf, 
                                            "concerns_over_care", "'concerns over care'"),
           total_report_to_pf = str_replace(total_report_to_pf, 
                                            "drug_related", "'drug related'"),
           total_report_to_pf = str_replace(total_report_to_pf, 
                                            "neglect_exposure", "'neglect or exposure'"),
           total_report_to_pf = str_replace(total_report_to_pf, 
                                            "fracture_trauma", "'fracture or trauma'"),
           total_report_to_pf = str_replace(total_report_to_pf, 
                                            "industrial_disease", "'industrial disease'"),
           total_report_to_pf = str_replace(total_report_to_pf, 
                                            "infectious_disease", "'infectious disease'"),
           total_report_to_pf = str_replace(total_report_to_pf, 
                                            "legal_order", "'legal order'"),
           total_report_to_pf = str_replace(total_report_to_pf, 
                                            "flagged_error", "'flagged in error'"),
           total_report_to_pf = str_replace(total_report_to_pf, 
                                            "other_report_pf", "'other report to PF'"),
           hb = case_when(`Health Board` == "Board" ~ "Board", TRUE ~ ""))
} else { #if no data is present create dummy dataframe
  PF_data_wrangle <- data.frame(hb = "Board", total_report_to_pf = NA, pf_rate = NA, pf_percent = NA)}

#Find max PF percent
max_PF_value <- PF_data_wrangle %>% filter(hb == "Board") %>% filter(pf_rate == max(pf_rate)) %>% 
  filter(total_report_to_pf == max(total_report_to_pf)) %>% pull(pf_percent)

#Second highest PF percent
max_PF_value2 <- PF_data_wrangle %>% filter(hb == "Board") %>% filter(pf_rate == max(pf_rate[pf_rate != max(pf_rate)])) %>% 
  pull(pf_percent)

#Second highest PF total
max_PF_name2 <- PF_data_wrangle %>% filter(hb == "Board") %>% filter(pf_rate == max(pf_rate[pf_rate != max(pf_rate)])) %>% 
  pull(total_report_to_pf)


##Chart 8 PF summary text pt2##

#Find max PF 
max_PF <- PF_data_wrangle  %>% 
  filter(hb == "Board") %>% filter(pf_rate == max(pf_rate))

#Highest PF name
max_PF_multiple <- max_PF %>%
  select(total_report_to_pf) %>%
  mutate(PF_names = paste(total_report_to_pf, collapse = ' and ')) %>%
  filter(total_report_to_pf == max(total_report_to_pf)) %>% 
  pull(PF_names)

#Test if individual cause is max or multiple cause share same max value
if (nrow(max_PF > 0)) {
  max_PF_name <- max_PF %>%
    mutate(alt_value = case_when(nrow(max_PF) == 1 ~ total_report_to_pf,
                                 nrow(max_PF) >= 2 ~ max_PF_multiple)) %>%
    filter(total_report_to_pf == max(total_report_to_pf)) %>% 
    pull(alt_value)
} else {max_PF_name = NA}

#Extra syntax if single or multiple cause identified for max
max_PF_txt1 <- max_PF %>%
  mutate(alt_value = case_when(nrow(max_PF) == 1 ~ "y",
                               nrow(max_PF) >= 2 ~ "ies")) %>%
  filter(total_report_to_pf == max(total_report_to_pf)) %>% 
  pull(alt_value)

#Extra syntax if single or multiple cause identified for max
max_PF_txt2 <- max_PF %>%
  mutate(alt_value = case_when(nrow(max_PF) == 1 ~ "",
                               nrow(max_PF) >= 2 ~ "each")) %>%
  filter(total_report_to_pf == max(total_report_to_pf)) %>% 
  pull(alt_value)

#find board denominators to check if CI narrative is needed
HB_n1 <- dcrs_data_report_all %>% select(Year, `Health Board`, total_report_to_pf) %>% filter(Year == 2, `Health Board` == "Board") %>% pull(total_report_to_pf)
HB_n2 <- dcrs_data_report_all %>% select(Year, `Health Board`, total_report_to_pf) %>% filter(Year == 3, `Health Board` == "Board") %>% pull(total_report_to_pf)

if (HB_n1 > 0 & HB_n2 > 0 ) { #only create if data is present for both years
  #Scotland Bar significant text
  increase_count <- PF_data_wrangle %>% filter(`Health Board` == "Scotland") %>% 
    summarise(count_cases = sum(direction == 'increase')) %>% pull(count_cases)
  decrease_count <- PF_data_wrangle %>% filter(`Health Board` == "Scotland") %>% 
    summarise(count_cases = sum(direction == 'decrease')) %>% pull(count_cases)
 
  #narrative if significant increase for Scotland is found   
  case_increase <- PF_data_wrangle %>%
    filter(direction == "increase",
           `Health Board` == "Scotland") %>%
    select(total_report_to_pf, pf_rate, pf_rate_old) %>%
    mutate(case_percent = percent(pf_rate, accuracy = 0.1),
           case_percent_old = percent(pf_rate_old, accuracy = 0.1),
           case_increase = case_when(increase_count >= 1 ~ paste0(tolower(total_report_to_pf), collapse = ', ')),
           case_change = paste("from", case_percent_old,  "to", case_percent, collapse = ', '),
           case_increase = case_when(increase_count >= 1 ~ paste(case_increase, "saw a significant increase", case_change),
                                     TRUE ~ "")) %>%
    filter(total_report_to_pf == max(total_report_to_pf)) %>% 
    pull(case_increase)
  
  #narrative if significant decrease for Scotland is found  
  case_decrease <- PF_data_wrangle %>%
    filter(direction == "decrease",
           `Health Board` == "Scotland") %>%
    select(total_report_to_pf, pf_rate, pf_rate_old) %>%
    mutate(case_percent = percent(pf_rate, accuracy = 0.1),
           case_percent_old = percent(pf_rate_old, accuracy = 0.1),
           case_decrease = case_when(decrease_count >= 1 ~ paste0(tolower(total_report_to_pf), collapse = ', ')),
           case_change = paste("from", case_percent_old,  "to", case_percent, collapse = ', '),
           case_decrease = case_when(decrease_count >= 1 ~ paste(case_decrease, "saw a significant decrease", case_change),
                                     TRUE ~ "")) %>%
    filter(total_report_to_pf == max(total_report_to_pf)) %>% 
    pull(case_decrease)
  
  #combine increase and decrease for full significance narrative
  if (increase_count >= 1 & decrease_count >= 1) {
    scot_pf_sig = paste0("+ At Scotland, ", case_increase, ", and ", case_decrease, " compared to ", year2, ".")
  } else if (increase_count >= 1) {
    scot_pf_sig = paste0("+ At Scotland, ", case_increase, " compared to ", year2, ".")
  } else if (decrease_count >= 1) {
    scot_pf_sig = paste0("+ At Scotland, ", case_decrease, " compared to ", year2, ".")
  } else { scot_pf_sig = "" }
  
  #HB
  increase_count <- PF_data_wrangle %>% filter(`Health Board` == "Board") %>% 
    summarise(count_cases = sum(direction == 'increase')) %>% pull(count_cases)
  decrease_count <- PF_data_wrangle %>% filter(`Health Board` == "Board") %>% 
    summarise(count_cases = sum(direction == 'decrease')) %>% pull(count_cases)
  
  #narrative if significant increase for board is found  
  case_increase <- PF_data_wrangle %>%
    filter(direction == "increase",
           `Health Board` == "Board") %>%
    select(total_report_to_pf, pf_rate, pf_rate_old) %>%
    mutate(case_percent = percent(pf_rate, accuracy = 0.1),
           case_percent_old = percent(pf_rate_old, accuracy = 0.1),
           case_increase = case_when(increase_count >= 1 ~ paste0(tolower(total_report_to_pf), collapse = ', ')),
           case_change = paste("from", case_percent_old,  "to", case_percent, collapse = ', '),
           case_increase = case_when(increase_count >= 1 ~ paste(case_increase, "saw a significant increase", case_change),
                                     TRUE ~ "")) %>%
    filter(total_report_to_pf == max(total_report_to_pf)) %>% 
    pull(case_increase)
  
  #narrative if significant decrease for board is found  
  case_decrease <- PF_data_wrangle %>%
    filter(direction == "decrease",
           `Health Board` == "Board") %>%
    select(total_report_to_pf, pf_rate, pf_rate_old) %>%
    mutate(case_percent = percent(pf_rate, accuracy = 0.1),
           case_percent_old = percent(pf_rate_old, accuracy = 0.1),
           case_decrease = case_when(decrease_count >= 1 ~ paste0(tolower(total_report_to_pf), collapse = ', ')),
           case_change = paste("from", case_percent_old,  "to", case_percent, collapse = ', '),
           case_decrease = case_when(decrease_count >= 1 ~ paste(case_decrease, "saw a significant decrease", case_change),
                                     TRUE ~ "")) %>%
    filter(total_report_to_pf == max(total_report_to_pf)) %>% 
    pull(case_decrease)
  
  #combine incrase and decrease for full significance narrative
  if (increase_count >= 1 & decrease_count >= 1) {
    hb_pf_sig = paste0("+ At NHS ", Board, ", ",case_increase, ", and ", case_decrease, " compared to ", year2, ".")
  } else if (increase_count >= 1) {
    hb_pf_sig = paste0("+ At NHS ", Board, ", ",case_increase, " compared to ", year2, ".")
  } else if (decrease_count >= 1) {
    hb_pf_sig = paste0("+ At NHS ", Board, ", ",case_decrease, " compared to ", year2, ".")
  } else { hb_pf_sig = "" }
} else { scot_pf_sig = "" 
hb_pf_sig = ""}

# summary text enquiry categories -----------------------------------------

#Chart 9 enquiry categories summary text

#Total enquiries  
total_enquiry <- dcrs_data_enquiry_all %>% select(`Health Board`, enquiry_category_total) %>%
  filter(Year == 3, `Health Board` == "Board") %>% pull(enquiry_category_total)

#update names if data is present
if (!is.na(total_enquiry)) {
  enquiry_data_wrangle <-  enquiry_data_wrangle %>%
    mutate(enquiry_category_total = str_replace(enquiry_category_total, 
                                                "gp_clinical_advice", "'GP clinical advice'"),
           enquiry_category_total = str_replace(enquiry_category_total, 
                                                "gp_process_advice", "'GP process advice'"),
           enquiry_category_total = str_replace(enquiry_category_total, 
                                                "hospital_clinical_advice", "'hospital clinical advice'"),
           enquiry_category_total = str_replace(enquiry_category_total, 
                                                "hospital_process_advice", "'hospital process advice'"),
           enquiry_category_total = str_replace(enquiry_category_total, 
                                                "hospice_clinical_advice", "'hospice clinical advice'"),
           enquiry_category_total = str_replace(enquiry_category_total, 
                                                "hospice_process_advice", "'hospice process advice'"),
           enquiry_category_total = str_replace(enquiry_category_total, 
                                                "funeral_director", "'funeral director'"),
           enquiry_category_total = str_replace(enquiry_category_total, 
                                                "informant_or_family", "'informant or family'"),
           enquiry_category_total = str_replace(enquiry_category_total, 
                                                "procurator_fiscal", "'procurator fiscal'"),
           enquiry_category_total = str_replace(enquiry_category_total, 
                                                "signposted", "'signposted'"),
           enquiry_category_total = str_replace(enquiry_category_total, 
                                                "other", "'other'"),
           hb = case_when(`Health Board` == "Board" ~ "Board", TRUE ~ ""))
} else { #if no data is present create dummy dataframe
  enquiry_data_wrangle <- data.frame(hb = "Board", enquiry_category_total = NA, enquiry_rate = NA, enquiry_percent = NA)}


#Max enquiry percent  
max_enquiry_value <- enquiry_data_wrangle %>% filter(hb == "Board") %>% filter(enquiry_rate == max(enquiry_rate)) %>% 
  pull(enquiry_percent)

#Max enquiry category name  
max_enquiry_name <- enquiry_data_wrangle %>% filter(hb == "Board") %>% filter(enquiry_rate == max(enquiry_rate)) %>% 
  pull(enquiry_category_total)

#Max enquiry total  
max_enquiry_number <- dcrs_data_enquiry_all %>%
  select(Year, `Health Board`, gp_clinical_advice, gp_process_advice, hospital_clinical_advice, hospital_process_advice, hospice_clinical_advice,
         hospice_process_advice, funeral_director, informant_or_family, procurator_fiscal, signposted, other) %>%
  filter(Year == 3, `Health Board` == "Board") %>%
  pivot_longer(cols = !c(Year, `Health Board`), names_to = 'enquiry_category', values_to = 'enquiry_number') %>%
  filter(enquiry_number == max(enquiry_number)) %>% pull(enquiry_number)

#Second highest percent  
max_enquiry_value2 <- enquiry_data_wrangle %>% 
  filter(hb == "Board") %>% filter(enquiry_rate == max(enquiry_rate[enquiry_rate != max(enquiry_rate)])) %>% pull(enquiry_percent)

#Second highest category name  
max_enquiry_name2 <- enquiry_data_wrangle %>% 
  filter(hb == "Board") %>% filter(enquiry_rate == max(enquiry_rate[enquiry_rate != max(enquiry_rate)])) %>% pull(enquiry_category_total)

#Second highest total  
max_enquiry_number2 <- dcrs_data_enquiry_all %>%
  select(Year, `Health Board`, gp_clinical_advice, gp_process_advice, hospital_clinical_advice, hospital_process_advice, hospice_clinical_advice,
         hospice_process_advice, funeral_director, informant_or_family, procurator_fiscal, signposted, other) %>%
  filter(Year == 3, `Health Board` == "Board") %>%
  pivot_longer(cols = !c(Year, `Health Board`), names_to = 'enquiry_category', values_to = 'enquiry_number') %>%
  filter(enquiry_number == max(enquiry_number[enquiry_number != max(enquiry_number)])) %>% pull(enquiry_number)

#get board denominators for both years to check if CI narrative is needed
if (Board == "National Golden Jubilee") {HB_n1 =0} else {
  HB_n1 <- dcrs_data_enquiry_all %>% select(Year, `Health Board`, enquiry_category_total) %>% filter(Year == 2, `Health Board` == "Board") %>% pull(enquiry_category_total)
}
HB_n2 <- dcrs_data_enquiry_all %>% select(Year, `Health Board`, enquiry_category_total) %>% filter(Year == 3, `Health Board` == "Board") %>% pull(enquiry_category_total)

if (HB_n1 > 0 & HB_n2 > 0 ) { #only create narrative if data is present for both years
  #Scotland bar significance text
  increase_count <- enquiry_data_wrangle %>% filter(`Health Board` == "Scotland") %>% 
    summarise(count_cases = sum(direction == 'increase')) %>% pull(count_cases)
  decrease_count <- enquiry_data_wrangle %>% filter(`Health Board` == "Scotland") %>% 
    summarise(count_cases = sum(direction == 'decrease')) %>% pull(count_cases)
  
  #narrative if significant increase for Scotland is found  
  case_increase <- enquiry_data_wrangle %>%
    filter(direction == "increase",
           `Health Board` == "Scotland") %>%
    select(enquiry_category_total, enquiry_rate, enquiry_rate_old) %>%
    mutate(case_percent = percent(enquiry_rate, accuracy = 0.1),
           case_percent_old = percent(enquiry_rate_old, accuracy = 0.1),
           case_increase = case_when(increase_count >= 1 ~ paste0(tolower(enquiry_category_total), collapse = ', ')),
           case_change = paste("from", case_percent_old,  "to", case_percent, collapse = ', '),
           case_increase = case_when(increase_count >= 1 ~ paste(case_increase, "saw a significant increase", case_change),
                                     TRUE ~ "")) %>%
    filter(enquiry_category_total == max(enquiry_category_total)) %>% 
    pull(case_increase)
  
  #narrative if significant decrease for Scotland is found  
  case_decrease <- enquiry_data_wrangle %>%
    filter(direction == "decrease",
           `Health Board` == "Scotland") %>%
    select(enquiry_category_total, enquiry_rate, enquiry_rate_old) %>%
    mutate(case_percent = percent(enquiry_rate, accuracy = 0.1),
           case_percent_old = percent(enquiry_rate_old, accuracy = 0.1),
           case_decrease = case_when(decrease_count >= 1 ~ paste0(tolower(enquiry_category_total), collapse = ', ')),
           case_change = paste("from", case_percent_old,  "to", case_percent, collapse = ', '),
           case_decrease = case_when(decrease_count >= 1 ~ paste(case_decrease, "saw a significant decrease", case_change),
                                     TRUE ~ "")) %>%
    filter(enquiry_category_total == max(enquiry_category_total)) %>% 
    pull(case_decrease)
  
  #combine increase and decrease for full significance narrative
  if (increase_count >= 1 & decrease_count >= 1) {
    scot_enquiry_sig = paste0("+ At Scotland, ", case_increase, ", and ", case_decrease, " compared to ", year2, ".")
  } else if (increase_count >= 1) {
    scot_enquiry_sig = paste0("+ At Scotland, ", case_increase, " compared to ", year2, ".")
  } else if (decrease_count >= 1) {
    scot_enquiry_sig = paste0("+ At Scotland, ", case_decrease, " compared to ", year2, ".")
  } else { scot_enquiry_sig = "" }
  
  #HB
  increase_count <- enquiry_data_wrangle %>% filter(`Health Board` == "Board") %>% 
    summarise(count_cases = sum(direction == 'increase')) %>% pull(count_cases)
  decrease_count <- enquiry_data_wrangle %>% filter(`Health Board` == "Board") %>% 
    summarise(count_cases = sum(direction == 'decrease')) %>% pull(count_cases)
  
  #narrative if significant increase for board is found  
  case_increase <- enquiry_data_wrangle %>%
    filter(direction == "increase",
           `Health Board` == "Board") %>%
    select(enquiry_category_total, enquiry_rate, enquiry_rate_old) %>%
    mutate(case_percent = percent(enquiry_rate, accuracy = 0.1),
           case_percent_old = percent(enquiry_rate_old, accuracy = 0.1),
           case_increase = case_when(increase_count >= 1 ~ paste0(tolower(enquiry_category_total), collapse = ', ')),
           case_change = paste("from", case_percent_old,  "to", case_percent, collapse = ', '),
           case_increase = case_when(increase_count >= 1 ~ paste(case_increase, "saw a significant increase", case_change),
                                     TRUE ~ "")) %>%
    filter(enquiry_category_total == max(enquiry_category_total)) %>% 
    pull(case_increase)
  
  #narrative if significant decrease for board is found  
  case_decrease <- enquiry_data_wrangle %>%
    filter(direction == "decrease",
           `Health Board` == "Board") %>%
    select(enquiry_category_total, enquiry_rate, enquiry_rate_old) %>%
    mutate(case_percent = percent(enquiry_rate, accuracy = 0.1),
           case_percent_old = percent(enquiry_rate_old, accuracy = 0.1),
           case_decrease = case_when(decrease_count >= 1 ~ paste0(tolower(enquiry_category_total), collapse = ', ')),
           case_change = paste("from", case_percent_old,  "to", case_percent, collapse = ', '),
           case_decrease = case_when(decrease_count >= 1 ~ paste(case_decrease, "saw a significant decrease", case_change),
                                     TRUE ~ "")) %>%
    filter(enquiry_category_total == max(enquiry_category_total)) %>% 
    pull(case_decrease)
  
  #combine increase and decrease for full significance narrative
  if (increase_count >= 1 & decrease_count >= 1) {
    hb_enquiry_sig = paste0("+ At NHS ", Board, ", ",case_increase, ", and ", case_decrease, " compared to ", year2, ".")
  } else if (increase_count >= 1) {
    hb_enquiry_sig = paste0("+ At NHS ", Board, ", ",case_increase, " compared to ", year2, ".")
  } else if (decrease_count >= 1) {
    hb_enquiry_sig = paste0("+ At NHS ", Board, ", ",case_decrease, " compared to ", year2, ".")
  } else { hb_enquiry_sig = "" }
} else { scot_enquiry_sig = "" 
hb_enquiry_sig = ""}

###Hospital significant changes for table and chart
hosp_check2 <- DCRS_Data_Hosp_chart_sig %>% filter(direction1 != "" | direction2 != "")


if (nrow(hosp_check != 0) & nrow(hosp_check2 != 0)) {#only create narrative if hospital data and significant changes present
hosp_sig <- DCRS_Data_Hosp_chart_sig %>%
  mutate(change1 = case_when(direction1 == "increase" ~ paste0(Locname, " saw a significant increase in '", 
                                                               tolower(Review), "' compared to ", year1),
                             direction1 == "decrease" ~ paste0(Locname, " saw a significant decrease in '", 
                                                               tolower(Review), "' compared to ", year1), 
                             TRUE ~ ""),
         change2 = case_when(direction2 == "increase" ~ paste0(Locname, " saw a significant increase in '", 
                                                               tolower(Review), "' compared to ", year2),
                             direction2 == "decrease" ~ paste0(Locname, " saw a significant decrease in '", 
                                                               tolower(Review), "' compared to ", year2), 
                             TRUE ~ ""),
         change = paste0(change1, change2, collapse = ''),
         change = case_when(direction1 != "" | direction2 != "" ~ paste0("+ ", change),
                            TRUE ~ "")) %>%
  filter(change != "") %>%
  filter(case_rate1 == max(case_rate1)) %>% pull(change)
} else {hosp_sig <- ""}

###Hospice significant changes for table and chart
hospice_check2 <- DCRS_Data_Hospice_chart_sig %>% filter(direction1 != "" | direction2 != "")

if (nrow(hospice_check != 0) & nrow(hospice_check2 != 0)) {#only create narrative if hospice data and significant changes present
hospice_sig <- DCRS_Data_Hospice_chart_sig %>%
  mutate(change1 = case_when(direction1 == "increase" ~ paste0(Locname, " saw a significant increase in '", 
                                                               tolower(Review), "' compared to ", year1),
                             direction1 == "decrease" ~ paste0(Locname, " saw a significant decrease in '", 
                                                               tolower(Review), "' compared to ", year1), 
                             TRUE ~ ""),
         change2 = case_when(direction2 == "increase" ~ paste0(Locname, " saw a significant increase in '", 
                                                               tolower(Review), "' compared to ", year2),
                             direction2 == "decrease" ~ paste0(Locname, " saw a significant decrease in '", 
                                                               tolower(Review), "' compared to ", year2), 
                             TRUE ~ ""),
         change = paste0(change1, change2, collapse = ''),
         change = case_when(direction1 != "" | direction2 != "" ~ paste0("+ ", change),
                            TRUE ~ "")) %>%
  filter(change != "") %>%
  filter(case_rate1 == max(case_rate1)) %>% pull(change)
} else {hospice_sig <- ""}
 
# summary text cause of death---------------------------------------------

##Chart 12 cause of death text##

#Identify top cause
top_cause <- DCRS_Data_Chapter_rank %>% filter(top5 == 1)

#Combine mutliple causes if more than 1 are top ranked
top_cause_multiple <- DCRS_Data_Chapter_rank %>% filter(top5 == 1) %>% 
  select(DiagGrp) %>%
  mutate(DiagGrp = paste(DiagGrp, collapse = ' and ')) %>%
  filter(row_number()==1) %>%
  pull(DiagGrp)

#Get top name depending if one or multiple causes
top_cause_name <- top_cause %>%
  mutate(alt_value = case_when(nrow(top_cause) == 1 ~ DiagGrp,
                               nrow(top_cause) >= 2 ~ top_cause_multiple)) %>%
  filter(row_number()==1) %>%
  pull(alt_value)

#Max total
top_cause_number <- DCRS_Data_Chapter2 %>% group_by(DiagGrp) %>% summarise(case_total = sum(case_total)) %>% filter(case_total == max(case_total)) %>% pull(case_total)

#Max location type (while stepping dow capital letters)
top_cause_place <- DCRS_Data_Chapter2 %>% filter(DiagGrp == top_cause_name) %>% filter(case_total == max(case_total)) %>% pull(instgrp2)
top_cause_place <- tolower(top_cause_place)

#Max percent
top_cause_place_percentage <- DCRS_Data_Chapter2 %>% filter(DiagGrp == top_cause_name) %>% filter(case_total == max(case_total)) %>% pull(case_percentage)

#Step down capital letter and contain in quotations
top_cause_name <- paste0("'",tolower(top_cause_name),"'")

#check if significant change were detected
diag_check2 <- DCRS_Data_Chapter_sig %>% filter(direction != "")

if (nrow(diag_check != 0) & nrow(diag_check2 != 0)) { #only create narrative if significant changes found
###significant changes for table and chart
diag_sig <- DCRS_Data_Chapter_sig %>%
  mutate(change = case_when(direction == "increase" ~ paste0(DiagGrp, " saw a significant increase in '", 
                                                               tolower(instgrp2), "' compared to ", year1),
                             direction == "decrease" ~ paste0(DiagGrp, " saw a significant decrease in '", 
                                                               tolower(instgrp2), "' compared to ", year1), 
                             TRUE ~ ""),
         change = paste0(change, collapse = ''),
         change = case_when(direction != "" ~ paste0("+ ", change),
                            TRUE ~ "")) %>%
  filter(change != "") %>%
  filter(case_rate2 == max(case_rate2)) %>% pull(change)
} else {diag_sig <- ""}


###Summary of significant changes found throughout to include in key points

#Board level chart and tables 
sig_change_message <- data.frame(hb_closure_sig, hb_cause_sig, hb_pf_sig, hb_admin_sig, hb_enquiry_sig, 
                                 hb_table2_sig, hb_table3_sig, hb_table4_sig) %>%
  mutate(hb_closure_sig = case_when(hb_closure_sig != "" ~ " closure categories,", TRUE ~ ""), 
         hb_cause_sig = case_when(hb_cause_sig != "" ~ " closure categories of cause too vague,", TRUE ~ ""),
         hb_pf_sig = case_when(hb_pf_sig != "" ~ " categories of cases referred to procurator fiscal,", TRUE ~ ""),
         hb_admin_sig = case_when(hb_admin_sig != "" ~ " closure categories with administrative errors,", TRUE ~ ""),
         hb_enquiry_sig = case_when(hb_enquiry_sig != "" ~ " categories of cases with enquiry calls,", TRUE ~ ""),
         hb_table2_sig = case_when(hb_table2_sig != "" ~ " categories of standard case status,", TRUE ~ ""),
         hb_table3_sig = case_when(hb_table3_sig != "" ~ " categories of MCCD status,", TRUE ~ ""),
         hb_table4_sig = case_when(hb_table4_sig != "" ~ " categories of breached case reasons,", TRUE ~ ""),
         change_message = case_when(hb_closure_sig != "" | hb_cause_sig != "" | hb_pf_sig != "" | hb_admin_sig != "" |
                                      hb_enquiry_sig != "" | hb_table2_sig != "" | hb_table3_sig != "" | hb_table4_sig != "" ~
           paste0("+ At NHS ", Board, hb_table2_sig, hb_table3_sig, hb_table4_sig, hb_closure_sig, hb_cause_sig, 
                  hb_pf_sig, hb_admin_sig, hb_enquiry_sig, " saw significant changes compared to previous years"),
           TRUE ~ "")) %>%
  pull(change_message)

#Hospital and hospice level charts and tables
hosp_change_message <- data.frame(hosp_sig, hospice_sig, diag_sig) %>%
  mutate(hosp_sig = case_when(hosp_sig != "" ~ " categories of standard case status at hospital,", TRUE ~ ""), 
         hospice_sig = case_when(hospice_sig != "" ~ " categories of standard case status at hospice,", TRUE ~ ""),
         diag_sig = case_when(diag_sig != "" ~ " causes of death,", TRUE ~ ""),
         change_message = case_when(hosp_sig != "" | hospice_sig != "" |  diag_sig != "" ~ paste0("+ At NHS ", Board, hosp_sig, hospice_sig, diag_sig, 
                                 " saw significant changes compared to previous years"), TRUE ~ "")) %>%
  pull(change_message)
