##########  Sets up the data and creates Scotland and board level run charts for MCCDs no in order used in part 2 of the report

Board <- hdr

# chart 1 Scotland run chart ----------------------------------------------

#set scotland run chart data
#Aggregate to Scotland level
dcrs_data_scot_run <- dcrs_data %>%
  filter(`Case Type` == "Standard") %>%
  #Update start date when moving on a year
  filter(`Created On` < end_date & `Created On` >= "2020-01-01") %>%  
  #Set all dates to the 1st of the month to group by month
  mutate(`Created On` = floor_date(`Created On`, "month"),
         `Health Board` = 'Scotland') %>%
  group_by(`Created On`, `Health Board`) %>%
  summarise(total_in_order = sum(`Case Status` == "Case in Order"),
            total_not_in_order = sum(`Case Status` == "Case not in Order")
            , .groups = "rowwise")  %>%
  #Dates to exclude due to reduction of DCRS service during pandemic
  mutate(month_exclude = case_when((`Created On` >= "2020-03-01" & `Created On` <= "2020-08-01") |
                                     (`Created On` >= "2020-11-01" & `Created On` <= "2021-05-01") |
                                     (`Created On` >= "2021-10-01" & `Created On` <= "2022-03-01") |
                                     (`Created On` >= "2023-01-01" & `Created On` <= "2023-03-01") ~ "skip"),
         case_total = (total_in_order + total_not_in_order),
         percent_nio = (total_not_in_order / case_total)*100) 

#Add April 2020 point as no data was collected during this month
apr_data <- head(dcrs_data_scot_run, 1) %>%
  mutate(`Health Board` = "Scotland",
         `Created On` = "2020-04-01", 
         total_in_order = 0, 
         total_not_in_order = 0, 
         month_exclude = "skip", 
         case_total = 0, 
         percent_nio = 0)

#Combine April point with the dataset
dcrs_data_scot_run <- rbind(dcrs_data_scot_run, apr_data) 

#scotland run chart

scot_run <- make_runchart(
  dcrs_data_scot_run,
  observation = "percent_nio",
  date = "Created On",
  specs = "month_exclude",
  baselinepts = 12,
  temporarypts = 9,
  onmedian = 0.01
) %>%
  mutate(grp = c(0,cumsum(abs(diff(is.na(month_exclude))))),  #add groupings required for line breaks
         grp2 = case_when(median_name != lag(median_name) ~ 1, TRUE ~ 0),
         grp2 = cumsum(grp2) + grp)

scot_run_summary <- scot_run %>%
  summarise_medians(observation_type = "Percentage") %>% 
  filter(!is.na(median_label_display)) %>%
  mutate(`Created On` = date_start) %>%
  select(`Created On`, median_change_direction, median_change_pct_display, median_display, median_name_display, median_label_display)

scot_run <- scot_run %>% left_join(scot_run_summary, by = "Created On") %>%
  mutate(percent_nio = percent_nio/100,
         median = median/100)


scot_run_plot <- ggplot(scot_run, aes(x = `Created On`)) +
  geom_line(data = scot_run %>% filter(is.na(month_exclude)), 
            aes(y=percent_nio, group = grp) , 
            colour = "#004380", linewidth = 1)+
  ggtitle("Chart 1: Run chart of monthly precentage MCCDs 'not in order' Scotland") + 
  geom_point(aes(y=percent_nio, group = 1), 
             colour = "#004380", size = 2) +  
  geom_point(data = scot_run %>% filter(month_exclude == "skip"), aes(y=percent_nio, group = 1), 
             colour = "#a6a6a6", size = 2) + 
  geom_line(data = scot_run %>% filter(is.na(month_exclude) & str_detect(scot_run$median_name, "Extended Median")), 
            aes(y=median, group = grp2), 
            linetype = "longdash", colour = "#F27822", linewidth = 1) +
  geom_line(data = scot_run %>% filter(is.na(month_exclude) & str_detect(scot_run$median_name, "Baseline Median") | str_detect(scot_run$median_name, "Temporary Median")), 
            aes(y=median, group = grp2), 
            linetype = "solid", colour = "#F27822", linewidth = 1) +
  geom_point(data = scot_run %>% filter(!is.na(observation_note) & str_detect(observation_note,"Shift")),
             aes(y=percent_nio, group = 1), 
             colour = "#ffcd04", size = 2) +
  #  geom_text(data = dataset %>% filter(is.na(exclude)), aes(y = median, label = base_label), 
  #           size= 3) +
  geom_text_repel(data = scot_run %>% filter(is.na(month_exclude)), aes(y = max(percent_nio), label = median_label_display, segment.color = NA), 
            size= 2.5, nudge_y= -0.15, hjust = 0) +   
  geom_point(data = scot_run %>% filter(!(is.na(trend))),
                                        aes(y=percent_nio, group = 1), shape = 1, size = 3, colour = "#00b0f0") +
  scale_y_continuous(limits=c(0, max(as.numeric(scot_run$percent_nio))+0.1*max(as.numeric(scot_run$percent_nio))), expand = c(0, 0), labels = percent) +
  xlab("Month") + ylab("Percent") +
  scale_x_date(breaks = seq(min(scot_run$`Created On`), max(scot_run$`Created On`), length.out = 20),
               limits = c(min(scot_run$`Created On`), max(scot_run$`Created On`)),
               date_labels = "%b\n%Y")+
  theme_classic()+
  theme(plot.title = element_text(size = 10),
        plot.title.position = "plot",
        axis.title.x = element_text(size = 8),
        axis.text.x = element_text(size = 7.5, angle = 45, hjust = 1),
        axis.title.y = element_text(size = 8))

scot_run_plot

ggsave(scot_run_plot, 
       filename = "Charts/scot_run.png",
       device = "png",
       height = 3.2, width = 6, units = "in")

# summary text Scotland run chart -----------------------------------------

#set text for scotland run chart

#find beaseline median
scot_start_base <- scot_run_summary %>%
  filter(substr(median_name_display,1, 4) == "Base") %>%
  pull(median_display)

#find current median
scot_current_base <- scot_run_summary %>%
  filter(substr(median_name_display,1, 4) == "Curr") %>%
  pull(median_display)

#calculate percent difference between baseline and current median
scot_percent_change <-  scot_run_summary %>%
  filter(substr(median_name_display,1, 4) == "Curr") %>%
  pull(median_change_pct_display)

count_scot <- scot_run_summary %>%
  mutate(c_flag = case_when(substr(median_name_display,1, 4) == "Curr" ~ 1, TRUE ~ 0)) %>%
  summarise(c_flag = sum(c_flag)) %>% pull(c_flag)
  
if(count_scot > 0 ) {
#find if measure has improved/deteriorated/not changed and set text
scot_change_direction <-  scot_run_summary %>%
  filter(substr(median_name_display,1, 4) == "Curr") %>%
  pull(median_change_direction)
}else{ 
  scot_change_direction <- "" }

if(scot_change_direction == "Decrease") {
  scot_change_status = "improved"
} else if (scot_change_direction == "Increase") {
  scot_change_status = "deteriorated"
} else
{ scot_change_status = "not changed"
}

#count how many points in current median
scot_count_median <- scot_run %>%
  filter(median_name != "na") %>%
  mutate(base_n = as.numeric(substr(median_name, 17, 17))) %>%
  filter(base_n == max(base_n)) %>%
  count(`base_n`) %>%
  pull(n)

#find if full/temporary median and set text
if(scot_count_median < 12){
  scot_median_text = "temporary"
} else
{ scot_median_text = "current"
} 

#Load run chart status from previous iteration going back to 2015
library(readxl)
RunChartStatement_scot <- read_excel("RunChartStatement.xlsx") %>%
  filter(NHSBoard == "Scotland") %>%
  pull(Statement)

#find if measure has improved/deteriorated/not changed and set main text for narrative
if (scot_change_direction == "Decrease"){
  scot_change_status = paste(RunChartStatement_scot, "More recently, from January 2020, Scotland has improved by", scot_percent_change,
                              "from", scot_start_base, "to a", scot_median_text, "median of", scot_current_base)
} else if (scot_change_direction == "Increase"){
  scot_change_status = paste(RunChartStatement_scot, "More recently, from January 2020, Scotland has deteriorated by", scot_percent_change,
                              "from", scot_start_base, "to a", scot_median_text, "median of", scot_current_base)
} else
{ scot_change_status = paste(RunChartStatement_scot, "More recently in analysis from January 2020 there has been no change in the percentage 'not in order'") }


#Flag if there is a sustained shift in the last 12 months
sustained_flag <- scot_run %>%
  filter(`Created On` >= (max(`Created On`) - 365)) %>%
  mutate(sustained_flag = case_when(substr(median_name,1, 4) == "Base" ~ 1, TRUE ~ 0)) %>%
  summarise(sustained_flag = sum(sustained_flag)) %>%
  pull(sustained_flag)

#Flag if there is an ongoing shift
shift_flag <- scot_run %>%
  filter(`Created On` >= (max(`Created On`) - 365)) %>%
  mutate(shift_flag = case_when(median < percent_nio & observation_note == "Shift" ~ 1,
                                median > percent_nio & observation_note == "Shift" ~ -1,
                                TRUE ~ 0)) %>%
  filter(`Created On` == max(`Created On`)) %>%
  pull(shift_flag)

#Add narrative if a recent sustained change or shift identified 
if(scot_change_direction == "Decrease" & sustained_flag > 1){
  scot_new_change_status = "Scotland has seen recent improvement with a new median below the baseline."
} else if (scot_change_direction == "Increase" & sustained_flag > 1){
  scot_new_change_status = "Scotland has seen recent deterioration with a new median above the baseline."
} else if (shift_flag == -1){
  scot_new_change_status = "Scotland has also seen signs of improvement with an ongoing shift above the current median."
} else if (shift_flag == 1){
  scot_new_change_status = "Scotland has also seen signs of deterioration with an ongoing shift above the current median."
} else
{ scot_new_change_status = "" }

# chart 2 board run chart -------------------------------------------------

#set board run chart data
#Aggregate to Board level
dcrs_data_run <- dcrs_data %>%
  filter(`Case Type` == "Standard") %>%
  #Update start date when moving on a year
  filter(`Created On` < end_date & `Created On` >= "2020-01-01") %>%  
  filter(`Health Board` == Board) %>%
  #Set all dates to the 1st of the month to group by month
  mutate(`Created On` = floor_date(`Created On`, "month")) %>%
  group_by(`Created On`, `Health Board`) %>%
  summarise(total_in_order = sum(`Case Status` == "Case in Order"),
            total_not_in_order = sum(`Case Status` == "Case not in Order")
            , .groups = "rowwise") %>%
  #Dates to exclude due to reduction of DCRS service during pandemic
  mutate(month_exclude = case_when((`Created On` >= "2020-03-01" & `Created On` <= "2020-08-01") |
                                     (`Created On` >= "2020-11-01" & `Created On` <= "2021-05-01") |
                                     (`Created On` >= "2021-10-01" & `Created On` <= "2022-03-01") |
                                     (`Created On` >= "2023-01-01" & `Created On` <= "2023-03-01") ~ "skip"),
         case_total = (total_in_order + total_not_in_order),
         percent_nio = (total_not_in_order / case_total*100),
         percent_nio = case_when(percent_nio >= 0 ~ percent_nio, TRUE ~ 0))

#Add April 2020 point as no data was collected during this month
apr_data <- head(dcrs_data_run, 1) %>%
  mutate(`Health Board` = Board,
         `Created On` = "2020-04-01", 
         total_in_order = 0, 
         total_not_in_order = 0, 
         month_exclude = "skip", 
         case_total = 0, 
         percent_nio = 0)

#Combine April point with the dataset
dcrs_data_run <- rbind(dcrs_data_run, apr_data)


#r set board run chart data quarter (for boards on quarterly aggregates)

dcrs_data_run_quarter <- dcrs_data %>%
  filter(`Case Type` == "Standard") %>%
  #Update start date when moving on a year
  filter(`Created On` < end_date & `Created On` >= "2020-01-01") %>%  
  filter(`Health Board` == Board) %>%
  #Set all dates to the 1st of the quarter to group by quarter
  mutate(`Created On` = floor_date(`Created On`, "quarter")) %>%
  group_by(`Created On`, `Health Board`) %>%
  summarise(total_in_order = sum(`Case Status` == "Case in Order"),
            total_not_in_order = sum(`Case Status` == "Case not in Order")
            , .groups = "rowwise") %>%
  #Dates to exclude due to reduction of DCRS service during pandemic
  mutate(month_exclude = case_when((`Created On` >= "2020-03-01" & `Created On` <= "2022-03-01") |
                                     (`Created On` >= "2023-01-01" & `Created On` <= "2023-03-01") ~ "skip"),
         case_total = (total_in_order + total_not_in_order),
         percent_nio = (total_not_in_order / case_total*100))


#board run chart

#If quarterly analysis then source and run the quarterly run chart function, else source and run the monthly run chart function
if(Board == "Borders" | Board == "Dumfries and Galloway"){

  #Run chart analysis using the quarterly runchart function  
  hb_run <- make_runchart(
    dcrs_data_run_quarter,
    observation = "percent_nio",
    date = "Created On",
    specs = "month_exclude",
    baselinepts = 12,
    temporarypts = 9,
    onmedian = 0.01
  ) %>%
    mutate(grp = c(0,cumsum(abs(diff(is.na(month_exclude))))),  #add groupings required for line breaks
           grp2 = case_when(median_name != lag(median_name) ~ 1, TRUE ~ 0),
           grp2 = cumsum(grp2) + grp)
  
  hb_run_summary <- hb_run %>%
    summarise_medians(observation_type = "Percentage") %>% 
    filter(!is.na(median_label_display)) %>%
    mutate(`Created On` = date_start) %>%
    select(`Created On`, median_change_direction, median_change_pct_display, median_display, median_name_display, median_label_display)
  
  hb_run <- hb_run %>% left_join(hb_run_summary, by = "Created On") %>%
    mutate(percent_nio = percent_nio/100,
           median = median/100)
} else {
  
  hb_run <- make_runchart(
    dcrs_data_run,
    observation = "percent_nio",
    date = "Created On",
    specs = "month_exclude",
    baselinepts = 12,
    temporarypts = 9,
    onmedian = 0.01
  ) %>%
    mutate(grp = c(0,cumsum(abs(diff(is.na(month_exclude))))),  #add groupings required for line breaks
           grp2 = case_when(median_name != lag(median_name) ~ 1, TRUE ~ 0),
           grp2 = cumsum(grp2) + grp)
  
  hb_run_summary <- hb_run %>%
    summarise_medians(observation_type = "Percentage") %>% 
    filter(!is.na(median_label_display)) %>%
    mutate(`Created On` = date_start) %>%
    select(`Created On`, median_change_direction, median_change_pct_display, median_display, median_name_display, median_label_display)
  
  hb_run <- hb_run %>% left_join(hb_run_summary, by = "Created On") %>%
    mutate(percent_nio = percent_nio/100,
           median = median/100)
  }

#dynamic chart title for mothly/quarterly analysis
chart_title <-   if(Board == "Borders" | Board == "Dumfries and Galloway") {
    ggtitle(paste0("Chart 2: Run chart of quarterly percentage MCCDs ‘not in order' NHS ", Board))} else {
    ggtitle(paste0("Chart 2: Run chart of monthly percentage MCCDs ‘not in order' NHS ", Board))}

x_axis <- if(Board == "Borders" | Board == "Dumfries and Galloway"){
  xlab("Quarter") } else {xlab("Month")}


#Chart all data from the run chart analysis
board_run <- ggplot(hb_run, aes(x = `Created On`)) +
  geom_line(data = hb_run %>% filter(is.na(month_exclude)), 
            aes(y=percent_nio, group = grp) , 
            colour = "#004380", linewidth = 1)+
  chart_title + 
  geom_point(aes(y=percent_nio, group = 1), 
             colour = "#004380", size = 2) +  
  geom_point(data = hb_run %>% filter(month_exclude == "skip"), aes(y=percent_nio, group = 1), 
             colour = "#a6a6a6", size = 2) + 
  geom_line(data = hb_run %>% filter(is.na(month_exclude) & str_detect(hb_run$median_name, "Extended Median")), 
            aes(y=median, group = grp2), 
            linetype = "longdash", colour = "#F27822", linewidth = 1) +
  geom_line(data = hb_run %>% filter(is.na(month_exclude) & str_detect(hb_run$median_name, "Baseline Median") | str_detect(hb_run$median_name, "Temporary Median")), 
            aes(y=median, group = grp2), 
            linetype = "solid", colour = "#F27822", linewidth = 1) +
  geom_point(data = hb_run %>% filter(!is.na(observation_note) & str_detect(observation_note,"Shift")),
             aes(y=percent_nio, group = 1), 
             colour = "#ffcd04", size = 2) +
  #  geom_text(data = dataset %>% filter(is.na(exclude)), aes(y = median, label = base_label), 
  #           size= 3) +
  geom_text_repel(data = hb_run %>% filter(is.na(month_exclude)), aes(y = max(percent_nio), label = median_label_display, segment.color = NA), 
            size= 2.5, nudge_y= 0.02, hjust = 0) +   
  geom_point(data = hb_run %>% filter(!(is.na(trend))),
             aes(y=percent_nio, group = 1), shape = 1, size = 3, colour = "#00b0f0") +
  scale_y_continuous(limits=c(0, max(as.numeric(hb_run$percent_nio))+0.1*max(as.numeric(hb_run$percent_nio))), expand = c(0, 0), labels = percent) +
  x_axis + ylab("Percent") +
  scale_x_date(breaks = seq(min(hb_run$`Created On`), max(hb_run$`Created On`), length.out = 20),
               limits = c(min(hb_run$`Created On`), max(hb_run$`Created On`)),
               date_labels = "%b\n%Y")+
  theme_classic()+
  theme(plot.title = element_text(size = 10),
        plot.title.position = "plot",
        axis.title.x = element_text(size = 8),
        axis.text.x = element_text(size = 7.5, angle = 45, hjust = 1),
        axis.title.y = element_text(size = 8))

board_run

ggsave(board_run, 
       filename = "Charts/board_run.png",
       device = "png",
       height = 3.2, width = 6, units = "in")

# summary text board run chart --------------------------------------------

#set text for board run chart

#Load run chart status from previous iteration going back to 2015
RunChartStatement <- read_excel("RunChartStatement.xlsx") %>%
  filter(NHSBoard == Board) %>%
  pull(Statement)

#find beaseline median
hb_start_base <- hb_run_summary %>%
  filter(substr(median_name_display,1, 4) == "Base") %>%
  pull(median_display)

#find current median
hb_current_base <- hb_run_summary %>%
  filter(substr(median_name_display,1, 4) == "Curr") %>%
  pull(median_display)

#find beaseline median
hb_start_base2 <- hb_run %>%
  filter(`Created On` != "2020-04-01",
    `grp2` == 0,
    !is.na(median_note != "na")) %>%
  pull(median)

#find current median
hb_current_base2 <- hb_run %>%
  filter(`Created On` != "2020-04-01") %>%
  filter(`grp2` == max(`grp2`),
  `Created On` == max(`Created On`)) %>%
  pull(median)

hb_percent_change <-  hb_run_summary %>%
  filter(substr(median_name_display,1, 4) == "Curr") %>%
  pull(median_change_pct_display)

hb_median <- hb_run %>%
  mutate(median_value = case_when(!is.na(median_name) ~ 1, TRUE ~ 0)) %>%
  summarise(median_value = sum(median_value))

if(hb_median > 0) {
#count how many points in current median
hb_count_median <- hb_run %>%
  filter(median_name != "na") %>%
  mutate(base_n = as.numeric(substr(median_name, 17, 17)),
         base_n = case_when(is.na(base_n) ~ 0, TRUE ~ base_n)) %>%
  filter(base_n == max(base_n)) %>%
  count(`base_n`) %>%
  pull(n)
} else {hb_count_median <- 0}

#find if full/temporary median and set text
if(hb_count_median < 12){
  hb_median_text = "temporary"
} else
{ hb_median_text = "current"
} 


#find if measure has improved/deteriorated/not changed and set main text for narrative
if(Board == "Western Isles" | Board == "Shetland" | Board == "Orkney" | Board == "National Golden Jubilee"){
  hb_change_status = "The board reports very small numbers of certificates 'not in order' so a board level run chart cannot be produced"
} else if (hb_start_base2 > hb_current_base2){
  hb_change_status = paste(RunChartStatement, "More recently in analysis from January 2020 the board has improved by", hb_percent_change,
                              "from", hb_start_base, "to a", hb_median_text, "median of", hb_current_base)
} else if (hb_start_base2 < hb_current_base2){
  hb_change_status = paste(RunChartStatement, "More recently in analysis from January 2020 the board has deteriorated by", hb_percent_change,
                              "from", hb_start_base, "to a", hb_median_text, "median of", hb_current_base)
} else
{ hb_change_status = paste(RunChartStatement, "More recently in analysis from January 2020 there has been no change in the percentage 'not in order'") }

#Flag if there is a sustained shift in the last 12 months
sustained_flag <- hb_run %>%
  filter(`Created On` >= (max(`Created On`) - 365)) %>%
  mutate(sustained_flag = case_when(substr(median_name,1, 4) == "Base" ~ 1, TRUE ~ 0)) %>%
  summarise(sustained_flag = sum(sustained_flag)) %>%
  pull(sustained_flag)

#Flag if there is an ongoing shift
shift_flag <- hb_run %>%
  filter(`Created On` >= (max(`Created On`) - 365)) %>%
  mutate(shift_flag = case_when(median < percent_nio & observation_note == "Shift" ~ 1,
                                median > percent_nio & observation_note == "Shift" ~ -1,
                                TRUE ~ 0)) %>%
  filter(`Created On` == max(`Created On`)) %>%
  pull(shift_flag)

#Add narrative if a recent sustained change or shift identified 
if(Board == "Western Isles" | Board == "Shetland" | Board == "Orkney" | Board == "National Golden Jubilee"){
  hb_new_change_status = ""
} else if (hb_start_base2 > hb_current_base2 & sustained_flag > 1){
  hb_new_change_status = "The board has seen recent improvement with a new median below the baseline."
} else if (hb_start_base2 < hb_current_base2 & sustained_flag > 1){
  hb_new_change_status = "The board has seen recent deterioration with a new median above the baseline."
} else if (shift_flag == -1){
  hb_new_change_status = "The board has also seen signs of improvement with an ongoing shift above the current median."
} else if (shift_flag == 1){
  hb_new_change_status = "The board has also seen signs of deterioration with an ongoing shift above the current median."
} else
{ hb_new_change_status = "" }

