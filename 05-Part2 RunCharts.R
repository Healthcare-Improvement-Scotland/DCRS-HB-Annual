##########  Sets up the data and creates Scotland and board level run charts for MCCDs no in order used in part 2 of the report

Board <- hdr

# chart 1 Scotland run chart ----------------------------------------------

#set scotland run chart data
#Aggregate to Scotland level
dcrs_data_scot_run <- dcrs_data %>%
  filter(`Case Type` == "Standard") %>%
  #Update start date when moving on a year
  filter(`Created On` < end_date & `Created On` >= "2019-01-01") %>%  
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
                                     (`Created On` >= "2023-01-01" & `Created On` <= "2023-03-01") ~ 1),
         case_total = (total_in_order + total_not_in_order),
         percent_nio = (total_not_in_order / case_total)) 

#Add April 2020 point as no data was collected during this month
apr_data <- head(dcrs_data_scot_run, 1) %>%
  mutate(`Health Board` = "Scotland",
         `Created On` = "2020-04-01", 
         total_in_order = 0, 
         total_not_in_order = 0, 
         month_exclude = 1, 
         case_total = 0, 
         percent_nio = 0)

#Combine April point with the dataset
dcrs_data_scot_run <- rbind(dcrs_data_scot_run, apr_data) 



#scotland run chart

library(plyr)
library(zoo)

#imports runchart function
source("Functions/stable.R") 
#debugonce(RunChart)

#Run chart analysis using the runchart function
dataset <- dcrs_data_scot_run %>%
  # filter(percent_nio == "NA")%>%
  mutate(Month = ymd(`Created On`))%>%
  group_by(`Health Board`)%>%
  nest()%>%
  mutate(RunData = map(data, ~RunChart(measure = .x$percent_nio, subgroup = .x$Month, exclude = .x$month_exclude, shiftsens = "none", percentage =TRUE)))%>%
  unnest(cols = c(RunData))%>%
  select(-data) %>%
  mutate(grp = c(0,cumsum(abs(diff(is.na(exclude))))))

#Chart all data from the run chart analysis
scot_run <- ggplot(dataset, aes(x = subgroup)) +
  geom_line(data = dataset %>% filter(is.na(exclude)), 
            aes(y=measure, group = grp) , 
            colour = "#004380", linewidth = 1)+
  ggtitle(paste0("Chart 1: Run chart of monthly percentage MCCDs ‘not in order' Scotland"))+ 
  geom_point(aes(y=measure, group = 1), 
             colour = "#004380", size = 2) +  
  geom_point(data = dataset %>% filter(exclude == 1), aes(y=measure, group = 1), 
             colour = "#a6a6a6", size = 2) + 
  geom_line(data = dataset %>% filter(is.na(exclude)), 
            aes(y=median, group = paste(base_n, grp)), 
            linetype = "longdash", colour = "#F27822", linewidth = 1) +
  geom_line(data = dataset %>% filter(is.na(exclude)), 
            aes(y=baselines, group = paste(base_n, grp)), 
            linetype = "solid", colour = "#F27822", linewidth = 1) +
  geom_point(aes(y=highlight, group = 1), 
             colour = "#ffcd04", size = 2) +
  geom_text(data = dataset %>% filter(is.na(exclude)), aes(y = max(measure), label = base_label), 
            size= 3, nudge_y= 0.02, hjust = 0) +
  geom_point(aes(y=as.numeric(trendind), group = 1), shape = 1, size = 3, colour = "#00b0f0") + 
  scale_y_continuous(limits=c(0, max(as.numeric(dataset$measure))+0.1*max(as.numeric(dataset$measure))), expand = c(0, 0), labels = percent) +
  xlab("Month") + ylab("Percent") +
  scale_x_date(breaks = seq(min(dataset$subgroup), max(dataset$subgroup), length.out = 20),
               limits = c(min(dataset$subgroup), max(dataset$subgroup)),
               date_labels = "%b\n%Y")+
  theme_classic()+
  theme(plot.title = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(size = 7.5, angle = 45, hjust = 1),
        axis.title.y = element_text(size = 10))
#debugonce(RunChart)

scot_run

detach("package:plyr", unload=TRUE)
detach("package:zoo", unload=TRUE)


# summary text Scotland run chart -----------------------------------------


#set text for scotland run chart

#find beaseline median
scot_start_base <- dataset %>%
  filter(`base_n` == 1) %>%
  filter(base_label != "na") %>%
  pull(median)

#find current median
scot_current_base <- dataset %>%
  filter(base_n != "na") %>%
  filter(base_n == max(base_n)) %>%
  filter(base_label != "na") %>%
  pull(median)

#calculate percent difference between baseline and current median
scot_percent_change <- percent(1-(scot_current_base/scot_start_base), accuracy = 0.1)

#find if measure has improved/deteriorated/not changed and set text
if(scot_start_base > scot_current_base){
  scot_change_status = "improved"
} else if (scot_start_base < scot_current_base){
  scot_change_status = "deteriorated"
} else
{ scot_change_status = "not changed"
}

#count how many points in current median
scot_count_median <- dataset %>%
  filter(base_n != "na") %>%
  filter(base_n == max(base_n)) %>%
  count(`base_n`) %>%
  pull(n)

#find if full/temporary median and set text
if(scot_count_median < 12){
  scot_median_text = "temporary"
} else
{ scot_median_text = "new"
} 

#Load run chart status from previous iteration going back to 2015
library(readxl)
RunChartStatement_scot <- read_excel("RunChartStatement.xlsx") %>%
  filter(NHSBoard == "Scotland") %>%
  pull(Statement)

#find if measure has improved/deteriorated/not changed and set main text for narrative
if (scot_start_base > scot_current_base){
  scot_change_status = paste(RunChartStatement_scot, "More recently, from January 2019, Scotland has improved by", scot_percent_change,
                              "from", percent(scot_start_base, accuracy = 0.1), "to a", scot_median_text, "median of", percent(scot_current_base, accuracy = 0.1))
} else if (scot_start_base < scot_current_base){
  scot_change_status = paste(RunChartStatement_scot, "More recently, from January 2019, Scotland has deteriorated by", scot_percent_change,
                              "from", percent(scot_start_base, accuracy = 0.1), "to a", scot_median_text, "median of", percent(scot_current_base, accuracy = 0.1))
} else
{ scot_change_status = paste(RunChartStatement_scot, "More recently there has been no change in the percentage 'not in order'") }

#Extract all data from run chart
scot_run_data <- scot_run$data

#Flag if there is a sustained shift in the last 12 months
sustained_flag <- scot_run_data %>%
  mutate(sustained_flag = case_when(baselines != "na" & base_n == max(base_n) ~ 1,
                                    TRUE ~ 0)) %>%
  filter(subgroup >= (max(subgroup) - 365)) %>%
  summarise(sustained_flag = sum(sustained_flag)) %>%
  pull(sustained_flag)

#Flag if there is an ongoing shift
shift_flag <- scot_run_data %>%
  mutate(shift_flag = case_when(median < highlight ~ 1,
                                median > highlight ~ -1,
                                TRUE ~ 0)) %>%
  filter(subgroup == max(subgroup)) %>%
  pull(shift_flag)

#Add narrative if a recent sustained change or shift identified 
if(scot_start_base > scot_current_base & sustained_flag > 1){
  scot_new_change_status = "Scotland has seen recent improvement with a new median below the baseline."
} else if (scot_start_base < scot_current_base & sustained_flag > 1){
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
  filter(`Created On` < end_date & `Created On` >= "2019-01-01") %>%  
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
                                     (`Created On` >= "2023-01-01" & `Created On` <= "2023-03-01") ~ 1),
         case_total = (total_in_order + total_not_in_order),
         percent_nio = (total_not_in_order / case_total))

#Add April 2020 point as no data was collected during this month
apr_data <- head(dcrs_data_run, 1) %>%
  mutate(`Health Board` = Board,
         `Created On` = "2020-04-01", 
         total_in_order = 0, 
         total_not_in_order = 0, 
         month_exclude = 1, 
         case_total = 0, 
         percent_nio = 0)

#Combine April point with the dataset
dcrs_data_run <- rbind(dcrs_data_run, apr_data)


#r set board run chart data quarter (for boards on quarterly aggregates)

dcrs_data_run_quarter <- dcrs_data %>%
  filter(`Case Type` == "Standard") %>%
  #Update start date when moving on a year
  filter(`Created On` < end_date & `Created On` >= "2019-01-01") %>%  
  filter(`Health Board` == Board) %>%
  #Set all dates to the 1st of the quarter to group by quarter
  mutate(`Created On` = floor_date(`Created On`, "quarter")) %>%
  group_by(`Created On`, `Health Board`) %>%
  summarise(total_in_order = sum(`Case Status` == "Case in Order"),
            total_not_in_order = sum(`Case Status` == "Case not in Order")
            , .groups = "rowwise") %>%
  #Dates to exclude due to reduction of DCRS service during pandemic
  mutate(month_exclude = case_when((`Created On` >= "2020-03-01" & `Created On` <= "2022-03-01") |
                                     (`Created On` >= "2023-01-01" & `Created On` <= "2023-03-01") ~ 1),
         case_total = (total_in_order + total_not_in_order),
         percent_nio = (total_not_in_order / case_total))

#Add April 2020 point as no data was collected during this month
apr_data <- head(dcrs_data_run_quarter, 1) %>%
  mutate(`Health Board` = Board,
         `Created On` = "2020-04-01", 
         total_in_order = 0, 
         total_not_in_order = 0, 
         month_exclude = 1, 
         case_total = 0, 
         percent_nio = 0)

#Combine April point with the dataset
dcrs_data_run_quarter <- rbind(dcrs_data_run_quarter, apr_data) 

#board run chart

library(plyr)
library(zoo)

#install.packages("ggrepel")
library(ggrepel)

#If quarterly analysis then source and run the quarterly run chart function, else source and run the monthly run chart function
if(Board == "Borders" | Board == "Dumfries and Galloway"){
  source("Functions/stable2.R") #imports quarterly runchart function

  #Run chart analysis using the quarterly runchart function  
  dataset <- dcrs_data_run_quarter %>%
    # filter(percent_nio == "NA")%>%
    mutate(Month = ymd(`Created On`))%>%
    group_by(`Health Board`)%>%
    nest()%>%
    mutate(RunData = map(data, ~RunChart(measure = .x$percent_nio, subgroup = .x$Month, exclude = .x$month_exclude, shiftsens = "none", percentage =TRUE)))%>%
    unnest(cols = c(RunData))%>%
    select(-data) %>%
    mutate(grp = c(0,cumsum(abs(diff(is.na(exclude))))))
} else
{source("Functions/stable.R") #imports runchart function
  
  #Run chart analysis using the monthly runchart function
  dataset <- dcrs_data_run %>%
    # filter(percent_nio == "NA")%>%
    mutate(Month = ymd(`Created On`))%>%
    group_by(`Health Board`)%>%
    nest()%>%
    mutate(RunData = map(data, ~RunChart(measure = .x$percent_nio, subgroup = .x$Month, exclude = .x$month_exclude, shiftsens = "none", percentage =TRUE)))%>%
    unnest(cols = c(RunData))%>%
    select(-data) %>%
    mutate(grp = c(0,cumsum(abs(diff(is.na(exclude))))))}

#dynamic chart title for mothly/quarterly analysis
chart_title <-   if(Board == "Borders" | Board == "Dumfries and Galloway") {
    ggtitle(paste0("Chart 2: Run chart of quarterly percentage MCCDs ‘not in order' NHS ", Board))} else {
    ggtitle(paste0("Chart 2: Run chart of monthly percentage MCCDs ‘not in order' NHS ", Board))}

#Chart all data from the run chart analysis
board_run <- ggplot(dataset, aes(x = subgroup)) +
  geom_line(data = dataset %>% filter(is.na(exclude)), 
            aes(y=measure, group = grp) , 
            colour = "#004380", linewidth = 1)+
  chart_title + 
  geom_point(aes(y=measure, group = 1), 
             colour = "#004380", size = 2) +  
  geom_point(data = dataset %>% filter(exclude == 1), aes(y=measure, group = 1), 
             colour = "#a6a6a6", size = 2) + 
  geom_line(data = dataset %>% filter(is.na(exclude)), 
            aes(y=median, group = paste(base_n, grp)), 
            linetype = "longdash", colour = "#F27822", linewidth = 1) +
  geom_line(data = dataset %>% filter(is.na(exclude)), 
            aes(y=baselines, group = paste(base_n, grp)), 
            linetype = "solid", colour = "#F27822", linewidth = 1) +
  geom_point(aes(y=highlight, group = 1), 
             colour = "#ffcd04", size = 2) +
  #  geom_text(data = dataset %>% filter(is.na(exclude)), aes(y = median, label = base_label), 
  #           size= 3) +
  geom_text(data = dataset %>% filter(is.na(exclude)), aes(y = max(measure), label = base_label), 
                  size= 3, nudge_y= 0.02, hjust = 0) +   
  geom_point(aes(y=as.numeric(trendind), group = 1), shape = 1, size = 3, colour = "#00b0f0") +
  scale_y_continuous(limits=c(0, max(as.numeric(dataset$measure))+0.1*max(as.numeric(dataset$measure))), expand = c(0, 0), labels = percent) +
  xlab("Month") + ylab("Percent") +
  scale_x_date(breaks = seq(min(dataset$subgroup), max(dataset$subgroup), length.out = 20),
               limits = c(min(dataset$subgroup), max(dataset$subgroup)),
               date_labels = "%b\n%Y")+
  theme_classic()+
  theme(plot.title = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(size = 7.5, angle = 45, hjust = 1),
        axis.title.y = element_text(size = 10))

board_run

detach("package:plyr", unload=TRUE)
detach("package:zoo", unload=TRUE)



# summary text board run chart --------------------------------------------

#set text for board run chart

#Load run chart status from previous iteration going back to 2015
RunChartStatement <- read_excel("RunChartStatement.xlsx") %>%
  filter(NHSBoard == Board) %>%
  pull(Statement)

#find beaseline median
board_start_base <- dataset %>%
  filter(`base_n` == 1) %>%
  filter(base_label != "na") %>%
  pull(median)

#find current median
board_current_base <- dataset %>%
  filter(base_n != "na") %>%
  filter(base_n == max(base_n)) %>%
  filter(base_label != "na") %>%
  pull(median)

#calculate percent difference between baseline and current median
board_percent_change <- percent(1-(board_current_base/board_start_base), accuracy = 0.1)

#count how many points in current median
board_count_median <- dataset %>%
  filter(base_n != "na") %>%
  filter(base_n == max(base_n)) %>%
  count(`base_n`) %>%
  pull(n)

#find if full/temporary median and set text
if(board_count_median < 12){
  board_median_text = "temporary"
} else
{ board_median_text = "current"
} 

#find if measure has improved/deteriorated/not changed and set main text for narrative
if(Board == "Western Isles" | Board == "Shetland" | Board == "Orkney" | Board == "National Golden Jubilee"){
  board_change_status = "The board reports very small numbers of certificates 'not in order' so a board level run chart cannot be produced."
} else if (board_start_base > board_current_base){
  board_change_status = paste(RunChartStatement, "More recently, from January 2019, the board has improved by", board_percent_change,
                              "from", percent(board_start_base, accuracy = 0.1), "to a", board_median_text, "median of", percent(board_current_base, accuracy = 0.1))
} else if (board_start_base < board_current_base){
  board_change_status = paste(RunChartStatement, "More recently, from January 2019, the board has deteriorated by", board_percent_change,
                              "from", percent(board_start_base, accuracy = 0.1), "to a", board_median_text, "median of", percent(board_current_base, accuracy = 0.1))
} else
{ board_change_status = paste(RunChartStatement, "More recently there has been no change in the percentage 'not in order'") }

#Extract all data from run chart
board_run_data <- board_run$data

#Flag if there is a sustained shift in the last 12 months
sustained_flag <- board_run_data %>%
  mutate(sustained_flag = case_when(baselines != "na" & base_n == max(base_n) ~ 1,
                                    TRUE ~ 0)) %>%
  filter(subgroup >= (max(subgroup) - 365)) %>%
  summarise(sustained_flag = sum(sustained_flag)) %>%
  pull(sustained_flag)

#Flag if there is an ongoing shift
shift_flag <- board_run_data %>%
  mutate(shift_flag = case_when(median < highlight ~ 1,
                                median > highlight ~ -1,
                                TRUE ~ 0)) %>%
  filter(subgroup == max(subgroup)) %>%
  pull(shift_flag)

#Add narrative if a recent sustained change or shift identified 
if(Board == "Western Isles" | Board == "Shetland" | Board == "Orkney" | Board == "National Golden Jubilee"){
  board_new_change_status = ""
} else if (board_start_base > board_current_base & sustained_flag > 1){
  board_new_change_status = "The board has seen recent improvement with a new median below the baseline."
} else if (board_start_base < board_current_base & sustained_flag > 1){
  board_new_change_status = "The board has seen recent deterioration with a new median above the baseline."
} else if (shift_flag == -1){
  board_new_change_status = "The board has also seen signs of improvement with an ongoing shift above the current median."
} else if (shift_flag == 1){
  board_new_change_status = "The board has also seen signs of deterioration with an ongoing shift above the current median."
} else
{ board_new_change_status = "" }