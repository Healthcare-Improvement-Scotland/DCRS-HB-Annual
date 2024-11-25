
library(tidyverse)
library(readxl)

#setwd("N:/Evidence/DMBI/Death Cert/Report - Healthboards Quarterly/2024 R Report")

load("Data/DCRS_NRS_linked_final.Rda") 

#The 'inst_suffix' variable is created based on the last letter (suffix) of the
# 'INSTITUTION' variable
# substr(oldvrb, start, stop)
DCRS_DATA_Loc <- DCRS_NRS_linked_final %>%
#  filter(YEAR == 'Year 8') %>%
    mutate(inst_suffix = substr(Institution, 5, 5),
           #Create the 'instgrp1' variable (associated insitution)          
           instgrp1 = character(length(Institution)),
           instgrp1 = ifelse(inst_suffix %in% c("H","K","J") &
           !(Institution %in% c("T318H","N183H","C422H","S203K",
                                "S121K","N182H","G501K","L102K","W105H")), 1, instgrp1),
           instgrp1 = ifelse(Institution=="D201N", 2, instgrp1),
           instgrp1 = ifelse(Institution %in% c("T318H", "A227V", "V103V","N404V",  "G501K",
                                                  "G414V", "C118J", "C407V", "C306V", "C413V",
                                                  "C415K", "H220V", "L102K", "S121K", "S110V",
                                                  "S121V", "T317V", "W105H", "W102V", "N183H",
                                                  "N182H", "C422H", "G201V", "S203K", "G583V",
                                                  "S340V", "G584V", "S204K"), 3, instgrp1),
           instgrp1 = ifelse(inst_suffix %in% c("V","S") &
                            !(Institution %in% c("A237V", "V213V", "T201V", "N101V",
                                                      "L330V", "L305V", "T301V", "T104V",
                                                      "G502V", "G517V", "S332V", "G323V",
                                                      "S124V", "G412V", "T334V", "S341V",
                                                      "G201v", "S121V", "W102V", "V103V",
                                                      "S110V", "H220V", "A227V", "C306V",
                                                      "T317V", "N404V", "C407V", "C413V",
                                                      "G414V", "G583V", "S340V", "G584V",
                                                      "H249V", "H250V", "H251V", "L310V",
                                                      "B101K")), 4, instgrp1),
           instgrp1 = ifelse(Institution== "C415K", 4, instgrp1),
           instgrp1 = ifelse(inst_suffix=="R" | Institution %in% c("H249V", "H250V", "H251V", "L310V",
                                                                   "N564C", "Y007T"), 5, instgrp1),
           instgrp1 = ifelse(Institution %in% c("A237V", "V213V", "T201V","N101V",
                                                  "L330V", "L305V", "T301V", "T104V",
                                                  "G502V", "G517V", "S332V", "G323V",
                                                  "S124V", "G412V", "T334V", "S341V"), 6, instgrp1),
           instgrp1 = ifelse(inst_suffix %in% c('B','C','E','T','L','Q','W', 'M','P'),
                            7, instgrp1),
###add labels at the 7 levels
           instgrp1 = factor(instgrp1, levels=c(1,2,3,4,5,6,7),
                                        labels=c("NHS Hospital", "Home / Private Address",
                                                "Hospice", "Private Care Homes and Care Homes",
                                                "Homes for the Elderly", "Private Hospital",
                                                "Other")))

#table(DCRS_DATA_Loc$instgrp1, exclude = FALSE)
###Create more concise location of death groups for analysis
DCRS_DATA_Loc <-DCRS_DATA_Loc %>%
  mutate(instgrp2= case_when(instgrp1=="NHS Hospital"~1,
                             instgrp1=="Home / Private Address"~2,
                             instgrp1=="Hospice"~3,
                             instgrp1 %in% c("Private Care Homes and Care Homes",
                                             "Homes for the Elderly",
                                             "Private Hospital",
                                             "Other")~4),
#add labels at the 4 levels
                             instgrp2 = factor(instgrp2,
                                         levels=c(1,2,3,4),
                                         labels=c("NHS Hospital", "Home / Private Address",
                                                  "Hospice", "Care Home Service / Other Inst.")))

#table(DCRS_DATA_Loc$instgrp2)

#https://www.opendata.nhs.scot/dataset/annual-hospital-beds-information/resource/1ac6f087-acb3-481f-be93-528ef0c55ade  source of location lookup?
###Read in location lookup
location1 <- read.csv("Data/location.csv")
NHS_INSTITUTION_PLACE_OF_DEATH <- location1 %>%
  select(Location, Locname, Postcode, Summary)%>%
  rename(Institution = Location)

#Link with location lookup for hospital/site location
DCRS_DATA_Loc <- left_join(DCRS_DATA_Loc, NHS_INSTITUTION_PLACE_OF_DEATH, by="Institution")

###Find the health board of occurence
DCRS_DATA_Loc <- DCRS_DATA_Loc %>%
  mutate(`Health.board.of.occurrence` = ifelse(Locname == "Golden Jubilee National Hospital", 18, `Health.board.of.occurrence`),
         `Health.board.of.occurrence` = factor(`Health.board.of.occurrence`,
                levels=c(2, 3, 4, 5, 6, 7, 10, 11, 12, 13, 14, 15, 16, 17, 18),
                labels=c("Grampian", "Tayside", "Fife", "Lothian", "Borders", "Forth Valley",
                         "Lanarkshire", "Ayrshire and Arran", "Dumfries and Galloway", "Orkney",
                         "Shetland", "Western Isles", "Greater Glasgow and Clyde", "Highland",
                         "National Golden Jubilee")))

#levels(DCRS_DATA_Loc$`Health.board.of.occurrence`)

###Find health board for all cases
DCRS_DATA_Loc <- DCRS_DATA_Loc %>%
    mutate(inst_prefix = substr(Institution, 1, 1),
           Health_Board = character(length(`Health.board.of.occurrence`)),
           Health_Board = ifelse(inst_prefix=="A", "Ayrshire and Arran", Health_Board),
           Health_Board = ifelse(inst_prefix=="B", "Borders", Health_Board),
           Health_Board = ifelse(inst_prefix %in% c("C","G"), "Greater Glasgow and Clyde", Health_Board),
           Health_Board = ifelse(inst_prefix=="F", "Fife", Health_Board),
           Health_Board = ifelse((inst_prefix=="H" | Institution %in% c("C106H", "C108H", "C113H", "C121H","C122H", "C118J")), "Highland", Health_Board),
           Health_Board = ifelse(inst_prefix=="L", "Lanarkshire", Health_Board),
           Health_Board = ifelse(inst_prefix=="N", "Grampian", Health_Board),
           Health_Board = ifelse(inst_prefix=="R", "Orkney", Health_Board),
           Health_Board = ifelse(inst_prefix=="S", "Lothian", Health_Board),
           Health_Board = ifelse(inst_prefix=="T", "Tayside", Health_Board),
           Health_Board = ifelse(inst_prefix=="V", "Forth Valley", Health_Board),
           Health_Board = ifelse(inst_prefix=="W", "Western Isles", Health_Board),
           Health_Board = ifelse(inst_prefix=="Y", "Dumfries and Galloway", Health_Board),
           Health_Board = ifelse(inst_prefix=="Z", "Shetland", Health_Board),
           Health_Board = ifelse(Institution=="D102H", "National Golden Jubilee", Health_Board),
           Health_Board = ifelse(Health_Board=="", `Health.board.of.occurrence`, Health_Board))

#table(DCRS_DATA_Loc$Health_Board)

####Add diagnosis information for underlying cause of death

DCRS_DATA_Loc_DIAG <- DCRS_DATA_Loc %>%
   mutate(MainDiag = `Underlying.cause.of.death.code`,
  ########### DiagGrp
          DiagGrp = character(length(MainDiag)),
          DiagGrp = ifelse(substr(MainDiag, 1, 1) =="A" | substr(MainDiag, 1, 1) =="B", 1, DiagGrp),
          DiagGrp = ifelse(substr(MainDiag, 1, 1) %in% c("C"), 2, DiagGrp),
          DiagGrp = ifelse(substr(MainDiag, 1, 2) %in% c("D1","D2", "D3", "D4"), 2, DiagGrp),
          DiagGrp = ifelse(substr(MainDiag, 1, 3) %in% c("D00"), 2, DiagGrp),
          DiagGrp = ifelse(substr(MainDiag, 1, 2) %in% c("D5","D6","D8"), 3, DiagGrp),
          DiagGrp = ifelse(substr(MainDiag, 1, 1) == "E", 4, DiagGrp),
          DiagGrp = ifelse(substr(MainDiag, 1, 1) == "F", 5, DiagGrp),
          DiagGrp = ifelse(substr(MainDiag, 1, 2) %in% c("G0", "G1","G2","G3","G4","G6", "G7","G8","G9", "H7"), 6, DiagGrp),
          DiagGrp = ifelse(substr(MainDiag, 1, 3) %in% c("H95"), 6, DiagGrp),
          DiagGrp = ifelse(substr(MainDiag, 1, 4) == "H709", 8, DiagGrp),
          DiagGrp = ifelse(substr(MainDiag, 1, 1) == "I", 9, DiagGrp),
          DiagGrp = ifelse(substr(MainDiag, 1, 1) == "J", 10, DiagGrp),
          DiagGrp = ifelse(substr(MainDiag, 1, 1) == "K", 11, DiagGrp),
          DiagGrp = ifelse(substr(MainDiag, 1, 1) == "L", 12, DiagGrp),
          DiagGrp = ifelse(substr(MainDiag, 1, 1) == "M", 13, DiagGrp),
          DiagGrp = ifelse(substr(MainDiag, 1, 1) == "N", 14, DiagGrp),
          DiagGrp = ifelse(substr(MainDiag, 1, 1) == "O", 15, DiagGrp),
          DiagGrp = ifelse(substr(MainDiag, 1, 1) == "P", 16, DiagGrp),
          DiagGrp = ifelse(substr(MainDiag, 1, 1) == "Q", 17, DiagGrp),
          DiagGrp = ifelse(substr(MainDiag, 1, 1) == "R", 18, DiagGrp),
          DiagGrp = ifelse(substr(MainDiag, 1, 1) == "S" |
                           substr(MainDiag, 1, 1) == "T" , 19, DiagGrp),
          DiagGrp = ifelse(substr(MainDiag, 1, 1) == "V" |
                           substr(MainDiag, 1, 1) == "W" |
                           substr(MainDiag, 1, 1) == "X" |
                           substr(MainDiag, 1, 1) == "Y" , 20, DiagGrp),
          DiagGrp = ifelse(substr(MainDiag, 1, 1) == "Z", 21, DiagGrp),
          DiagGrp = ifelse(substr(MainDiag, 1, 1) == "U", 22, DiagGrp),
          DiagGrp = factor(DiagGrp,
                          levels=c(1,2,3,4,5,6,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22),
                          labels=c("Certain infectious and parasitic diseases",
                                   "All Neoplasms",
                                   "Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism",
                                   "Endocrine, nutritional and metabolic diseases",
                                   "Mental and behavioural disorders",
                                   "Diseases of the nervous system and the sense organs",
                                   "Diseases of the nervous system and the sense organs",
                                   "Diseases of the circulatory systems",
                                   "Diseases of the respiratory system",
                                   "Diseases of the digestive system",
                                   "Diseases of the skin and subcutaneous tissue",
                                   "Diseases of the musculoskeletal system and connective tissue",
                                   "Diseases of the genitourinary system",
                                   "Pregnancy, childbirth and the puerperium",
                                   "Certain conditions originating in the perinatal period",
                                   "Congenital malformations, deformations and chromosomal abnormalities",
                                   "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified",
                                   "Injury, poisoning and certain other consequences of external causes",
                                   "External causes of morbidity and mortality",
                                   "Factors influencing health status and contact with health services",
                                   "Codes for special purposes")))

save(DCRS_DATA_Loc_DIAG, file="Data/DCRS_DATA_Loc_DIAG.Rda")

#table(DCRS_DATA_Loc_DIAG$DiagGrp)
