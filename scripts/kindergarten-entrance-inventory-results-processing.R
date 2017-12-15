library(dplyr)
library(datapkg)
library(readxl)
library(tidyr)

##################################################################
#
# Processing Script for Kindergarten Entrance Inventory Results
# Created by Jenna Daly
# On 12/15/2017
#
##################################################################

#Setup environment
sub_folders <- list.files()
raw_location <- grep("raw", sub_folders, value=T)
path_to_raw <- (paste0(getwd(), "/", raw_location))
xwalk <- dir(path_to_raw, recursive=T, pattern = "district")

#Collate 08-09 thru 12-13 data
all_csvs <- dir(path_to_raw, recursive=T, pattern = ".csv")
state_data <- grep("State", all_csvs, value=T)
district_data <- all_csvs[!all_csvs %in% state_data]

#Collate 13-14 and beyond data
all_xlsx <- dir(path_to_raw, recursive=T, pattern = ".xlsx")

#Process district_data: 08-09 thru 12-13
district1 <- data.frame(stringsAsFactors = F)
for (i in 1:length(district_data)) {
  current_file <- read.csv(paste0(path_to_raw, "/", district_data[i]), stringsAsFactors = F, header = F, check.names = F)
  year <- substr(district_data[i], 1, 4)
  domain <- gsub(".*KEI_\\s*|.csv.*", "", district_data[i])
  if (year == 2012) {
    current_file <- current_file[,1:5]
    names(current_file) <- c("District", "Number of Students Tested", "Level 1", "Level 2", "Level 3")
    current_file <- current_file[-c(1:2),]
    current_file$Year <- paste(year, as.numeric(substr(year, 3, 4))+1, sep="-")
    current_file$Domain <- domain
  } else {
    current_file[,1] <- NULL
    names(current_file) <- c("District", "Year", "Number of Students Tested", "Level 1", "Level 2", "Level 3")
    current_file$Domain <- domain
  }
  district1 <- rbind(district1, current_file)
}

#Remove Statewide data from 08-09 thru 11-12 (this comes from state_data)
district1 <- district1[!(district1$Year != "2012-13" & district1$District == "Statewide"),]

#Process district_data: 13-14 and beyond
file1 <- read_excel(paste0(path_to_raw, "/", all_xlsx[2]), sheet=1, skip=0)
colnames(file1) <- c("districtname", 
                     "LA_PCT1", "LA_PCT2", "LA_PCT3",
                     "LI_PCT1", "LI_PCT2", "LI_PCT3",
                     "NU_PCT1", "NU_PCT2", "NU_PCT3",
                     "PH_PCT1", "PH_PCT2", "PH_PCT3",
                     "CR_PCT1", "CR_PCT2", "CR_PCT3",
                     "PR_PCT1", "PR_PCT2", "PR_PCT3")
year <- as.numeric(gsub("[^\\d]+", "", all_xlsx[2], perl=TRUE))
file1$Year <- paste(year, as.numeric(substr(year, 3, 4))+1, sep="-")
file1 <- file1[-1,]

file2 <- read_excel(paste0(path_to_raw, "/", all_xlsx[1]), sheet=2, skip=0)
file2[,2] <- NULL

file2[] <- as.data.frame(lapply(file2, gsub, pattern='%', replacement=''))
state_file2 <- file2[file2$districtname == "State",]
state_file2 <- gather(state_file2, Variable, Value, 2:20)
state_file2 <- state_file2[state_file2$Variable != "districtname",]
state_file2$Value <- as.numeric(state_file2$Value)*100
state_file2 <- spread(state_file2, "Variable", "Value")
state_file2$districtname <- "Connecticut"

file2 <- rbind(file2, state_file2)
file2 <- file2[file2$districtname != "State",]

#Combine years
district2 <- rbind(file1, file2)

#Step 1: Wide to Long
district2 <- gather(district2, Variable, Value, 2:19)

#Assing Level and Domain
district2$Level <- NA
district2$Level[grepl("PCT1", district2$Variable)] <- "Level 1"
district2$Level[grepl("PCT2", district2$Variable)] <- "Level 2"
district2$Level[grepl("PCT3", district2$Variable)] <- "Level 3"
district2$Domain <- NA
district2$Domain[grepl("LA", district2$Variable)] <- "Language"
district2$Domain[grepl("LI", district2$Variable)] <- "Literacy"
district2$Domain[grepl("NU", district2$Variable)] <- "Numeracy"
district2$Domain[grepl("PH", district2$Variable)] <- "Physical"
district2$Domain[grepl("CR", district2$Variable)] <- "Creative"
district2$Domain[grepl("PR", district2$Variable)] <- "Personal"
district2$Variable <- NULL

#Step 2: Long to Wide
district2 <- spread(district2, Level, Value)

names(district2)[1] <- "District"
district2$`Number of Students Tested` <- NA

#Process state data (08-09 thru 12-13 only)
state <- data.frame(stringsAsFactors = F)
for (i in 1:length(state_data)) {
  current_file <- read.csv(paste0(path_to_raw, "/", state_data[i]), stringsAsFactors = F, header = T, check.names = F)
  domain <- gsub(".*-_\\s*|_Skills.*", "", state_data[i])
  names(current_file) <- c("District", "Year", "Number of Students Tested", "Level 1", "Level 2", "Level 3")
  current_file$Domain <- domain
  state <- rbind(state, current_file)
}
  
#Combine all district and state data
KEI <- rbind(district1, district2, state)

#Merge in districts
districts <- read.csv(paste0(path_to_raw, "/", xwalk), stringsAsFactors = F, header = T, check.names = F)
KEI_xwalk <- merge(KEI, districts, by = "District", all.y=T)

KEI_xwalk$District <- NULL

KEI_xwalk<-KEI_xwalk[!duplicated(KEI_xwalk), ]

KEI_xwalk$Domain[KEI_xwalk$Domain == "Creative-Aesthetic"] <- "Creative"
KEI_xwalk$Domain[KEI_xwalk$Domain == "Personal-Social"] <- "Personal"
KEI_xwalk$Domain[KEI_xwalk$Domain == "Physical-Motor"] <- "Physical"

#backfill year
years <- c("2008-09",
           "2009-10",
           "2010-11",
           "2011-12",
           "2012-13",
           "2013-14",
           "2014-15", 
           "2015-16",
           "2016-17")

backfill_years <- expand.grid(
  `FixedDistrict` = unique(districts$`FixedDistrict`),
  `Domain` = c("Creative", "Language", "Literacy", "Numeracy", "Personal", "Physical"),
  `Year` = years
)

backfill_years$FixedDistrict <- as.character(backfill_years$FixedDistrict)
backfill_years$Year <- as.character(backfill_years$Year)

backfill_years <- arrange(backfill_years, FixedDistrict)

KEI_backfill <- merge(KEI_xwalk, backfill_years, all.y=T)

KEI_backfill <- unique(KEI_backfill)

#Calculate Numbers
KEI_backfill <- KEI_backfill %>% 
  mutate(`Level 1 Num` = round(as.numeric(`Number of Students Tested`)*(as.numeric(`Level 1`)/100), 0),
         `Level 2 Num` = round(as.numeric(`Number of Students Tested`)*(as.numeric(`Level 2`)/100), 0),
         `Level 3 Num` = round(as.numeric(`Number of Students Tested`)*(as.numeric(`Level 3`)/100), 0))

#Convert wide to long
KEI_backfill_long <- gather(KEI_backfill, "Variable", "Value", c(4:7, 9:11))

#Configure Level and MT columns based on Variable column
KEI_backfill_long$Level <- "Total Tested"
KEI_backfill_long$Level[grepl("Level 1", KEI_backfill_long$Variable)] <- "Level 1"
KEI_backfill_long$Level[grepl("Level 2", KEI_backfill_long$Variable)] <- "Level 2"
KEI_backfill_long$Level[grepl("Level 3", KEI_backfill_long$Variable)] <- "Level 3"
KEI_backfill_long$`Measure Type` <- "Percent"
KEI_backfill_long$`Measure Type`[grepl("Num", KEI_backfill_long$Variable)] <- "Number"

#Set variable
KEI_backfill_long$Variable <- "Kindergarten Entrance Inventory Results"

#Recode Domain
KEI_backfill_long$Domain[KEI_backfill_long$Domain == "Creative"] <- "Creative/Aesthetic"
KEI_backfill_long$Domain[KEI_backfill_long$Domain == "Personal"] <- "Personal/Social"   
KEI_backfill_long$Domain[KEI_backfill_long$Domain == "Physical"] <- "Physical/Motor"    

#Recode years 2008-09 >> 2008-2009
KEI_backfill_long$Year <- sub('(?<=.{5})', '20', KEI_backfill_long$Year, perl=TRUE)

#Select and sort columns
KEI_final <- KEI_backfill_long %>% 
  rename(District = FixedDistrict, `Skill Domain` = Domain, `Skill Level` = Level) %>% 
  select(District, FIPS, Year, `Skill Domain`, `Skill Level`, `Measure Type`, Variable, Value) %>% 
  arrange(District, Year, `Skill Domain`, `Skill Level`, `Measure Type`)

#Set '*' to -9999 so we can round the Value column
KEI_final$Value[KEI_final$Value == "*"] <- -9999

#Set blank FIPS to ""
KEI_final$FIPS[is.na(KEI_final$FIPS)] <- ""

#Now round Value column
KEI_final$Value <- round(as.numeric(KEI_final$Value), 2)

#Write CSV
write.table(
  KEI_final,
  file.path(getwd(), "data", "kindergarten-entrance-inventory-2017.csv"),
  sep = ",",
  row.names = F, 
  na = "-6666"
)









