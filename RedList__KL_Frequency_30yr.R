##################### Required ##########################
library (dplyr)  # For Pipes %>%
library (lubridate) # For Date Function
library (tidyr) # For pivot_wider Function
library (plyr) # For join Function
library(conflicted)
conflict_prefer("arrange", "dplyr")
conflict_prefer("summarize", "dplyr")
conflict_prefer("mutate", "dplyr")
#############################################################
# Clear the Console and the Environment
rm(list=ls()) 

setwd("D:/ArunV/R Programming Ebird")

CurYear <- 2022
CurMonth <- 4
unzip <- 0

# file name for ebd file
ebdfile <- paste0("ebd_IN-KL_rel",month.abb[CurMonth],"-",CurYear)

# List the interested columns

preimp <- c(
  "TAXONOMIC.ORDER","CATEGORY","SCIENTIFIC.NAME","COMMON.NAME","OBSERVATION.COUNT","STATE","APPROVED",
  "OBSERVATION.DATE","GROUP.IDENTIFIER","SAMPLING.EVENT.IDENTIFIER"
)

#Incase the unzip is not done, uncomment this line
# if (unzip)
# {
#   unzip(paste(dir, ebdfile,'.zip',sep=''))
# }

# Read the header plus first row
nms <- read.delim( 
  paste0 (ebdfile,".txt"),
  nrows = 1, 
  sep = '\t', 
  header = T, 
  quote = "", 
  stringsAsFactors = F, 
  na.strings = c ("", " ",NA)
)

nms <- names(nms)
nms [!(nms %in% preimp)] <- "NULL"
nms [nms %in% preimp] <- NA

ebd <- read.delim(paste0(ebdfile,".txt"),
                  colClasses = nms,
                  #                  nrows = 100000, # For testing, this is useful
                  sep = '\t', 
                  header = T, 
                  quote = "", 
                  stringsAsFactors = F, 
                  na.strings = c ("", " ",NA))

save(ebd, file = "KL_RedListData_30YR.RData")

# load("KL_RedListData_30YR.RData")

# Downsize the DataSet to Kerala. Not required if you download KL data
# ebd <- ebd %>% 
#             filter (STATE.CODE == "IN-KL")

# Remove unapproved species (typically Exotics)
ebd <- ebd %>%
  filter (APPROVED == 1)

# Remove STATE and APPROVED column and downsize data
ebd <- ebd %>% 
  select (
    CATEGORY, TAXONOMIC.ORDER, SCIENTIFIC.NAME,COMMON.NAME,OBSERVATION.COUNT,OBSERVATION.DATE,
    GROUP.IDENTIFIER,SAMPLING.EVENT.IDENTIFIER)

ebd = ebd %>%
  mutate (
    OBSERVATION.DATE = as.Date(OBSERVATION.DATE),
    YEAR  = year(OBSERVATION.DATE)
  )

# Remove OBSERVATION.DATE column and downsize data
ebd <- ebd %>% 
  select (
    CATEGORY, TAXONOMIC.ORDER, SCIENTIFIC.NAME,COMMON.NAME,OBSERVATION.COUNT,YEAR,
    GROUP.IDENTIFIER,SAMPLING.EVENT.IDENTIFIER)

ebd <- ebd %>%
  filter (YEAR >= CurYear-30 & YEAR < CurYear)

# Add GROUP.ID for finding unique lists to help remove duplicate checklist
ebd <- ebd %>% 
  mutate (
    GROUP.ID = ifelse (
      is.na(GROUP.IDENTIFIER), 
      SAMPLING.EVENT.IDENTIFIER,
      GROUP.IDENTIFIER
    )
  )

# Remove SAMPLING.EVENT.IDENTIFIER & GROUP.IDENTIFIER as its not needed anymore          
ebd <- ebd %>% 
  distinct (CATEGORY,TAXONOMIC.ORDER,SCIENTIFIC.NAME,COMMON.NAME,OBSERVATION.COUNT,YEAR,GROUP.ID)

ebd <- ebd %>%
  filter (CATEGORY %in% c('species','issf','domestic'))

# Remove GROUP.ID as its not needed anymore          
ebd <- ebd %>% 
  distinct (CATEGORY,TAXONOMIC.ORDER,SCIENTIFIC.NAME,COMMON.NAME,OBSERVATION.COUNT,YEAR)

# To be used later
ebd_d <- ebd

# Remove CATEGORY, TAXONOMIC.ORDER, as its not needed anymore          
ebd <- ebd %>% 
  distinct (SCIENTIFIC.NAME,COMMON.NAME,OBSERVATION.COUNT,YEAR)

ebd_f <- ebd
ebd_f$OBSERVATION.COUNT <- 1

data_1 <- ebd_f %>%
                group_by(SCIENTIFIC.NAME,COMMON.NAME,YEAR) %>%
                summarize(count = sum(OBSERVATION.COUNT)) %>%
                arrange(YEAR, SCIENTIFIC.NAME,COMMON.NAME,desc(count))

data_2 <- pivot_wider(data_1, names_from = YEAR, values_from = count )
data_2[is.na(data_2)] <- 0

data_3 <- data_2 %>%
  group_by(SCIENTIFIC.NAME,COMMON.NAME) %>%
  summarise_all(sum) %>%
  mutate(COUNT = NA)

data_3$COUNT <- rowSums((data_3[3:32])>0)

# Generate Data Set for Scientific Names and Taxonomic Order
ebd_d1 <- ebd_d %>% 
                distinct (TAXONOMIC.ORDER, SCIENTIFIC.NAME,CATEGORY)
ebd_d1 <- ebd_d1[order(ebd_d1$TAXONOMIC.ORDER),]
ebd_d1 <- ebd_d1 %>%
                group_by(SCIENTIFIC.NAME) %>%
                mutate(rank = rank(SCIENTIFIC.NAME, ties.method = "first"))
ebd_d1 <- ebd_d1[!(ebd_d1$rank==2 | ebd_d1$rank==3),]

# Final Data with Taxonomic Order
data_4 <- join(data_3, ebd_d1, by="SCIENTIFIC.NAME")
data_4 = subset(data_4, select = -c(CATEGORY,rank) )
data_4 <- data_4 %>%
                select(TAXONOMIC.ORDER, everything())
data_4 <- data_4[order(data_4$TAXONOMIC.ORDER),]

# Save the dataframe in RDS file
saveRDS(data_4,"KL_Count_YearWise.RDS")

# Write to csv file
write.csv(data_4,"D:/ArunV/R Programming Ebird/KL_Count_YearWise.csv", row.names = FALSE)
