data  = read.csv("NY property data.csv")
library(dplyr)
library(prettyR)
library(ggplot2)
library(zoo)

## 1.ZIP
ZIP = data[,c("B", "BLOCK", "TAXCLASS", "ZIP")]
ZIP$ZIP[ZIP$ZIP == 0] = NA
##Create three dataframe. group ZIP by B, (B, BLOCK), (B, BLOCK, TAXCLASS) respectively, and get mode。
MODE_B = ZIP %>%
  group_by(B) %>%
  summarise(ZIP = Mode(ZIP))
MODE_B = as.data.frame(MODE_B)

MODE_B_BLOCK = ZIP %>%
  group_by(B, BLOCK) %>%
  summarise(ZIP = Mode(ZIP))
MODE_B_BLOCK = as.data.frame(MODE_B_BLOCK)

MODE_B_BLOCK_TAX = ZIP %>%
  group_by(B, BLOCK, TAXCLASS) %>%
  summarise(ZIP = Mode(ZIP))
MODE_B_BLOCK_TAX = as.data.frame(MODE_B_BLOCK_TAX)

## There are some groups with more than 1 mode.
##relpace all ">1 mode" with NA
MODE_B_BLOCK_TAX$ZIP[MODE_B_BLOCK_TAX$ZIP==">1 mode"] = NA

##filter all the NA and fill them with the former group. 
## filter all the NA
NA_1 = MODE_B_BLOCK_TAX[is.na(MODE_B_BLOCK_TAX$ZIP),]

## For NA, group by (B, BLOCK) to look for mode ZIP
FILL1 = merge(x = NA_1[,c("B","BLOCK", "TAXCLASS")], y = MODE_B_BLOCK, by = c("B","BLOCK"), all.x = TRUE)
## In FILL1 there are still some NA and ">1 mode", again we fill them with the former group.
##Again, relpace all ">1 mode" with NA
FILL1$ZIP[FILL1$ZIP ==">1 mode"] = NA
##  For NA,  group by B
NA_2 = FILL1[is.na(FILL1$ZIP),]
FILL2 = merge(x = NA_2[,c("B","BLOCK", "TAXCLASS")], y = MODE_B, by = "B", all.x = TRUE)
FILL3 = rbind(FILL2, FILL1[!is.na(FILL1$ZIP),])

##combining FILL1 with all non-NA in MODE_B_BLOCK_TAX to creat a complete 'dictionary', where every B_BLOCK_TAX group has a unique mode ZIP
COMPLETE_DICT = rbind(FILL3, MODE_B_BLOCK_TAX[!is.na(MODE_B_BLOCK_TAX$ZIP),])

##fill missing value in ZIP$ZIP with the same logic
NA_ALL = ZIP[is.na(ZIP$ZIP),]

FILL_ALL = merge(x = NA_ALL[,c("B","BLOCK", "TAXCLASS")], y = COMPLETE_DICT, by = c('B','BLOCK','TAXCLASS'), all.x = TRUE)

COMPLETE_ZIP = rbind(FILL_ALL, ZIP[!is.na(ZIP$ZIP),])

## update original data with new ZIP column
data$ZIP = COMPLETE_ZIP$ZIP

## 2.STORIES
STORIES = data[,c("B", "BLOCK", "TAXCLASS", "STORIES")]
STORIES$STORIES[STORIES$STORIES == 0] = NA
##Create three dataframe. group STORIES by B, (B, BLOCK), (B, BLOCK, TAXCLASS) respectively, and get mode。
MODE_B = STORIES %>%
  group_by(B) %>%
  summarise(STORIES = Mode(STORIES))
MODE_B = as.data.frame(MODE_B)

MODE_B_BLOCK = STORIES %>%
  group_by(B, BLOCK) %>%
  summarise(STORIES = Mode(STORIES))
MODE_B_BLOCK = as.data.frame(MODE_B_BLOCK)

MODE_B_BLOCK_TAX = STORIES %>%
  group_by(B, BLOCK, TAXCLASS) %>%
  summarise(STORIES = Mode(STORIES))
MODE_B_BLOCK_TAX = as.data.frame(MODE_B_BLOCK_TAX)

## There are some groups with more than 1 mode.
##relpace all ">1 mode" with NA
MODE_B_BLOCK_TAX$STORIES[MODE_B_BLOCK_TAX$STORIES==">1 mode"] = NA

##filter all the NA and fill them with the former group. 
## filter all the NA
NA_1 = MODE_B_BLOCK_TAX[is.na(MODE_B_BLOCK_TAX$STORIES),]

## For NA, group by (B, BLOCK) to look for mode STORIES
FILL1 = merge(x = NA_1[,c("B","BLOCK", "TAXCLASS")], y = MODE_B_BLOCK, by = c("B","BLOCK"), all.x = TRUE)
## In FILL1 there are still some NA and ">1 mode", again we fill them with the former group.
##Again, relpace all ">1 mode" with NA
FILL1$STORIES[FILL1$STORIES ==">1 mode"] = NA
##  For NA,  group by B
NA_2 = FILL1[is.na(FILL1$STORIES),]
FILL2 = merge(x = NA_2[,c("B","BLOCK", "TAXCLASS")], y = MODE_B, by = "B", all.x = TRUE)
FILL3 = rbind(FILL2, FILL1[!is.na(FILL1$STORIES),])

##combining FILL1 with all non-NA in MODE_B_BLOCK_TAX to creat a complete 'dictionary', where every B_BLOCK_TAX group has a unique mode STORIES
COMPLETE_DICT = rbind(FILL3, MODE_B_BLOCK_TAX[!is.na(MODE_B_BLOCK_TAX$STORIES),])

##fill missing value in STORIES$STORIES with the same logic
NA_ALL = STORIES[is.na(STORIES$STORIES),]

FILL_ALL = merge(x = NA_ALL[,c("B","BLOCK", "TAXCLASS")], y = COMPLETE_DICT, by = c('B','BLOCK','TAXCLASS'), all.x = TRUE)

COMPLETE_STORIES = rbind(FILL_ALL, STORIES[!is.na(STORIES$STORIES),])

## update original data with new STORIES column
data$STORIES = COMPLETE_STORIES$STORIES


## 3.LTFRONT
LTFRONT = data[,c("B", "ZIP", "STORIES", "LTFRONT")]
LTFRONT$LTFRONT[LTFRONT$LTFRONT == 0] = NA
##Create three dataframe. group LTFRONT by B, (B, ZIP), (B, ZIP, STORIES) respectively, and get MEDIAN。
MEDIAN_B = LTFRONT %>%
  group_by(B) %>%
  summarise(LTFRONT = median(LTFRONT, na.rm = TRUE))
MEDIAN_B = as.data.frame(MEDIAN_B)

MEDIAN_B_ZIP = LTFRONT %>%
  group_by(B, ZIP) %>%
  summarise(LTFRONT = median(LTFRONT, na.rm = TRUE))
MEDIAN_B_ZIP = as.data.frame(MEDIAN_B_ZIP)

MEDIAN_B_ZIP_STORIES = LTFRONT %>%
  group_by(B, ZIP, STORIES) %>%
  summarise(LTFRONT = median(LTFRONT, na.rm = TRUE))
MEDIAN_B_ZIP_STORIES = as.data.frame(MEDIAN_B_ZIP_STORIES)

##filter all the NA and fill them with the former group. 
## filter all the NA
NA_1 = MEDIAN_B_ZIP_STORIES[is.na(MEDIAN_B_ZIP_STORIES$LTFRONT),]

## For NA, group by (B, ZIP) to look for MEDIAN LTFRONT
FILL1 = merge(x = NA_1[,c("B","ZIP", "STORIES")], y = MEDIAN_B_ZIP, by = c("B","ZIP"), all.x = TRUE)
##  For NA,  group by B
NA_2 = FILL1[is.na(FILL1$LTFRONT),]
FILL2 = merge(x = NA_2[,c("B","ZIP", "STORIES")], y = MEDIAN_B, by = "B", all.x = TRUE)
FILL3 = rbind(FILL2, FILL1[!is.na(FILL1$LTFRONT),])

##combining FILL1 with all non-NA in MEDIAN_B_ZIP_STORIES to creat a complete 'dictionary', where every B_ZIP_STORIES group has a unique MEDIAN LTFRONT
COMPLETE_DICT = rbind(FILL3, MEDIAN_B_ZIP_STORIES[!is.na(MEDIAN_B_ZIP_STORIES$LTFRONT),])

##fill missing value in LTFRONT$LTFRONT with the same logic
NA_ALL = LTFRONT[is.na(LTFRONT$LTFRONT),]

FILL_ALL = merge(x = NA_ALL[,c("B","ZIP", "STORIES")], y = COMPLETE_DICT, by = c('B','ZIP','STORIES'), all.x = TRUE)

COMPLETE_LTFRONT = rbind(FILL_ALL, LTFRONT[!is.na(LTFRONT$LTFRONT),])

## update original data with new LTFRONT column
data$LTFRONT = COMPLETE_LTFRONT$LTFRONT

## 4.LTDEPTH
LTDEPTH = data[,c("B", "ZIP", "STORIES", "LTDEPTH")]
LTDEPTH$LTDEPTH[LTDEPTH$LTDEPTH == 0] = NA
##Create three dataframe. group LTDEPTH by B, (B, ZIP), (B, ZIP, STORIES) respectively, and get MEDIAN。
MEDIAN_B = LTDEPTH %>%
  group_by(B) %>%
  summarise(LTDEPTH = median(LTDEPTH, na.rm = TRUE))
MEDIAN_B = as.data.frame(MEDIAN_B)

MEDIAN_B_ZIP = LTDEPTH %>%
  group_by(B, ZIP) %>%
  summarise(LTDEPTH = median(LTDEPTH, na.rm = TRUE))
MEDIAN_B_ZIP = as.data.frame(MEDIAN_B_ZIP)

MEDIAN_B_ZIP_STORIES = LTDEPTH %>%
  group_by(B, ZIP, STORIES) %>%
  summarise(LTDEPTH = median(LTDEPTH, na.rm = TRUE))
MEDIAN_B_ZIP_STORIES = as.data.frame(MEDIAN_B_ZIP_STORIES)

##filter all the NA and fill them with the former group. 
## filter all the NA
NA_1 = MEDIAN_B_ZIP_STORIES[is.na(MEDIAN_B_ZIP_STORIES$LTDEPTH),]

## For NA, group by (B, ZIP) to look for MEDIAN LTDEPTH
FILL1 = merge(x = NA_1[,c("B","ZIP", "STORIES")], y = MEDIAN_B_ZIP, by = c("B","ZIP"), all.x = TRUE)
##  For NA,  group by B
NA_2 = FILL1[is.na(FILL1$LTDEPTH),]
FILL2 = merge(x = NA_2[,c("B","ZIP", "STORIES")], y = MEDIAN_B, by = "B", all.x = TRUE)
FILL3 = rbind(FILL2, FILL1[!is.na(FILL1$LTDEPTH),])

##combining FILL1 with all non-NA in MEDIAN_B_ZIP_STORIES to creat a complete 'dictionary', where every B_ZIP_STORIES group has a unique MEDIAN LTDEPTH
COMPLETE_DICT = rbind(FILL3, MEDIAN_B_ZIP_STORIES[!is.na(MEDIAN_B_ZIP_STORIES$LTDEPTH),])

##fill missing value in LTDEPTH$LTDEPTH with the same logic
NA_ALL = LTDEPTH[is.na(LTDEPTH$LTDEPTH),]

FILL_ALL = merge(x = NA_ALL[,c("B","ZIP", "STORIES")], y = COMPLETE_DICT, by = c('B','ZIP','STORIES'), all.x = TRUE)

COMPLETE_LTDEPTH = rbind(FILL_ALL, LTDEPTH[!is.na(LTDEPTH$LTDEPTH),])

## update original data with new LTDEPTH column
data$LTDEPTH = COMPLETE_LTDEPTH$LTDEPTH

## 5.BLDFRONT
BLDFRONT = data[,c("B", "ZIP", "STORIES", "BLDFRONT")]
BLDFRONT$BLDFRONT[BLDFRONT$BLDFRONT == 0] = NA
##Create three dataframe. group BLDFRONT by B, (B, ZIP), (B, ZIP, STORIES) respectively, and get MEDIAN。
MEDIAN_B = BLDFRONT %>%
  group_by(B) %>%
  summarise(BLDFRONT = median(BLDFRONT, na.rm = TRUE))
MEDIAN_B = as.data.frame(MEDIAN_B)

MEDIAN_B_ZIP = BLDFRONT %>%
  group_by(B, ZIP) %>%
  summarise(BLDFRONT = median(BLDFRONT, na.rm = TRUE))
MEDIAN_B_ZIP = as.data.frame(MEDIAN_B_ZIP)

MEDIAN_B_ZIP_STORIES = BLDFRONT %>%
  group_by(B, ZIP, STORIES) %>%
  summarise(BLDFRONT = median(BLDFRONT, na.rm = TRUE))
MEDIAN_B_ZIP_STORIES = as.data.frame(MEDIAN_B_ZIP_STORIES)

##filter all the NA and fill them with the former group. 
## filter all the NA
NA_1 = MEDIAN_B_ZIP_STORIES[is.na(MEDIAN_B_ZIP_STORIES$BLDFRONT),]

## For NA, group by (B, ZIP) to look for MEDIAN BLDFRONT
FILL1 = merge(x = NA_1[,c("B","ZIP", "STORIES")], y = MEDIAN_B_ZIP, by = c("B","ZIP"), all.x = TRUE)
##  For NA,  group by B
NA_2 = FILL1[is.na(FILL1$BLDFRONT),]
FILL2 = merge(x = NA_2[,c("B","ZIP", "STORIES")], y = MEDIAN_B, by = "B", all.x = TRUE)
FILL3 = rbind(FILL2, FILL1[!is.na(FILL1$BLDFRONT),])

##combining FILL1 with all non-NA in MEDIAN_B_ZIP_STORIES to creat a complete 'dictionary', where every B_ZIP_STORIES group has a unique MEDIAN BLDFRONT
COMPLETE_DICT = rbind(FILL3, MEDIAN_B_ZIP_STORIES[!is.na(MEDIAN_B_ZIP_STORIES$BLDFRONT),])

##fill missing value in BLDFRONT$BLDFRONT with the same logic
NA_ALL = BLDFRONT[is.na(BLDFRONT$BLDFRONT),]

FILL_ALL = merge(x = NA_ALL[,c("B","ZIP", "STORIES")], y = COMPLETE_DICT, by = c('B','ZIP','STORIES'), all.x = TRUE)

COMPLETE_BLDFRONT = rbind(FILL_ALL, BLDFRONT[!is.na(BLDFRONT$BLDFRONT),])

## update original data with new BLDFRONT column
data$BLDFRONT = COMPLETE_BLDFRONT$BLDFRONT

## 6.BLDDEPTH
BLDDEPTH = data[,c("B", "ZIP", "STORIES", "BLDDEPTH")]
BLDDEPTH$BLDDEPTH[BLDDEPTH$BLDDEPTH == 0] = NA
##Create three dataframe. group BLDDEPTH by B, (B, ZIP), (B, ZIP, STORIES) respectively, and get MEDIAN。
MEDIAN_B = BLDDEPTH %>%
  group_by(B) %>%
  summarise(BLDDEPTH = median(BLDDEPTH, na.rm = TRUE))
MEDIAN_B = as.data.frame(MEDIAN_B)

MEDIAN_B_ZIP = BLDDEPTH %>%
  group_by(B, ZIP) %>%
  summarise(BLDDEPTH = median(BLDDEPTH, na.rm = TRUE))
MEDIAN_B_ZIP = as.data.frame(MEDIAN_B_ZIP)

MEDIAN_B_ZIP_STORIES = BLDDEPTH %>%
  group_by(B, ZIP, STORIES) %>%
  summarise(BLDDEPTH = median(BLDDEPTH, na.rm = TRUE))
MEDIAN_B_ZIP_STORIES = as.data.frame(MEDIAN_B_ZIP_STORIES)

##filter all the NA and fill them with the former group. 
## filter all the NA
NA_1 = MEDIAN_B_ZIP_STORIES[is.na(MEDIAN_B_ZIP_STORIES$BLDDEPTH),]

## For NA, group by (B, ZIP) to look for MEDIAN BLDDEPTH
FILL1 = merge(x = NA_1[,c("B","ZIP", "STORIES")], y = MEDIAN_B_ZIP, by = c("B","ZIP"), all.x = TRUE)
##  For NA,  group by B
NA_2 = FILL1[is.na(FILL1$BLDDEPTH),]
FILL2 = merge(x = NA_2[,c("B","ZIP", "STORIES")], y = MEDIAN_B, by = "B", all.x = TRUE)
FILL3 = rbind(FILL2, FILL1[!is.na(FILL1$BLDDEPTH),])

##combining FILL1 with all non-NA in MEDIAN_B_ZIP_STORIES to creat a complete 'dictionary', where every B_ZIP_STORIES group has a unique MEDIAN BLDDEPTH
COMPLETE_DICT = rbind(FILL3, MEDIAN_B_ZIP_STORIES[!is.na(MEDIAN_B_ZIP_STORIES$BLDDEPTH),])

##fill missing value in BLDDEPTH$BLDDEPTH with the same logic
NA_ALL = BLDDEPTH[is.na(BLDDEPTH$BLDDEPTH),]

FILL_ALL = merge(x = NA_ALL[,c("B","ZIP", "STORIES")], y = COMPLETE_DICT, by = c('B','ZIP','STORIES'), all.x = TRUE)

COMPLETE_BLDDEPTH = rbind(FILL_ALL, BLDDEPTH[!is.na(BLDDEPTH$BLDDEPTH),])

## update original data with new BLDDEPTH column
data$BLDDEPTH = COMPLETE_BLDDEPTH$BLDDEPTH

## 7.FULLVAL
FULLVAL = data[,c("B", "ZIP", "STORIES", "TAXCLASS", "FULLVAL")]
FULLVAL$FULLVAL[FULLVAL$FULLVAL == 0] = NA
##Create three dataframe. group FULLVAL by B, (B, ZIP), (B, ZIP, STORIES) respectively, and get MEDIAN。
MEDIAN_B = FULLVAL %>%
  group_by(B) %>%
  summarise(FULLVAL = median(FULLVAL, na.rm = TRUE))
MEDIAN_B = as.data.frame(MEDIAN_B)

MEDIAN_B_ZIP = FULLVAL %>%
  group_by(B, ZIP) %>%
  summarise(FULLVAL = median(FULLVAL, na.rm = TRUE))
MEDIAN_B_ZIP = as.data.frame(MEDIAN_B_ZIP)

MEDIAN_B_ZIP_STORIES = FULLVAL %>%
  group_by(B, ZIP, STORIES) %>%
  summarise(FULLVAL = median(FULLVAL, na.rm = TRUE))
MEDIAN_B_ZIP_STORIES = as.data.frame(MEDIAN_B_ZIP_STORIES)

MEDIAN_B_ZIP_STORIES_TAXCLASS = FULLVAL %>%
  group_by(B, ZIP, STORIES, TAXCLASS) %>%
  summarise(FULLVAL = median(FULLVAL, na.rm = TRUE))
MEDIAN_B_ZIP_STORIES_TAXCLASS = as.data.frame(MEDIAN_B_ZIP_STORIES_TAXCLASS)

##filter all the NA and fill them with the former group. 
## filter all the NA
NA_1 = MEDIAN_B_ZIP_STORIES_TAXCLASS[is.na(MEDIAN_B_ZIP_STORIES_TAXCLASS$FULLVAL),]

## For NA, group by (B, ZIP, STORIES) to look for MEDIAN FULLVAL
FILL1 = merge(x = NA_1[,c("B","ZIP", "STORIES", "TAXCLASS")], y = MEDIAN_B_ZIP_STORIES, by = c("B","ZIP", "STORIES"), all.x = TRUE)
##  For NA,  group by B
NA_2 = FILL1[is.na(FILL1$FULLVAL),]
FILL2 = merge(x = NA_2[,c("B","ZIP", "STORIES", "TAXCLASS")], y = MEDIAN_B_ZIP, by = c("B", "ZIP"), all.x = TRUE)
FILL3 = rbind(FILL2, FILL1[!is.na(FILL1$FULLVAL),])

NA_3 = FILL3[is.na(FILL3$FULLVAL),]
FILL4 = merge(x = NA_3[,c("B","ZIP", "STORIES", "TAXCLASS")], y = MEDIAN_B, by = "B", all.x = TRUE)
FILL5 = rbind(FILL4, FILL3[!is.na(FILL3$FULLVAL),])
##combining FILL1 with all non-NA in MEDIAN_B_ZIP_STORIES to creat a complete 'dictionary', where every B_ZIP_STORIES group has a unique MEDIAN FULLVAL
COMPLETE_DICT = rbind(FILL5, MEDIAN_B_ZIP_STORIES_TAXCLASS[!is.na(MEDIAN_B_ZIP_STORIES_TAXCLASS$FULLVAL),])

##fill missing value in FULLVAL$FULLVAL with the same logic
NA_ALL = FULLVAL[is.na(FULLVAL$FULLVAL),]

FILL_ALL = merge(x = NA_ALL[,c("B","ZIP", "STORIES", "TAXCLASS")], y = COMPLETE_DICT, by = c('B','ZIP','STORIES','TAXCLASS'), all.x = TRUE)

COMPLETE_FULLVAL = rbind(FILL_ALL, FULLVAL[!is.na(FULLVAL$FULLVAL),])

## update original data with new FULLVAL column
data$FULLVAL = COMPLETE_FULLVAL$FULLVAL

## 8.AVLAND
AVLAND = data[,c("B", "ZIP", "STORIES", "TAXCLASS", "AVLAND")]
AVLAND$AVLAND[AVLAND$AVLAND == 0] = NA
##Create three dataframe. group AVLAND by B, (B, ZIP), (B, ZIP, STORIES) respectively, and get MEDIAN。
MEDIAN_B = AVLAND %>%
  group_by(B) %>%
  summarise(AVLAND = median(AVLAND, na.rm = TRUE))
MEDIAN_B = as.data.frame(MEDIAN_B)

MEDIAN_B_ZIP = AVLAND %>%
  group_by(B, ZIP) %>%
  summarise(AVLAND = median(AVLAND, na.rm = TRUE))
MEDIAN_B_ZIP = as.data.frame(MEDIAN_B_ZIP)

MEDIAN_B_ZIP_STORIES = AVLAND %>%
  group_by(B, ZIP, STORIES) %>%
  summarise(AVLAND = median(AVLAND, na.rm = TRUE))
MEDIAN_B_ZIP_STORIES = as.data.frame(MEDIAN_B_ZIP_STORIES)

MEDIAN_B_ZIP_STORIES_TAXCLASS = AVLAND %>%
  group_by(B, ZIP, STORIES, TAXCLASS) %>%
  summarise(AVLAND = median(AVLAND, na.rm = TRUE))
MEDIAN_B_ZIP_STORIES_TAXCLASS = as.data.frame(MEDIAN_B_ZIP_STORIES_TAXCLASS)

##filter all the NA and fill them with the former group. 
## filter all the NA
NA_1 = MEDIAN_B_ZIP_STORIES_TAXCLASS[is.na(MEDIAN_B_ZIP_STORIES_TAXCLASS$AVLAND),]

## For NA, group by (B, ZIP, STORIES) to look for MEDIAN AVLAND
FILL1 = merge(x = NA_1[,c("B","ZIP", "STORIES", "TAXCLASS")], y = MEDIAN_B_ZIP_STORIES, by = c("B","ZIP", "STORIES"), all.x = TRUE)
##  For NA,  group by B
NA_2 = FILL1[is.na(FILL1$AVLAND),]
FILL2 = merge(x = NA_2[,c("B","ZIP", "STORIES", "TAXCLASS")], y = MEDIAN_B_ZIP, by = c("B", "ZIP"), all.x = TRUE)
FILL3 = rbind(FILL2, FILL1[!is.na(FILL1$AVLAND),])

NA_3 = FILL3[is.na(FILL3$AVLAND),]
FILL4 = merge(x = NA_3[,c("B","ZIP", "STORIES", "TAXCLASS")], y = MEDIAN_B, by = "B", all.x = TRUE)
FILL5 = rbind(FILL4, FILL3[!is.na(FILL3$AVLAND),])
##combining FILL1 with all non-NA in MEDIAN_B_ZIP_STORIES to creat a complete 'dictionary', where every B_ZIP_STORIES group has a unique MEDIAN AVLAND
COMPLETE_DICT = rbind(FILL5, MEDIAN_B_ZIP_STORIES_TAXCLASS[!is.na(MEDIAN_B_ZIP_STORIES_TAXCLASS$AVLAND),])

##fill missing value in AVLAND$AVLAND with the same logic
NA_ALL = AVLAND[is.na(AVLAND$AVLAND),]

FILL_ALL = merge(x = NA_ALL[,c("B","ZIP", "STORIES", "TAXCLASS")], y = COMPLETE_DICT, by = c('B','ZIP','STORIES','TAXCLASS'), all.x = TRUE)

COMPLETE_AVLAND = rbind(FILL_ALL, AVLAND[!is.na(AVLAND$AVLAND),])

## update original data with new AVLAND column
data$AVLAND = COMPLETE_AVLAND$AVLAND

write.csv(data, 'data_cleaned.csv')
