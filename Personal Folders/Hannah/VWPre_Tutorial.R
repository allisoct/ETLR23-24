###Tutorial VWPre for data preprocessing###

#check character
#RIGHT_IA_LABEL: 0-., 1-tar, 2-dis1, 3-dis2, 4-dis3
#LEFT_IA_LABEL: 0-. (outside)

library(VWPre)

setwd("C:/Users/hgaud/OneDrive/Studium/ADVANCED3_LanguageAndLinguistics")
data <- read.delim("practice_data.txt")
head(data)
colnames(data)

#checks data columns (specify subject and item), conversion to correct form if necessary
#TODO: item column correct?
dat0 <- prep_data(data, Subject="RECORDING_SESSION_LABEL", Item="IP_INDEX")
head(dat0)

#relabel anything outside IAs as 0/outside
#TODO: Does manual recoding and NoIA=5 instead of 4 make sense?
dat1a <- recode_ia(data=dat0, IDs=c("0"="0", "1"="1", "2"="2", "3"="3", "4"="4"), Labels=c(.="Outside", tar="Target", dis1="Distractor1", dis2="Distractor2", dis3="Distractor3"))
dat1 <- relabel_na(dat1a, NoIA=5)
head(dat1)
check_ia(dat1)

#create time column, adjust if necessary
#TODO: check
check_msg_time(dat1, Msg = "target_word_onset")
dat2 <- create_time_series(dat1, Adjust=100)
check_msg_time(dat2, Msg = "target_word_onset") 
dat2a <- create_time_series
check_time_series(dat2)

#select columns of right eye only
dat3 <- select_recorded_eye(data=dat2, Recording="R", WhenLandR="Right")

#get proportional data for time bins
#TODO: NoIA=4 or rather 5 as used before?
check_samplingrate(dat3)
ds_options(SamplingRate=1000)
dat4 <- bin_prop(dat3, NoIA=4, BinSize=20, SamplingRate=1000) #NoIA=4/5?
check_samplingrate(dat4)
check_samples_per_bin(dat4)

#transform data into elogits/empirical log odds ratios 
dat5 <- transform_to_elogit(dat4, NoIA=4, ObsPerBin=20)

#create final data set
#TODO: correct column names for "arrange"?
dat6 <- rename_columns(dat5, Labels=c(IA1="Target", IA2="Distractor1", IA3="Distractor2", IA4="Distractor3"))
final_data <- dat6 %>% 
  arrange(., Subject, TRIAL_INDEX, Time)
head(final_data)
colnames(final_data)
str(final_data)
summary(final_data)