###Tutorial VWPre for data preprocessing###

library(VWPre)
library(ggplot2)


setwd("C:/Users/hgaud/OneDrive/Studium/ADVANCED3_LanguageAndLinguistics")
data <- read.delim("practice_data.txt", na.strings=c(".", "NA"))
head(data)
colnames(data)

#checks data columns (specify subject and item), conversion to correct form if necessary
dat0 <- prep_data(data, Subject="RECORDING_SESSION_LABEL", Item="IP_INDEX")
head(dat0)

#relabel anything outside IAs as 0/outside
dat1 <- relabel_na(dat0, NoIA=4)
head(dat1)
check_ia(dat1)

#create time column, adjust if necessary
dat2 <- create_time_series(dat1, Adjust=100)

#select columns of right eye only
dat3 <- select_recorded_eye(data=dat2, Recording="R", WhenLandR="Right")

#get proportional data for time bins
check_samplingrate(dat3)
ds_options(SamplingRate=1000)
dat4 <- bin_prop(dat3, NoIA=4, BinSize=20, SamplingRate=1000)
check_samplingrate(dat4)
check_samples_per_bin(dat4)

#transform data into elogits/empirical log odds ratios 
dat5 <- transform_to_elogit(dat4, NoIA=4, ObsPerBin=20)

#create final data set
dat6 <- rename_columns(dat5, Labels=c(IA1="Target", IA2="Distractor1", IA3="Distractor2", IA4="Distractor3"))
final_data <- dat6 %>% 
  arrange(., Subject, trialID, Time)
head(final_data)
colnames(final_data)
str(final_data)
summary(final_data)



#plotting
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#grand average using event means: outside looks included
plot_avg(data = final_data, type = "proportion", xlim = c(-100, 1000), 
         IAColumns = c(IA_outside_P = "Outside", IA_Target_P = "Target", IA_Distractor1_P = "Distractor1", 
                       IA_Distractor2_P = "Distractor2", IA_Distractor3_P = "Distractor3"),
         Condition1 = NULL, Condition2 = NULL, Cond1Labels = NA, Cond2Labels = NA, ErrorBar = TRUE, VWPreTheme = FALSE) + 
  theme_minimal() + 
  scale_color_manual(values = cbPalette, labels = c("Outside", "Target", "Distractor1", "Distractor2", "Distractor3"))

#grand average using event means: outside looks excluded
plot_avg(data = final_data, type = "proportion", xlim = c(-100, 1000), 
         IAColumns = c(IA_Target_P = "Target", IA_Distractor1_P = "Distractor2", 
                       IA_Distractor2_P = "Distractor2", IA_Distractor3_P = "Distractor3"),
         Condition1 = NULL, Condition2 = NULL, Cond1Labels = NA, Cond2Labels = NA, ErrorBar = TRUE, VWPreTheme = FALSE) + 
  theme_minimal() + scale_color_manual(values = cbPalette, labels = c("Target", "Distractor1", "Distractor2", "Distractor3"))

#grand average using event means: target vs. other proportions
final_data2 = mutate(final_data, IA_234_P = IA_Distractor1_P + IA_Distractor2_P + IA_Distractor3_P)
plot_avg(data = final_data2, type = "proportion", xlim = c(-100, 1000), 
         IAColumns = c(IA_Target_P = "Target", IA_234_P = "Non-Targets"),
         Condition1 = NULL, Condition2 = NULL, Cond1Labels = NA, Cond2Labels = NA, ErrorBar = TRUE, VWPreTheme = FALSE) + 
  theme_minimal() + scale_color_manual(values = cbPalette, labels = c("Target", "Non-Target"))
