###Tutorial VWPre for data preprocessing###

library(VWPre)
library(ggplot2)


setwd("C:/Users/hgaud/OneDrive/Studium/ADVANCED3_LanguageAndLinguistics")
data <- read.delim("practice_data.txt", na.strings=c(".", "NA"))
head(data)
colnames(data)

#checks data columns (specify subject and item), conversion to correct form if necessary
dat0 <- prep_data(data, Subject="RECORDING_SESSION_LABEL") #our items are specified as "trialID" and we already explicitly export that from OpenSesame so we don't actually need the Item argument here
head(dat0)

#relabel anything outside IAs as 0/outside
dat1 <- relabel_na(dat0, NoIA=4)
head(dat1)
check_ia(dat1)

#create time column, adjust if necessary
dat2 <- create_time_series(dat1)

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
#it's always good to know what we should expect to see when we plot. do you see something you would expect (or not expect) in these plots?
#xlim ends up referring to our time, how long are our sentences? do these limits cover our sentence length? 
#our data is a little different than some other datasets
#many datasets use limits like this (-100:1000) since their 0 time refers to target onset (since they don't have normalized sentence lengths)
#all of our targets occur at exactly the same time, so we don't need this kind of offsetting (our sentences are 1903 ms long)
#right now, this plots all of our data with no faceting
#what we are interested in is how our look proportions change based on our conditions (what are those conditions?)
#if we specify those conditions using the "Condition1 = " and "Condition2 = " arguments (which are currently null) our graphs will be "faceted" and we can see differences between conditions
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
#this is good! this target vs nontarget comparison is commonly done, was this an idea of yours or is this also in the tutorial? there is however still a problem here 
#(probably two problems, which are relatively obvious when actually looking at the graph)
#hints: your IA_234_P line is roughly 3x higher than the other line.. why may that be?
#seems like their might be a labeling problem? the first mistake makes the labeling problem quite a bit more obvious as well
final_data2 = mutate(final_data, IA_234_P = IA_Distractor1_P + IA_Distractor2_P + IA_Distractor3_P)
plot_avg(data = final_data2, type = "proportion", xlim = c(-100, 1000), 
         IAColumns = c(IA_Target_P = "Target", IA_234_P = "Non-Targets"),
         Condition1 = NULL, Condition2 = NULL, Cond1Labels = NA, Cond2Labels = NA, ErrorBar = TRUE, VWPreTheme = FALSE) + 
  theme_minimal() + scale_color_manual(values = cbPalette, labels = c("Target", "Non-Target"))
