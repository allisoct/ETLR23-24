library(tidyverse)
library(readxl)
library(VWPre)


urlET <- "https://raw.githubusercontent.com/allisoct/vwpcl/main/dfPPG.csv"
urlBehav <- "https://raw.githubusercontent.com/allisoct/vwpcl/main/pooled_data_included.csv"
dfEyeTracking <- read_csv(urlET)
dfBehavioral <- read_csv(urlBehav)
head(df)
df <- select(df, -...1, -...2)
head(df)


dfPPG <- read_csv("GitHub/ETLR23-24/R/practice_data/dfPPG.csv")


dfBehavBad <- read_excel("GitHub/ETLR23-24/R/practice_data/pooled_data_included.xlsx")


dfBehav <- read_excel("GitHub/ETLR23-24/R/practice_data/pooled_data_included.xlsx", sheet = "Sheet3")


head(dfBehavBad)
summary(dfBehavBad)
sum(is.na(dfBehavBad))



summary(dfBehav)


dfBehav <- dfBehav %>%
  rename("RAN_Digit" = "RAN.Digit..s." )


summary(dfBehav)

write.csv(dfBehav, "GitHub/ETLR23-24/R/practice_data/dfBehav.csv")
write_csv(dfBehav, "GitHub/ETLR23-24/R/practice_data/dfBehav2.csv")


mean(dfBehav$RAN.Color..s.)
sd(dfBehav$RAN.Color..s.)



summary(dfBehav$Direction)
unique(dfBehav$Direction)

dfBehav$RAN_Total <- dfBehav$RAN.Object..s. + dfBehav$RAN_Digit + dfBehav$RAN.Color..s.

dfAscending <- filter(dfPPG, Direction == "ascending")



df <- dfPPG %>%
  filter(Time >= 850 & Time <= 1590,
         predictability == "predictable",
         vwp_correct == 1,
         Direction == "ascending")



p1 = plot_avg(data = dfPPG, type = "proportion", xlim = c(0, 2000), 
              IAColumns = c(IA_Target_P = "Target",IA_anom_P = "Anomalous\nDistractor"),
              Condition1 = "predictability", Condition2 = "difficulty", 
              Cond1Labels = NA, Cond2Labels = NA, 
              ErrorBar = FALSE, ErrorBand = TRUE,
              ErrorType = "SE", #ConfLev = 95, CItype = "pointwise",
              VWPreTheme = FALSE) + theme_minimal()

p1
