
library(nhanesA)
library(writexl)
library(tidyverse)

dir.create("data", showWarnings = FALSE)

#pull in data from from NHANES and save it

#DEMO 

demo_raw <- nhanes('P_DEMO') |> tibble()

saveRDS(demo_raw, "data/P_DEMO.Rds")

#BMI 
bmi_raw <- nhanes('P_BMX') |> tibble()

saveRDS(bmi_raw, "data/P_BMX .Rds")

#Fertility lavs 
amh_raw <- nhanes('P_TST') |> tibble()

saveRDS(amh_raw, "data/P_TST.Rds")

#REGULAR MENSES 
menses_raw <- nhanes('P_RHQ') |> tibble()

saveRDS(menses_raw, "data/P_RHQ.Rds")

# Now that data are saved, I can just read in the tibble

demo_raw <- readRDS("data/P_DEMO.Rds")
bmi_raw <- readRDS("data/P_BMX .Rds")
amh_raw <- readRDS("data/P_TST.Rds")
menses_raw <- readRDS("data/P_RHQ.Rds")

demo_reduced <- demo_raw |>
  select(SEQN,RIAGENDR, RIDAGEYR, RIDSTATR) 
bmi_reduced <- bmi_raw |>
  select(SEQN,BMXBMI) 
amh_reduced<- amh_raw |>
  select(SEQN,LBX17H, LBXAMH, LBXEST,LBXFSH, LBXLUH, LBXPG4) 
menses_reduced <- menses_raw |>
  select(SEQN,RHQ031,RHQ074, RHQ076) 


NEW <- left_join(demo_reduced, bmi_reduced, by = "SEQN")
NEW2 <- left_join(NEW, amh_reduced, by = "SEQN")
NEW3 <- left_join(NEW2, menses_reduced, by = "SEQN")

write_xlsx(NEW3, "Project_B.xlsx")

#Clean the data 
Clean_data <- read_excel("Project_B.xlsx")

#Select only female 
Clean_data <- Clean_data |>
  filter(RIAGENDR == "Female")

#Select only those interviewed and tested
Clean_data <- Clean_data |>
  filter(RIDSTATR == "Both interviewed and MEC examined")

#Select those over 18 and under 44 because per survey pregnancy related questions ended at this age
Clean_data <- Clean_data |>
  filter(RIDAGEYR > 18 & RIDAGEYR<44)

#Convert categorical variables to factors RHQ031
Clean_data$RHQ031<- factor(Clean_data$RHQ031, levels = c("Yes", "No"))
Clean_data<-Clean_data |>
  mutate(Clean_data=ifelse(RHQ031==1, "Regular Menses","Irregular Menses"))

#Convert categorical variables to factors RHQ074
Clean_data$RHQ074<- factor(Clean_data$RHQ074, levels = c("Yes", "No"))

#Convert categorical variables to factors RHQ076
Clean_data$RHQ076<- factor(Clean_data$RHQ076, levels = c("Yes", "No"))

write_xlsx(Clean_data, "Clean_Data.xlsx")

# Address missing numbers 
# Keeps only completely full rows for data for study 1
clean_data_study1 <- Clean_data %>% drop_na()

write_xlsx(clean_data_study1, "Clean_Data_Study1.xlsx")

#Analysis B (boxplot)

ggplot(clean_data_study1, aes(x=RHQ031, y = BMXBMI, fill = RHQ031))+
  geom_boxplot()+
  labs (title = "Average BMI Variance with Menstrual Regularity",
        x = "Regular Menses",
        y = "Irregular Menses")+
  theme_minimal()

#Analysis C

#Analysis D

#Analysis E

# Regression 

# Regression

# Comparison 




