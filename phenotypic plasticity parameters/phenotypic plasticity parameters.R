rm(list = ls())
library(dplyr)
library(openxlsx)
library(emmeans)
library(stringr)
library(tibble)
library(reshape2)
library(ggplot2)
library(ggpmisc)
library(broom)
setwd("./phenotypic plasticity parameters/")
data =read.xlsx("./data.sample.xlsx")

## data is unbalanced
table(data$Env,data$ID)

i="PH"

MEM = data%>%
  group_by(Env,ID)%>%
  summarise(!!i := mean(!!sym(i),na.rm=T)) %>%
  group_by(Env)%>%
  mutate(EM = mean(!!sym(i),na.rm=T))%>%
  distinct(Env,.keep_all = T)%>%
  ungroup(Env)%>%
  summarise(MEM = mean(EM))%>%
  .$"MEM"

data2= data%>%
  group_by(Env,ID)%>%
  summarise(!!i := mean(!!sym(i),na.rm=T)) %>%
  group_by(Env)%>%
  mutate(EM = mean(!!sym(i),na.rm=T))%>%
  ungroup(Env)%>%
  mutate(ej = EM - MEM)

JRA <- lm(PH ~ ID/ej-1, data=data2) 

tidy_JRA= tidy(JRA)

# Intercepts: terms that start with ID but NOT contain :
intercepts <- tidy_JRA %>%
  filter(grepl("^ID", term) & !grepl(":", term)) %>%
  mutate(ID = gsub("^ID", "", term)) %>%
  select(ID, intercept = estimate)

# Slopes: interaction terms ID:ej
slopes <- tidy_JRA %>%
  filter(grepl("^ID.*:ej$", term)) %>%
  mutate(ID = gsub("^ID(.*):ej$", "\\1", term)) %>%
  select(ID, slope = estimate)

# Join together
ID_effects <- left_join(intercepts, slopes, by = "ID")

head(ID_effects)
