library(here)
getwd()
library(tidyverse)
library(skimr)
library(modelr)
library(reticulate)
library(patchwork)
library(viridis)
library(ggpol)
library(apyramid)
library(ggcharts)
library(janitor)
df <- read_csv("data/Facebook data_13 Sept 2021.csv",
               col_types = list(level = col_factor(),
                                lgbt_support = col_factor(),
                                gender = col_factor(),
                                age_group = col_factor(),
                                n_millions = col_double(),
                                country = col_factor(),
                                POP = col_double(),
                                date_extracted = col_date(),
                                source = col_character()))
save(df,file="data/df.Rda")

skim(df)
# "National" = data set from FB data where "Population" is data from UN estimates of US Population

#Data Wrangling - subsets of data
df1 <- df %>% filter(level == "Population")
df2 <- df %>% filter(level == "National")

#EDA - bivariate tables
skim(df2)
df2 %>% group_by(gender,age_group) %>% 
  summarize(sum = sum(n_millions))
lgbtsupport_gender <- df2 %>% group_by(lgbt_support,gender) %>% 
  summarize(sum = sum(n_millions))
lgbtsupport_age <- df2 %>% group_by(lgbt_support,age_group) %>% 
  summarize(sum = sum(n_millions))

#contingency Tables & data viz for this
lgbtsupport_gender <- lgbtsupport_gender%>% pivot_wider(names_from=lgbt_support,values_from=sum)
lgbtsupport_age <- lgbtsupport_age%>% pivot_wider(names_from=lgbt_support,values_from=sum)
lgbtsupport_age

agegroup <- c("15-19","20-24","25-29","30-34","35-39","40-44",
              "45-49","50-54","55-59","60-64")
age <- as.table(as.matrix(lgbtsupport_age,rownames=agegroup),rownames=agegroup)
rownames(age) = agegroup
age = age[,-1]
age

genders <- c("Male","Female")
gender <- as.table(as.matrix(lgbtsupport_gender))
rownames(gender) = genders
gender = gender[,-1]
gender

library("gplots")
balloonplot(t(age), show.margins = FALSE,label=T,
            main="LGBT Support by Age Group")
balloonplot(t(gender), show.margins = FALSE,label=T,
            main="LGBT Support by Gender")


#Relevel so no support is the base
df2$lgbt_support <- relevel(df2$lgbt_support,ref="No Evidence")
levels(df2$lgbt_support)
str(df2)

save(df2,file="data/df2.Rda")

#simple tables
# library(qwraps2)

lgbt_support_summary_US <- df2 %>% 
  group_by(lgbt_support) %>% 
  summarise(sum = sum(n_millions)) %>% 
  mutate(perc = sum/sum(sum)*100)
lgbt_support_summary_US %>% adorn_totals()

age_group_summary_US <- df2 %>% 
  group_by(age_group) %>% 
  summarise(sum = sum(n_millions))%>% 
  mutate(perc = sum/sum(sum)*100)
age_group_summary_US %>% adorn_totals()

gender_summary_US <- df2 %>% 
  group_by(gender) %>% 
  summarise(sum = sum(n_millions))%>% 
  mutate(perc = sum/sum(sum)*100)
gender_summary_US %>% adorn_totals()

#EDA Graphs
py1 <- age_pyramid(df1,age_group=age_group,
                   split_by=gender,
                   count = n_millions,
                   show_midpoint=F)+
  labs(y="population in millions",title="US population in 2019", caption= "source:UNDESA")+
  scale_fill_manual(values = c('#4682b4','#ee7989'))
py1

py2 <- age_pyramid(df2, age_group=age_group,
                   split_by=gender,
                   count = n_millions,
                   show_midpoint=F)+
  labs(y="population in millions",title="US population on Facebook", caption= "source:Facebook Marketing Data")+
  scale_fill_manual(values = c('#4682b4','#ee7989'))
py2

py3 <- age_pyramid(df2, age_group=age_group,
                   split_by=gender,
                   count = n_millions,
                   show_midpoint=F,
                   stack_by = lgbt_support)+
  labs(y="population in millions",title="US population on Facebook and Instagram", caption= "source:Facebook Marketing Ads Manager (September 2021)")+
  scale_fill_manual(values = c('#f4cae4','#8dd3c7'))
py3


py1 + py2
py3

p1 <- ggplot(df2) +
 aes(x = age_group, fill = age_group, weight = n_millions) +
 geom_bar() +
 scale_fill_hue(direction = 1) +
 labs(title = "Facebook users in the United States by age and gender",
      y="Number in millions") +
 facet_wrap(vars(gender))+
  theme_nightblue()
p1

p2 <- ggplot(df2) +
  aes(x = age_group, fill = age_group, weight = n_millions) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  labs(title = "Facebook users in the United States by age",
       y="Number in millions") +
  theme_minimal()
p2

p2a <- df2 %>% filter(lgbt_support=="Yes") %>% 
  ggplot() +
  aes(x = age_group, fill = age_group, 
      weight = n_millions) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  labs(title = "Facebook users in the United States by age \n behavior of LGBTIQ+ support",
       y="Number in millions") +
  theme_minimal()
p2a

p2b <- df2 %>% filter(lgbt_support=="Yes") %>% 
  ggplot() +
  aes(x = age_group, fill = age_group, 
      weight = n_millions) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  labs(title = "Demonstrated LGBTIQ+ Support by age and sex",
       caption="source: Instgram and Facebook users in U.S.A. \n September 19, 2021",
       y="Number in millions") +
  theme_nightblue()+
  facet_wrap(vars(gender))
p2b

p3 <- ggplot(df2) +
  aes(x = gender, fill = gender, weight = n_millions) +
  geom_bar() +
  scale_fill_brewer(palette = "Accent") +
  labs(title = "Facebook users in the United States by gender",
       y="Number in millions") +
  theme_minimal()
p3

p4 <- ggplot(df2) +
  aes(x = gender, fill = gender, weight = n_millions) +
  geom_bar() +
  scale_fill_brewer(palette = "Accent") +
  labs(title = "Facebook users in the United States by gender",
       y="Number in millions") +
  theme_minimal()+
  facet_wrap(vars(lgbt_support))
p4

p5 <- ggplot(df2) +
 aes(x = lgbt_support, y = n_millions, fill = gender) +
 geom_bar(stat="identity") +
  scale_fill_brewer(palette = "Set2") +
 theme_minimal()
p5

df_male <- df2 %>% filter(gender == "Male")
df_female <- df2 %>% filter(gender == "Female")

p6 <- ggplot(df2) +
 aes(x = lgbt_support, y = n_millions, fill = age_group) +
 geom_boxplot(shape = "circle") +
 scale_fill_hue(direction = 1) +
 theme_minimal()
p6

p7 <- ggplot(df2) +
  aes(x = age_group, fill = age_group, weight = n_millions) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  labs(title = "Facebook users in the United States by age and gender \n
       by LGBT supportive behavior",
       y="Number in millions") +
  theme_classic() +
  facet_wrap(vars(lgbt_support))
p7

p8 <- ggplot(df2) +
  aes(x = age_group, y = n_millions, fill = lgbt_support) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#99d8c9","#fc9272")) +
  theme_classic()+
  labs(title="LGBTIQ+ support behavior on Facebook by age groups")
p8

p9 <- ggplot(df2) +
 aes(x = age_group, y = n_millions, fill = age_group) +
 geom_boxplot(shape = "circle") +
 scale_fill_hue(direction = 1) +
 theme_classic()+
  labs(title="Facebook Population in the US by Age Groups",
       caption="box plot is for lower and upper limits of count data of LGBTIQ+ support")
p9

#Graphs
py1 + py2
py3

p1
p2
p2a
p2b
p3
p4
p5
p6
p7
p8
p9


