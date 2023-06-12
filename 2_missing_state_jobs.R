setwd("/Users/mkonczal/Documents/GitHub/State-and-Local-Employment-Data/")

library(janitor)
library(tidyverse)
library(ggtext)
library(scales)


##### SET UP SOME THINGS #####
source(file = "1_load_clean_sae_data.R")



############### PART 1: DATA ############################################################################
# This excludes federal jobs, so it's just state and local jobs, under the assumption
# that the states and localities do not control federal jobs with their budgets.
# sae_JS is Just States, no federal jobs
sae_JS <- sae %>%
  #Note we filter on supersector code 90 here, for public sector, seasonally-adjusted, total state level, and not annual
  filter(data_type_code == 1, seasonal == "S", area_code == 0, supersector_code == 90, period != "M13") %>%
  filter(industry_code %in% c(90920000,90930000)) %>%
  group_by(date, state_name) %>%
  summarize(value = sum(value)) %>% ungroup() %>%
  group_by(state_name) %>%
  mutate(covid_baseline = value[date=="2020-01-01"]) %>%
  mutate(pre_GR = value[date=="2008-12-01"]) %>%
  mutate(post_GR = value[date=="2011-12-01"]) %>%
  mutate(halfway_covid = value[date=="2022-04-01"]) %>%
  ungroup() %>%
  mutate(covid_change = value-covid_baseline, covid_percent_change = covid_change/covid_baseline) %>%
  mutate(GR_change = post_GR-pre_GR, GR_percent_change = GR_change/pre_GR) %>%
  mutate(change_2021 = halfway_covid-covid_baseline, change_2021_percent = change_2021/covid_baseline) %>%
  # Keeping just states for now
  filter(state_name != "Virgin Islands") %>%
  filter(state_name != "District of Columbia") %>%
  filter(state_name != "Puerto Rico")


maxdate <- max(sae_JS$date)
maxdate <- as.character(format(maxdate, format="%b %Y"))

###### GRAPHIC 1 ######
# This is total losses by state.
sae_JS %>% filter(date == max(date)) %>%
  mutate(ordered_change = fct_reorder(state_name, covid_percent_change)) %>%
  ggplot(aes(x=ordered_change, y=covid_percent_change)) + geom_bar(stat = "identity", fill="skyblue") +
  geom_point(aes(x=ordered_change, y=change_2021_percent), size=2) +
  coord_flip() + theme_classic() +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title.position = "plot",
    panel.border = element_blank(),
    axis.ticks.y = element_blank()) +
  theme(title = element_text(face="plain", size=26), plot.caption = element_text(size=12), axis.text.x=element_text(size=20)) +
  labs(caption = "Data: BLS, State Employment and Unemployment data; Author's Calculations. Mike Konczal, Roosevelt Institute") +
  scale_y_continuous(labels = scales::percent) +
  labs(x="", y="",
       subtitle=paste("Change in state and local government employment: blue bar is Jan 2020 to ", maxdate, ", dots are Jan 2020 to Apr 2022.", sep=""),
       title="State and Local Government Employment is Still Recovering",
       caption="Data: BLS, State and Area Employment. Seasonally Adjusted. Author's Calculations. Mike Konczal, Roosevelt Institute")
  
ggsave("graphics/sae_1.png", width = 19, height=10.68, dpi="retina")


###### GRAPHIC 2 - COMPARING NOW TO GREAT RECESSION ######
sae_JS %>% filter(date == max(date)) %>%
  mutate(ordered_change = fct_reorder(state_name, covid_percent_change)) %>%
  ggplot() +
  geom_segment( aes(x=ordered_change, xend=ordered_change, y=covid_percent_change, yend=GR_percent_change), color="grey") +
  geom_point( aes(x=ordered_change, y=covid_percent_change), color="dark red", size=3 ) +
  geom_point( aes(x=ordered_change, y=GR_percent_change), color="#228b22", size=3 ) +
  coord_flip()+
  theme_light() +
  theme(panel.grid.major.x = element_blank(),
        plot.title = element_markdown()) +
  ggtitle("Change in State and Local Government Employment, <span style = 'color:#8b0000;'>2020 to 2022</span> and <span style = 'color:#228b22;'>2009 to 2011</span>, all December") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.border = element_blank(),
    plot.title.position = "plot",
    axis.ticks.y = element_blank()) +
  theme(title = element_text(face="plain", size=22), plot.caption = element_text(size=12), axis.text.x=element_text(size=20)) +
  geom_hline(yintercept=0, linetype="solid", color = "black", alpha=0.5) +
  labs(x="", y="",
       subtitle="The Great Recession Saw Wide Variance in State and Local Job Losses",
       caption="Data: BLS, State Employment and Unemployment data. Seasonally Adjusted. Author's Calculations. Mike Konczal, Roosevelt Institute") +
  scale_y_continuous(labels = scales::percent)

ggsave("graphics/sae_2.png", width = 19, height=10.68, dpi="retina")



# THIRD GRAPHIC - LOSSES ACROSS YEARS
sae_JS %>% filter(date == max(date)) %>%
  mutate(halfway_percent = halfway_covid/covid_baseline-1) %>%
  mutate(ordered_change = fct_reorder(state_name, halfway_percent)) %>%
  ggplot() +
  geom_segment( aes(x=ordered_change, xend=ordered_change, y=covid_percent_change, yend=halfway_percent), color="grey") +
  geom_point( aes(x=ordered_change, y=covid_percent_change), color="dark red", size=3 ) +
  geom_point( aes(x=ordered_change, y=halfway_percent), color="#228b22", size=3 ) +
  coord_flip()+
  theme_light() +
  theme(panel.grid.major.x = element_blank(),
        plot.title = element_markdown()) +
  ggtitle("Change in State and Local Government Employment, <span style = 'color:#228b22;'>2020 to 2021</span> and <span style = 'color:#8b0000;'>2021 to Current</span>") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    plot.title.position = "plot",
    axis.ticks.y = element_blank()
  ) +
  theme(title = element_text(face="plain", size=22), plot.caption = element_text(size=12), axis.text.x=element_text(size=20)) +
  xlab("") +
  geom_hline(yintercept=0, linetype="solid", color = "black", alpha=0.5) +
  scale_y_continuous(labels = scales::percent) +
  labs(caption = "Data: BLS, State Employment and Unemployment data. Seasonally Adjusted. Author's Calculations. Mike Konczal, Roosevelt Institute.",
       subtitle = "Recovery is Roughly Constant; Bigger Losses Haven't Seen Faster Job Recoveries")

ggsave("graphics/sae_3.png", width = 19, height=10.68, dpi="retina")


#### GRAPHIC 4 - EDUCATION VERSUS NON-EDUCATION ####
sae %>%
  filter(period != "M13") %>%
  # 90931611 is state education employment, 90922000 is local education employment
  filter(industry_code == "90931611" | industry_code == "90932000" |
           industry_code == "90921611" | industry_code == "90922000") %>%
  mutate(is_edu = industry_code == 90921611 | industry_code == 90931611) %>%
  filter(year == 2022 | (year == 2019 & date < "2019-07-01")) %>%
  group_by(year, is_edu, state_name) %>% summarize(total_employment = sum(value)) %>%
  ungroup() %>% group_by(state_name, is_edu) %>%
  summarize(per_diff = (total_employment[year==2022]-total_employment[year==2019])/total_employment[year==2019]) %>%
  ungroup() %>%
  pivot_wider(names_from = is_edu, values_from = per_diff) %>%
  rename(edu = `TRUE`, non_edu = `FALSE`)%>%
  ggplot(aes(non_edu, edu)) + geom_point() + theme_classic() +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title.position = "plot",
    panel.border = element_blank(),
    axis.ticks.y = element_blank()) +
  theme(title = element_text(face="plain", size=22), plot.caption = element_text(size=12)) +
  labs(caption = "Data: BLS, State Employment and Unemployment data. Seasonally unadjusted data. Author's Calculations. Mike Konczal, Roosevelt Institute") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(x="Change in Noneducation Employment", y="Change in Education Employment",
       subtitle=paste("Education is 58 Percent of State and Local Government Jobs, and Responsible for 70 Percent of Losses"),
       title="State and Local Losses Are Both in Education and Broader, First Six Months of 2019 versus 2022",
       caption="Data: BLS, State and Area Employment. Seasonally Unadjusted. Author's Calculations. Mike Konczal, Roosevelt Institute") +
  geom_hline(yintercept=0, linetype="solid", color = "black", alpha=0.5) +
  geom_vline(xintercept=0, linetype="solid", color = "black", alpha=0.5)

ggsave("graphics/sae_4.png", width = 19, height=10.68, dpi="retina")






sae %>%
  filter(period != "M13") %>%
  # 90931611 is state education employment, 90922000 is local education employment
  filter(industry_code == "90931611" | industry_code == "90932000" |
           industry_code == "90921611" | industry_code == "90922000") %>%
  mutate(is_edu = industry_code == 90921611 | industry_code == 90931611) %>%
  filter(year == 2022 | (year == 2019 & date < "2019-07-01")) %>%
  group_by(year, is_edu, state_name) %>% summarize(total_employment = sum(value)) %>%
  ungroup() %>% group_by(state_name, is_edu) %>%
  summarize(per_diff = (total_employment[year==2022]-total_employment[year==2019])) %>%
  pivot_wider(names_from = is_edu, values_from = per_diff) %>%
  ungroup() %>% summarize(edu = sum(`TRUE`), nonedu = sum(`FALSE`)) %>%
  summarize(edu/(edu+nonedu))

sae %>%
  filter(period != "M13") %>%
  # 90931611 is state education employment, 90922000 is local education employment
  filter(industry_code == "90931611" | industry_code == "90932000" |
           industry_code == "90921611" | industry_code == "90922000") %>%
  mutate(is_edu = industry_code == 90921611 | industry_code == 90931611) %>%
  filter(date == "2019-06-01") %>%
  group_by(date, is_edu, state_name) %>% summarize(total_employment = sum(value)) %>%
  ungroup() %>%
  pivot_wider(names_from = is_edu, values_from = total_employment) %>%
  ungroup() %>% summarize(edu = sum(`TRUE`), nonedu = sum(`FALSE`)) %>%
  summarize(edu/(edu+nonedu))



#### STANDBY GRAPHIC - A DIFFERENT WAY TO PRESENT GRAPHIC 3 ####
sae_JS %>% filter(date == max(date)) %>%
  mutate(halfway_percent = halfway_covid/covid_baseline-1)  %>%
  mutate(change_since_halfway = GR_percent_change - halfway_percent) %>%
  select(state_name, change_since_halfway) %>%
  mutate(ordered_change = fct_reorder(state_name, change_since_halfway)) %>%
  ggplot(aes(x=ordered_change, y=change_since_halfway)) + geom_bar(stat = "identity", fill="skyblue") + coord_flip() + theme_classic() +
  xlab("") +
  ylab("") +
  ggtitle("Change in State and Local Government Employment, Jan 2020 to Current") +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title.position = "plot",
    panel.border = element_blank(),
    axis.ticks.y = element_blank() 
  ) +
  labs(caption = "Data: BLS, State Employment and Unemployment data; Author's Calculations. Mike Konczal, Roosevelt Institute") +
  scale_y_continuous(labels = scales::percent) +
  labs(x="", y="",
       subtitle="Change in State and Local Government Employment, Jan 2020 to Current",
       title="Decrease in Employment is Across All States",
       caption="Data: BLS, State Employment and Unemployment data; Author's Calculations. Mike Konczal, Roosevelt Institute")


stateU <- read_csv("data/stateU.csv") %>% rename(state_name = State)


a <- sae_JS %>% filter(date==max(date)) %>%
  select(date, state_name, GR_percent_change) %>%
  left_join(stateU, by="state_name") 
b <- lm(GR_percent_change ~ UnemploymentRate, data=a)
summary(b)

sae_JS %>% filter(date==max(date)) %>%
  select(date, state_name, GR_percent_change) %>%
  left_join(stateU, by="state_name") %>%
  ggplot(aes(UnemploymentRate,GR_percent_change)) + geom_point() + theme_classic() +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title.position = "plot",
    panel.border = element_blank(),
    axis.ticks.y = element_blank()) +
  theme(title = element_text(face="plain", size=22), plot.caption = element_text(size=12)) +
  labs(caption = "Data: BLS, State Employment and Unemployment data. Seasonally unadjusted data. Author's Calculations. Mike Konczal, Roosevelt Institute") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(y="Change in State and Local Public Employment", x="State Leve Unemployment Rate",
       subtitle=paste("Education is 58 Percent of State and Local Government Jobs, and Responsible for 70 Percent of Losses"),
       title="State and Local Losses Are Both in Education and Broader, First Six Months of 2019 versus 2022",
       caption="Data: BLS, State and Area Employment. Seasonally Unadjusted. Author's Calculations. Mike Konczal, Roosevelt Institute") +
  geom_hline(yintercept=0, linetype="solid", color = "black", alpha=0.5) +
  geom_vline(xintercept=0, linetype="solid", color = "black", alpha=0.5)

ggsave("graphics/sae_4.png", width = 19, height=10.68, dpi="retina")
