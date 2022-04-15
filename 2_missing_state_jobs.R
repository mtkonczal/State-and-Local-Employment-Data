setwd("/Users/mkonczal/Documents/GitHub/State-and-Local-Employment-Data/")

library(janitor)
library(tidyverse)
library(ggtext)


##### SET UP SOME THINGS #####
#source(file = "1_load_clean_sae_data.R")
load("data/sae.RData")


############### PART 3: ANALYSIS ############################################################################

sae_S <- sae %>%
  #Note we filter on supersector code 90 here, for public sector, and seasonally-adjusted, and total state level
  filter(data_type_code == "01", seasonal == "S", area_code == "00000", supersector_code == "90") %>%
  group_by(industry_code, state_code) %>%
  mutate(covid_baseline = value[date=="2020-01-01"]) %>%
  mutate(pre_GR = value[date=="2008-12-01"]) %>%
  mutate(post_GR = value[date=="2011-12-01"]) %>%
  mutate(halfway_covid = value[date=="2021-01-01"]) %>%
  ungroup() %>%
  mutate(covid_change = value-covid_baseline, covid_percent_change = covid_change/covid_baseline) %>%
  mutate(GR_change = pre_GR-post_GR, GR_percent_change = GR_change/pre_GR)

by_total <- sae_S %>% filter(industry_code == "90000000", date == max(date))


#BELOW HERE IS BAD CODE NOW - LET'S MAKE IT GOOD WITH THE NEW STUFF NOW!
#ALL THE INDUSTRY CODES WORK! ALREADY
by_total <- sae_change(sae, "90000000", "S", 2019, 2022, "M01")
by_total_GR <- sae_change(sae, "90000000", "S", 2008, 2011, "M12")
by_total_2019 <- sae_change(sae, "90000000", "S", 2019, 2020, "M12")
by_total_2020 <- sae_change(sae, "90000000", "S", 2020, 2022, "M01")

by_total$GR <- by_total_GR$gov_job_loss_percent
by_total$y2019 <- by_total_2019$gov_job_loss_percent
by_total$y2020 <- by_total_2020$gov_job_loss_percent
by_total

by_federal <- sae_change(sae, "90910000", "S", 2019, 2021, "M12")
by_state <- sae_change(sae, "90920000", "S", 2019, 2021, "M12")
by_local <- sae_change(sae, "90930000", "S", 2019, 2021, "M12")

hist(by_total$gov_job_loss_percent)
sum(by_total$gov_job_loss)
sum(by_federal$gov_job_loss)
sum(by_state$gov_job_loss)
sum(by_local$gov_job_loss)

by_total_U <- sae_change(sae, "90000000", "U", 2019, 2021, "M12")
by_federal_U <- sae_change(sae, "90910000", "U", 2019, 2021, "M12")
by_state_U <- sae_change(sae, "90920000", "U", 2019, 2021, "M12")
by_local_U <- sae_change(sae, "90930000", "U", 2019, 2021, "M12")
by_state_UE <- sae_change(sae, "90921611", "U", 2019, 2021, "M12")
by_local_UE <- sae_change(sae, "90931611", "U", 2019, 2021, "M12")
by_state_UNE <- sae_change(sae, "90922000", "U", 2019, 2021, "M12")
by_local_UNE <- sae_change(sae, "90932000", "U", 2019, 2021, "M12")

sum(by_total_U$gov_job_loss)
sum(by_federal_U$gov_job_loss)
sum(by_state_U$gov_job_loss)
sum(by_local_U$gov_job_loss)
sum(by_state_UE$gov_job_loss)
sum(by_local_UE$gov_job_loss)
sum(by_state_UNE$gov_job_loss)
sum(by_local_UNE$gov_job_loss)

summary(by_local_UE$gov_job_loss_percent)
summary(by_local_UNE$gov_job_loss_percent)

summary(by_state_UE$gov_job_loss_percent)
summary(by_state_UNE$gov_job_loss_percent)


################ PART 3: GRAPHICS ################################


# JOIN THEM INSTEAD - OR CHECK THAT THEY ACTUALLY MATCH IN STATE NAME
# Plot
by_total <- by_total %>% 
  rowwise() %>% 
  arrange(covid_percent_change) %>% 
  mutate(state_name=factor(state_name, state_name))

by_total <- by_total %>%
  filter(state_name != "Virgin Islands") %>%
  filter(state_name != "District of Columbia") %>%
  filter(state_name != "Puerto Rico")

# FIRST GRAPHIC - LOSSES ACROSS STATES
ggplot(by_total, aes(x=state_name, y=covid_percent_change)) +
  geom_segment( aes(x=state_name, xend=state_name, y=0, yend=covid_percent_change), color="grey") +
  geom_point( color="dark red", size=3) +
  theme_light() +
  coord_flip() +
  xlab("") +
  ylab("Percent") +
  ggtitle("Change in State and Local Government Employment, Dec 2019 to Dec 2021") +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title.position = "plot",
    panel.border = element_blank(),
    axis.ticks.y = element_blank() 
  ) +
  labs(caption = "Data: BLS, State Employment and Unemployment data; Author's Calculations. @rortybomb")
ggsave("graphics/state_local_loss.png")

# SECOND GRAPHIC - LOSSES ACROSS YEARS
by_total_years <- by_total %>%
  arrange(y2019)

ggplot(by_total_years) +
  geom_segment( aes(x=state_name, xend=state_name, y=gov_job_loss_percent, yend=y2020), color="grey") +
  geom_point( aes(x=state_name, y=gov_job_loss_percent), color="dark red", size=3 ) +
  geom_point( aes(x=state_name, y=y2020), color="#228b22", size=3 ) +
  coord_flip()+
  theme_light() +
  theme(panel.grid.major.x = element_blank(),
                        plot.title = element_markdown()) +
  ggtitle("Change in State and Local Government Employment, <span style = 'color:#8b0000;'>2019 to 2020</span> and <span style = 'color:#228b22;'>2020 to 2021</span>, all December") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    plot.title.position = "plot",
    axis.ticks.y = element_blank()
  ) +
  xlab("") +
  ylab("Percent") +
  geom_hline(yintercept=0, linetype="solid", color = "black", alpha=0.5) +
  labs(caption = "Data: BLS, State Employment and Unemployment data; Author's Calculations. @rortybomb")
ggsave("graphics/losses_by_years.png")


# THIRD GRAPHIC - COMPARING NOW TO GREAT RECESSION
ggplot(by_total) +
  geom_segment( aes(x=state_name, xend=state_name, y=covid_percent_change, yend=GR_percent_change), color="grey") +
  geom_point( aes(x=state_name, y=covid_percent_change), color="dark red", size=3 ) +
  geom_point( aes(x=state_name, y=GR_percent_change), color="#228b22", size=3 ) +
  coord_flip()+
  theme_light() +
  theme(panel.grid.major.x = element_blank(),
        plot.title = element_markdown()) +
  ggtitle("Change in State and Local Government Employment, <span style = 'color:#8b0000;'>2019 to 2021</span> and <span style = 'color:#228b22;'>2008 to 2011</span>, all December") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    plot.title.position = "plot",
    axis.ticks.y = element_blank()
  ) +
  xlab("") +
  ylab("Percent") +
  geom_hline(yintercept=0, linetype="solid", color = "black", alpha=0.5) +
  labs(caption = "Data: BLS, State Employment and Unemployment data; Author's Calculations. @rortybomb")
ggsave("graphics/losses_vs_Great_Recession.png")

# THIRD GRAPHIC - STATE VERSUS LOCAL?


# FOURTH GRAPHIC - EDUCATION?

by_total
