###############################################################
# Code to read in State unemployment data from CPS website and apply the Sahm Rule to the states.
#
#
# Mike Konczal
# Last updated 7/12/2023

library(hrbrthemes)
library(janitor)
library(tidyverse)
library(ggtext)
library(ggrepel)
library(huxtable)
library(scales)
library(tidytext)
library(lubridate)
library(zoo)
library(data.table)
library(httr)

##### Load Theme #####
theme_lass <-   theme_modern_rc(ticks = TRUE) + theme(legend.position = "none", legend.title = element_blank(),
                                                      panel.grid.major.y = element_line(size=0.5),
                                                      panel.grid.minor.y = element_blank(),
                                                      plot.title.position = "plot",
                                                      axis.title.x = element_blank(),
                                                      axis.title.y = element_blank(),
                                                      plot.title = element_text(size = 25, face="bold"),
                                                      plot.subtitle = element_text(size=15, color="white"),
                                                      plot.caption = element_text(size=10, face="italic"),
                                                      legend.text = element_text(size=12),
                                                      axis.text.y = element_text(size=12, face="bold"),
                                                      axis.text.x = element_text(size=12, face="bold"),
                                                      strip.text = element_text(face = "bold", color="white", hjust = 0.5, size = 10),
                                                      panel.grid.major.x = element_blank(),
                                                      panel.grid.minor.x = element_blank(),
                                                      strip.background = element_blank()) +
  theme(text = element_text(family = "Larsseit"),
        plot.title = element_text(family = "Larsseit"),
        plot.subtitle = element_text(family = "Larsseit"),
        plot.caption = element_text(family="Larsseit"),
        strip.text = element_text(family="Larsseit"))



#### Load Data ####

lau <- GET("https://download.bls.gov/pub/time.series/la/la.data.3.AllStatesS", user_agent("rortybomb@gmail.com")) %>%
  content(as = "text") %>%
  fread() %>%
  clean_names()
lau$value <- as.numeric(lau$value)
lau$date <- paste(substr(lau$period, 2,3), "01", lau$year, sep="/")
lau$date <- as.Date(lau$date, "%m/%d/%Y")

lau_series <- GET("https://download.bls.gov/pub/time.series/la/la.series", user_agent("rortybomb@gmail.com")) %>%
  content(as = "text") %>%
  fread() %>%
  clean_names()
lau_series$series_id <- str_trim(lau_series$series_id)


la_area_type <- GET("https://download.bls.gov/pub/time.series/la/la.area_type", user_agent("rortybomb@gmail.com")) %>%
  content(as = "text") %>%
  fread() %>%
  clean_names()

la_state_region <-  GET("https://download.bls.gov/pub/time.series/la/la.state_region_division", user_agent("rortybomb@gmail.com")) %>%
  content(as = "text") %>%
  fread() %>%
  clean_names()

lau <- lau %>%
  inner_join(lau_series, by="series_id") %>%
  inner_join(la_area_type, by="area_type_code") %>%
  inner_join(la_state_region, by="srd_code")


rm(lau_series, la_area_type)


#### Sahm Rule by State ####

state_unemployment <- lau %>% filter(!is.na(date)) %>% filter(measure_code == 3) %>%
  select(date, state_text = srd_text, ur = value)

a <-
  state_unemployment %>% group_by(state_text) %>%
  mutate(last_three_months = (ur + lag(ur,1) + lag(ur,2))/3) %>%
  mutate(lowest_previous = rollapply(ur, width = 12, FUN = min, align = "right", fill = NA)) %>%
  ungroup() %>% filter(!is.na(lowest_previous)) %>%
  mutate(violating = last_three_months > lowest_previous + 0.5) %>%
  group_by(date) %>%
  summarize(n = sum(violating)) %>%
  ungroup()

MI_dates <- unique(a$date)
MI_dates <- sort(MI_dates, decreasing = TRUE)
MI_dates = MI_dates[seq(1, length(MI_dates), 12)]


a %>%
  filter(date >= "2010-01-01") %>%
ggplot() + geom_line(aes(x=date,y=n)) + theme_lass +
  scale_x_date(date_labels = "%b\n%Y", breaks=MI_dates) +
  labs(title="Number of States Triggering the Sahm Rule Declined Recently",
       subtitle="Number of states where the average 3-month state-level unemployment rate is 0.5 above lowest level over past 12 months.",
       y="Number of States",
       caption="LAU, Seasonally-Adjusted, Author's Calculations. Mike Konczal, Roosevelt Institute")

ggsave("graphics/sahm_state.png", dpi="retina", width = 12, height=6.75, units = "in")



# Recessions!
recessions.df = read.table(textConnection(
  "Peak, Trough
1857-06-01, 1858-12-01
1860-10-01, 1861-06-01
1865-04-01, 1867-12-01
1869-06-01, 1870-12-01
1873-10-01, 1879-03-01
1882-03-01, 1885-05-01
1887-03-01, 1888-04-01
1890-07-01, 1891-05-01
1893-01-01, 1894-06-01
1895-12-01, 1897-06-01
1899-06-01, 1900-12-01
1902-09-01, 1904-08-01
1907-05-01, 1908-06-01
1910-01-01, 1912-01-01
1913-01-01, 1914-12-01
1918-08-01, 1919-03-01
1920-01-01, 1921-07-01
1923-05-01, 1924-07-01
1926-10-01, 1927-11-01
1929-08-01, 1933-03-01
1937-05-01, 1938-06-01
1945-02-01, 1945-10-01
1948-11-01, 1949-10-01
1953-07-01, 1954-05-01
1957-08-01, 1958-04-01
1960-04-01, 1961-02-01
1969-12-01, 1970-11-01
1973-11-01, 1975-03-01
1980-01-01, 1980-07-01
1981-07-01, 1982-11-01
1990-07-01, 1991-03-01
2001-03-01, 2001-11-01
2007-12-01, 2009-06-01
2020-02-01, 2020-04-01"), sep=',',
  colClasses=c('Date', 'Date'), header=TRUE)

# But just the ones we need.
recessions.trim = subset(recessions.df, Peak >= min(a$date) )

a %>%
  ggplot() + geom_line(aes(x=date,y=n)) + theme_lass +
  scale_x_date(date_labels = "%Y") +
  labs(title="Number of States Triggering the Sahm Rule Across Years",
       subtitle="Number of states where the average 3-month state-level unemployment rate is 0.5 above lowest level over past 12 months.",
       y="Number of states",
       caption="LAU, Seasonally-Adjusted, Author's Calculations. Mike Konczal, Roosevelt Institute") +
  geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4, size=0)

ggsave("graphics/sahm_state_long.png", dpi="retina", width = 12, height=6.75, units = "in")



#### A graphic with four states ####
state_unemployment %>% filter(state_text %in% c("Wyoming","Vermont","North Carolina", "California")) %>%
  filter(date >= "2021-01-01") %>%
  ggplot(aes(date,ur, color=state_text)) + geom_line(size=1.2) + theme_lass + theme(legend.position = c(0.5,0.85)) +
  labs(subtitle="State level unemployment - NC, VT, and WY were triggering the Sahm Rule, now they aren't.",
       caption="LAU, Seasonally-Adjusted, Author's Calculations. Mike Konczal, Roosevelt Institute")

ggsave("sahm_states.png", dpi="retina", width = 12, height=6.75, units = "in")

#### Sahm Rule by percentage of employment ####
jobs <-
  lau %>% filter(!is.na(date)) %>% filter(measure_code == 5) %>%
  select(date, state_text = srd_text, jobs = value) %>%
  group_by(date) %>%
  mutate(total_jobs = sum(jobs)) %>%
  ungroup() %>%
  mutate(percent_jobs = jobs/total_jobs)

b <-
state_unemployment %>% group_by(state_text) %>%
  mutate(last_three_months = (ur + lag(ur,1) + lag(ur,2))/3) %>%
  mutate(lowest_previous = rollapply(ur, width = 12, FUN = min, align = "right", fill = NA)) %>%
  ungroup() %>% filter(!is.na(lowest_previous)) %>%
  mutate(violating = last_three_months > lowest_previous + 0.5) %>%
  left_join(jobs, by=c("date","state_text")) %>%
  group_by(date) %>%
  summarize(percent_employment = sum(violating*percent_jobs))




b %>%
  ggplot() + geom_line(aes(x=date,y=percent_employment)) + theme_lass +
  scale_x_date(date_labels = "%Y") +
  labs(title="Percent of Employment in States Triggering the Sahm Rule",
       subtitle="Preliminary. Sahm Rule = states wherethe average 3-month state-level unemployment rate is 0.5 above lowest level over past 12 months.",
       y="Number of states",
       caption="LAU, Seasonally-Adjusted, Author's Calculations. Mike Konczal, Roosevelt Institute") +
  geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4, size=0) +
  scale_y_continuous(labels = percent)

ggsave("graphics/percent_employment.png", dpi="retina", width = 12, height=6.75, units = "in")