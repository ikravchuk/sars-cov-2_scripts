library(tidyverse)
library(ggthemes)
library(scales)
library(hrbrthemes)

# War begins
war_begin <- "2022-02-24"

#WHO data
# Source: https://covid19.who.int/WHO-COVID-19-global-data.csv
global_cases <- read_csv("raw_data/WHO-COVID-19-global-data.csv")

# World Bank Data
# Source: https://wdi.worldbank.org/table/2.1
populations <- list(Ukraine = 43800000,
                    Poland = 37700000,
                    Czechia = 10500000,
                    Germany = 83200000) 

# 4 countries data for 1st half of the 2022 year
sel_cases <- global_cases %>%
  # Countries
  #filter(Country %in% c("Ukraine", "Poland", "Czechia", "Germany")) %>%
  filter(Country %in% c("Ukraine", "Poland")) %>%
  #Dates
  filter(Date_reported >= as.Date("2022-01-01") &
           Date_reported <= as.Date("2022-06-30")) %>%
  #New cases per 100,000
  mutate(cases_100 = (New_cases * 100000) / as.numeric(populations[Country])) %>%
  #weeks
  mutate(Week = as.Date(cut(Date_reported, breaks = "week", start.on.monday = FALSE)))

ggplot(sel_cases, aes(x = Week, y = New_cases, color = Country)) +
  stat_summary(fun = sum, geom = "line", linewidth = 0.7) +
  scale_x_date(labels = date_format("%Y-%m-%d"), breaks = "4 week")+
  scale_y_continuous(labels = scales::comma)+
  # Line when War begins
  geom_vline(xintercept = as.Date(war_begin),
             linetype = 2,
             color = "darkgreen",
             linewidth=0.5)


ggplot(sel_cases, aes(x = Week, y = New_cases, color = Country)) +
  stat_summary(fun = sum, geom = "line", linewidth = 0.7) +
  scale_x_date(labels = date_format("%Y-%m-%d"), breaks = "4 week")+
  scale_y_continuous(labels = scales::comma)+
  # Line when War begins
  geom_vline(xintercept = as.Date(war_begin),
             linetype = 2,
             color = "darkgreen",
             linewidth=0.5)

# fragment of data
sel_cases_frag <- sel_cases %>%
  filter(Country %in% c("Poland")) %>%
  filter(Date_reported >= as.Date("2022-01-01") &
           Date_reported <= as.Date("2022-04-30")) %>%
  select(Date_reported, New_cases, Week) %>%
  rename(data_date = Date_reported)

ggplot(sel_cases_frag, aes(x = Week, y = New_cases)) +
  stat_summary(fun.y = mean, geom = "line", linewidth = 0.7) +
  scale_x_date(labels = date_format("%Y-%m-%d"), breaks = "2 week")+
  # Line when War begins
  geom_vline(xintercept = as.Date(war_begin),
             linetype = 2,
             color = "darkgreen",
             linewidth=0.5)



## Crossing the border
cross_from_UA <- read.csv("raw_data/cross/clean_from_Ukraine.csv",fileEncoding="UTF-16LE", sep = ";")

cross_to_UA <- read_csv("raw_data/cross/clean_cross_to_ua.csv")

cross_to_UA <- cross_to_UA %>%
  rename(individuals_to = individuals)

cross_UA_cor <- merge(cross_from_UA, cross_to_UA, by="data_date")

sel_cross_from <- cross_UA_cor %>%
  filter(data_date >= as.Date("2022-01-15") &
           data_date <= as.Date("2022-04-30")) %>%
  mutate(cumulative = cumsum(individuals - individuals_to)) 

sel_cross_from$data_date <- as.Date(sel_cross_from$data_date, format = "%Y-%m-%d")

## crossings from
ggplot(sel_cross_from, aes(x = as.Date(data_date), y = individuals)) +
  geom_line(linewidth = 0.7, color = "tomato") +
  theme_ipsum() +
  #geom_line(sel_cases_frag, aes(x = Week, y = New_cases), linewidth = 0.7, color = "darkviolet") +
  scale_x_date(labels = date_format("%Y-%m-%d"), breaks = "2 week")+
  scale_y_continuous(labels = scales::comma)+
  # Line when War begins
  geom_vline(xintercept = as.Date(war_begin),
             linetype = 2,
             color = "darkgreen",
             linewidth=0.5)

## cumulative crossing
ggplot(sel_cross_from, aes(x = as.Date(data_date), y = cumulative)) +
  geom_line(linewidth = 0.7, color = "tomato") +
  theme_ipsum() +
  #geom_line(sel_cases_frag, aes(x = Week, y = New_cases), linewidth = 0.7, color = "darkviolet") +
  scale_x_date(labels = date_format("%Y-%m-%d"), breaks = "2 week")+
  scale_y_continuous(labels = scales::comma)+
  # Line when War begins
  geom_vline(xintercept = as.Date(war_begin),
             linetype = 2,
             color = "darkgreen",
             linewidth=0.5)  
# 2022-04-23 - 2,899,713 refugees in Poland 
# src: https://data2.unhcr.org/en/documents/details/92257
#geom_vline(xintercept = as.Date("2022-04-23"),
#linetype = 2,
#color = "darkgreen",
#linewidth=0.5)

cross_cases <- left_join(sel_cases_frag , sel_cross_from, by="data_date")

c = 10
ggplot(cross_cases, aes(x = Week)) +
  
  stat_summary(
    aes(y = cumulative / c),
    geom = "line", 
    linewidth = 0.7,
    color = "tomato") +
  # stat_summary(
  #   aes(y = cumulative * 0.018 ),
  #   geom = "line", 
  #   linewidth = 0.7,
  #   color = "darkred") +
  # stat_summary(
  #   aes(y = cumulative * 0.018 ),
  #   linewidth = 0.7,
  #   color = "darkred") +
  
  stat_summary(fun = sum,
               aes(y = New_cases ),
               #geom = "line", 
               linewidth = 0.7,
               color = "darkviolet") +
  stat_summary(fun = sum,
               aes(y = New_cases),
               geom = "line", 
               linewidth = 0.7,
               color = "darkviolet") +
  theme_ipsum() +
  
  scale_x_date(labels = date_format("%Y-%m-%d"), breaks = "2 week")+
  scale_y_continuous(
    #trans=~./c,
    labels = scales::comma,
    name = "New Cases per week in Poland",
    sec.axis = sec_axis( trans=~.*c,
                         name="Refugee Cumulative Number",
                         labels = scales::comma,
    )
  )+
  # Line when War begins
  geom_vline(xintercept = as.Date(war_begin),
             linetype = 2,
             color = "darkgreen",
             linewidth=0.5) +
  theme(
    axis.title.y = element_text(color = "darkviolet"),
    axis.text.y = element_text(color = "darkviolet"),
    axis.title.y.right = element_text(color = "tomato"),
    axis.text.y.right = element_text(color = "tomato"),
    axis.text.x = element_text(size = 10, angle = 45, margin = margin(t = 20))
  )


ggplot(cross_cases, aes(x = data_date)) +
  

  stat_summary(fun = ,
               aes(y = individuals),
               geom = "line", 
               linewidth = 0.7,
               color = "tomato") +
  # stat_summary(fun = ,
  #              aes(y = individuals - individuals_to),
  #              geom = "line",
  #              linetype = "dashed",
  #              linewidth = 0.7,
  #              color = "tomato") +
  
  stat_summary(
    aes(y = New_cases ),
    geom = "line",
    linewidth = 0.7,
    color = "darkviolet") +
  stat_summary(
    aes(y = New_cases ),
    geom = "smooth",
    linewidth = 0.7,
    color = "darkviolet") +
  # stat_summary(fun = sum,
  #              aes(y = New_cases ),
  #              #geom = "line", 
  #              linewidth = 0.7,
  #              color = "darkviolet") +
  # stat_summary(fun = sum,
  #              aes(y = New_cases),
  #              geom = "line", 
  #              linewidth = 0.7,
  #              color = "darkviolet") +
  theme_ipsum() +
  
  scale_x_date(labels = date_format("%Y-%m-%d"), breaks = "2 week")+
  scale_y_continuous(
    #trans=~./c,
    labels = scales::comma,
    name = "New Cases in Poland",
    sec.axis = sec_axis( trans=~.,
                         name="Border Crossing Number",
                         labels = scales::comma,
    )
  )+
  # Line when War begins
  geom_vline(xintercept = as.Date(war_begin),
             linetype = 2,
             color = "darkgreen",
             linewidth=0.5) +
  theme(
    axis.title.y = element_text(color = "darkviolet"),
    axis.text.y = element_text(color = "darkviolet"),
    axis.title.y.right = element_text(color = "tomato"),
    axis.text.y.right = element_text(color = "tomato"),
    axis.text.x = element_text(size = 10, angle = 45, margin = margin(t = 20))
  )

##### TRENDS
cases_week <- cross_cases %>%
  filter(Week != "2021-12-26") %>%
  select(Week, New_cases) %>%
  group_by(Week) %>%
  mutate(cases_week = sum(New_cases)) %>%
  select(Week, cases_week) %>%
  distinct(Week, cases_week)

plot(cases_week)

# ## 1. trendsegmentR
library(trendsegmentR)

tsfit <- trendsegment(x = cases_week$cases_week)
tsfit

plot(cases_week$cases_week, type = "b", ylim = range(cases_week$cases_week, tsfit$est))
lines(tsfit$est, col=2, lwd=2)
abline(v=tsfit$cpt, col=3, lty=2, lwd=2)

## 2. Rbeast

library(Rbeast)


out=beast(cases_week$cases_week, season='none')
plot(out)
print(out)

# crossings

out=beast(cross_from_UA$individuals, season='none')
plot(out)
print(out)

## 3. changepoint

library(changepoint)

cpt2 <- cpt.mean(cases_week$cases_week, method = "AMOC")

cpt2 <- cpt.mean(cases_week$cases_week, method = "PELT", penalty = "CROPS", pen.value = c(1,25))

plot(cpt2, diagnostic = TRUE)


