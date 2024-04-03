library(tidyverse)
library(ggthemes)
library(scales)


# waves in UA
# stages = waves
waves <- as.Date(c("2021-02-01","2021-09-01", "2022-01-01"))

#WHO data
# Source: https://covid19.who.int/WHO-COVID-19-global-data.csv
global_cases <- read_csv("raw_data/WHO-COVID-19-global-data.csv")

# World Bank Data
# Source: https://wdi.worldbank.org/table/2.1
populations <- list(Ukraine = 43800000,
                    Poland = 37700000,
                    Czechia = 10500000,
                    Germany = 83200000)

# Cases UA from Ministry of Healthcare



## Visual based on global data

sel_cases <- global_cases %>%
  # Countries
  #filter(Country %in% c("Ukraine", "Poland", "Czechia", "Germany")) %>%
  filter(Country %in% c("Ukraine")) %>%
  #Dates
  filter(Date_reported <= as.Date("2022-06-30")) %>%
  #New cases per 100,000
  #mutate(cases_100 = (New_cases * 100000) / as.numeric(populations[Country])) %>%
  #weeks
  mutate(Week = as.Date(cut(Date_reported, breaks = "month", start.on.monday = FALSE)))


ggplot(sel_cases, aes(x = Week, y = New_cases)) +
  
  # Line where waves begin-end
  geom_vline(xintercept = as.Date(waves[1]),
             linetype = 2,
             color = "darkgreen",
             linewidth=0.5) +
  geom_vline(xintercept = as.Date(waves[2]),
             linetype = 2,
             color = "darkgreen",
             linewidth=0.5)+
  geom_vline(xintercept = as.Date(waves[3]),
             linetype = 2,
             color = "darkgreen",
             linewidth=0.5)+
  
  annotate("rect", fill = "darkgreen", alpha = 0.5, 
           xmin = as.Date("2020-01-01"), xmax = as.Date(waves[1]),
           ymin = 0, ymax = Inf) +
  annotate("rect", fill = "yellow", alpha = 0.5, 
           xmin = as.Date(waves[1]), xmax = as.Date(waves[2]),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "orange", alpha = 0.5, 
           xmin = as.Date(waves[2]), xmax = as.Date(waves[3]),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "tomato", alpha = 0.5, 
           xmin = as.Date(waves[3]), xmax = as.Date("2022-06-30"),
           ymin = -Inf, ymax = Inf) +
  
  stat_summary(fun = sum, geom = "line", linewidth = 3, color = "darkslategrey") +
  stat_summary(fun = sum,
               aes(y = New_cases ),
               linewidth = 20,
               color = "darkslategray4") +
  
  scale_x_date(name= "", # "Date",#"Months",
               expand = c(0,0),
               limits = as.Date(c("2020-03", "2022-06"), format = "%Y-%m"),
               labels = date_format("%Y-%b"),
               breaks = "3 month")+
  scale_y_continuous(
    expand = c(0,0),
    limits = c(0, 800000),
    labels = scales::comma,
    name = "New Cases in Ukraine per month",
    )+

  
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) + 
  theme(
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 16, margin = margin(t = 20))
  )


count_panlin <- df_ua_167 %>% count(pangolin_lineage)
count_panlin_1081 <- jerya_ua_all %>% count(pangolin_lineage)