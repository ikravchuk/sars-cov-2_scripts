library(tidyverse)
library("ggsci")

nb.cols <- 18
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)


cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7", 
          "#293352", )

# Load 114
df_ua_114 <- read_csv("data/ua_114_meta_mut.csv")
df_ua_war1 <- df_ua_114[df_ua_114$date > as.Date("2022-01-01"),]

# Load 167
df_ua_167 <- read_csv("data/ua_167_meta.csv")
df_ua_war2 <- df_ua_167[df_ua_167$date > as.Date("2022-01-01"),]

# Load all UA seq in 01-06.2023
df_war_ua <- read_csv("data/ua_war_308_meta_mut.csv")

# Load 4 wave = War period
df_ua4 <- read_csv("data/ua4_67_meta_mut.csv")

# Load Polish data
pl_df <- read_csv("data/pl_meta_mut.csv")

## Plot variant over time
## Poland
pl_var <- pl_df %>%
  filter(as.Date(date) <= as.Date("2022-06-01"))%>%
  select(date, pangolin_lineage) %>%
  mutate(pan = substr(pangolin_lineage, start = 1, stop = 4)) %>%
  group_by(date, pan) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count)) %>%
  mutate(pan = replace(pan, percentage <= 0.05, "Other"))%>%
  mutate(pan = replace(pan, pan == "Unas", "Other"))
  
  
ggplot(pl_var, aes(x = date, y = percentage, fill = pan )) +
  theme_minimal() +
  geom_bar(position="stack", stat="identity", width = 1) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(labels = date_format("%m.%Y")) +
  
  # Line when War begins
  geom_vline(xintercept = as.Date(war_begin),
             linetype = 1,
             color = "black",
             linewidth=0.5) +
  
  theme(legend.position="bottom")+
  theme(legend.title=element_blank())+
  scale_fill_brewer(palette = "Dark2")

## Ukraine
ua_var <- df_war_ua %>%
  filter(as.Date(date) <= as.Date("2022-06-01") &
           as.Date(date) >= as.Date("2022-02-01"))%>%
  select(date, pangolin_lineage) %>%
  mutate(pan = substr(pangolin_lineage, start = 1, stop = 4)) %>%
  group_by(date, pan) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count))%>%
  mutate(pan = replace(pan, percentage <= 0.01, "Other"))%>%
  mutate(pan = replace(pan, pan == "Unas", "Other"))


ggplot(ua_var, aes(x = date, y = percentage, fill = pan )) +
  theme_minimal() +
  geom_bar(position="stack", stat="identity", width = 1) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(labels = date_format("%m.%Y")) +
  
  # Line when War begins
  geom_vline(xintercept = as.Date(war_begin),
             linetype = 1,
             color = "black",
             linewidth=0.5) +
  
  theme(legend.position="bottom")+
  theme(legend.title=element_blank()) +
  scale_fill_brewer(palette = "Dark2")


#Poland and Ukraine overlap
um <- unlist(strsplit(df_war_ua$nucmutations,","))
um_df <- data.frame(n_mut = um)
pm <- unlist(strsplit(pl_df$nucmutations,","))
pm_df <- data.frame(n_mut = pm)

int_up <- data.frame(n_mut = intersect(um, pm))
diff_up <- data.frame(n_mut = setdiff(um, pm))
diff_pu <- data.frame(n_mut = setdiff(pm, um))

func_pos <- function(x){
  regexp <- "[[:digit:]]+"
  x_cut <- str_extract(x, regexp)
  return(x_cut)
}

func_first <- function(x, df){
  df_y <- df[str_detect(df$nucmutations, x), ]
  return(min(df_y$date))
}

func_count <- function(x, df){
  df_y <- df[str_detect(df$nucmutations, x), ]
  return(count(df_y))
}

result_df <- int_up %>%
  mutate(pos = as.numeric(func_pos(n_mut))) %>%
  arrange(pos) %>%
  rowwise() %>%
  mutate(first_ua = func_first(n_mut, df_ua4),
         first_pl = func_first(n_mut, pl_df)) %>%
  mutate(count_ua = func_count(n_mut, df_ua4),
         count_pl = func_count(n_mut, pl_df)) %>%
  mutate(p_ua = count_ua/nrow(df_ua4),
         p_pl = count_pl/nrow(pl_df))

result_diff_up_df <- diff_up %>%
  mutate(pos = as.numeric(func_pos(n_mut))) %>%
  arrange(pos) %>%
  rowwise() %>%
  mutate(first_ua = func_first(n_mut, df_ua4)) %>%
  mutate(count_ua = func_count(n_mut, df_ua4)) %>%
  mutate(p_ua = count_ua/nrow(df_ua4))

#result_diff_pu_df <- diff_pu %>%
# mutate(pos = as.numeric(func_pos(n_mut))) %>%
#arrange(pos) %>%
#rowwise() %>%
#mutate(first_pl = func_first(n_mut, pl_df)) %>%
#mutate(count_pl = func_count(n_mut, pl_df)) %>%
#mutate(p_pl = count_pl/nrow(pl_df))

result_df_selected <- result_df %>%
  filter(p_ua < 0.1) %>%
  filter(p_pl < 0.1) %>%
  filter(count_ua > 1)
