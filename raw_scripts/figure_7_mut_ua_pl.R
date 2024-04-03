library(tidyverse)
library(ggthemes)

# War begins
war_begin <- "2022-02-24"

# Load 4 wave = War period
df_ua4 <- read_csv("data/ua4_67_meta_mut.csv")

# Load Polish data
pl_df <- read_csv("data/pl_meta_mut.csv")

# Mutations that will be selected for Figure
muts <- c('C8991T',
          'C10507T',
          'C25317T',
          'C7772T',
          'C5274T',
          'C9430T',
          'C21595T',
          'C26571T',
          'C5284T')

# func for selection from UA and PL data
func_sel <- function(muts){
  sel <- NULL
  for (m in muts) {
    
    ua_sel <- filter(df_ua4, grepl(m, nucmutations))
    pl_sel <- filter(pl_df, grepl(m, nucmutations))
    s <- rbind(ua_sel, pl_sel)
    s["m"] <- m
    sel <- rbind(sel, s)
  }
  return(sel)
}

# Selection
df_9 <- as.data.frame(func_sel(muts))

# Plotting
ggplot(df_9, aes(x = date, fill = country )) +
  #theme_minimal() +
  geom_dotplot(dotsize = 1) +
  scale_y_continuous(NULL, breaks = NULL) +
  scale_x_date(labels = date_format("%m.%Y")) +
  
  # Line when War begins
  geom_vline(xintercept = as.Date(war_begin),
             linetype = 2,
             color = "tomato",
             linewidth=0.5) +
  
  scale_fill_manual(values = c("#00AFBB", "#E69138")) +
  facet_wrap(~m) +
  theme(legend.position="bottom")+
  theme(legend.title=element_blank())