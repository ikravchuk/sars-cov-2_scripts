library(tidyverse)
library(gridExtra)
library(ggthemes)

# Load 167 meta
df_ua <- read_csv("data/ua_167_meta.csv")

# Load 114 meta + mut
df_ua <- read_csv("data/ua_114_meta_mut.csv")

# Load wave
df_ua4 <- read_csv("data/ua4_67_meta_mut.csv")

# count AA mutations
func_count_a <- function(x, df){
  df_y <- df[str_detect(df$ExistingMutList, x), ]
  return(count(df_y))
}

um_a <- unlist(strsplit(df_ua4$ExistingMutList,","))

spike <- data.frame(a_mut = um_a) %>%
  filter(grepl("Spike", a_mut))%>%
  distinct(a_mut, .keep_all=TRUE) %>%
  mutate(pos = as.numeric(func_pos(a_mut)))%>%
  arrange(pos) %>%
  rowwise()%>%
  mutate(n1 = as.numeric(func_count_a(a_mut, df_ua1)))%>%
  mutate(n2 = as.numeric(func_count_a(a_mut, df_ua2)))%>%
  mutate(n3 = as.numeric(func_count_a(a_mut, df_ua3)))%>%
  mutate(n4 = as.numeric(func_count_a(a_mut, df_ua4)))%>%
  arrange(pos)



theme_set(theme_bw())


s1 <- ggplot(spike, aes(x = a_mut, y = n1)) +
  geom_bar(stat = "identity", fill="#56B4E9") +
  coord_flip() +
  theme(axis.text.y = element_text(size = 5)) +
  ylim(0, 30) +
  ggtitle("First wave")

s2 <- ggplot(spike, aes(x = a_mut, y = n2)) +
  geom_bar(stat = "identity", fill ="#E69F00") +
  coord_flip() +
  ylim(0, 70) +
  ggtitle("Second wave")

s3 <- ggplot(spike, aes(x = a_mut, y = n3)) +
  geom_bar(stat = "identity", fill="#0072B2") +
  coord_flip() +
  ylim(0, 70) +
  ggtitle("Third wave")

s4 <- ggplot(spike, aes(x = a_mut, y = n4)) +
  geom_bar(stat = "identity", fill="#D55E00") +
  coord_flip() +
  ylim(0, 70) +
  ggtitle("Fourth wave")


grid.arrange(s1+ 
               theme(axis.title.x = element_blank(),
                     axis.title.y = element_blank()),
             
             s2 + 
               theme(axis.title.x = element_blank(),
                     axis.text.y = element_blank(),
                     
                     axis.title.y = element_blank() ), 
             s3 + 
               theme(axis.title.x = element_blank(),
                     axis.text.y = element_blank(),
                     
                     axis.title.y = element_blank() ), 
             s4 +
               theme(axis.title.x = element_blank(),
                     axis.text.y = element_blank(),
                     
                     axis.title.y = element_blank() ), 
             ncol = 4
)