library(tidyverse)
library(scales)

### ------ 1. UKRAINE DATA ------

# 114 - sequenced
# 167 - our samples = 114 + 53 sequenced by french center
# 1081 - bioinformatics analysis = 155 (out of 167) + 926 other UA sequences

## 1.1. Ukraine 114 meta + mut

# metadata from GISAID, 114 our sequences
# 114 x 28
metadata <- read_tsv("raw_data/1673988258516.metadata.tsv")

# results of mutational analysis of sequences 
# tool: https://gisaid.org/database-features/covsurver-mutations-app/
# ref: Khare, S., et al (2021) GISAID’s Role in Pandemic Response.
# China CDC Weekly, 3(49): 1049-1051.
# doi: 10.46234/ccdcw2021.255 PMCID: 8668406
# 114 x 33
mutations <- read_tsv("raw_data/covsurver_result9582_perquery.tsv")

# combine into one dataframe
# 114 x 60
df_ua <- merge(x = metadata, y = mutations, by.x = "strain", by.y = "Query")

# waves in UA
# stages = waves
waves <- as.Date(c("2021-01-01","2021-04-01", "2022-01-01"))

df_ua1 <- df_ua[df_ua$date < waves[1],]
df_ua2 <- df_ua[waves[1] < df_ua$date & df_ua$date < waves[2],]
df_ua3 <- df_ua[waves[2] < df_ua$date & df_ua$date < waves[3],]
df_ua4 <- df_ua[df_ua$date > waves[3],]

# SAVE to files
# UA full 114
write_csv(df_ua, "data/ua_114_meta_mut.csv")

# UA all waves
write_csv(df_ua1, "data/ua1_18_meta_mut.csv") # 18 samples
write_csv(df_ua2, "data/ua2_7_meta_mut.csv")  # 7 samples
write_csv(df_ua3, "data/ua3_22_meta_mut.csv") # 22 samples
write_csv(df_ua4, "data/ua4_67_meta_mut.csv") # 67 samples


## 1.2. Ukraine 167 meta

source("gisaid_groups_prep.R")

write_csv(meta_167, "data/ua_167_meta.csv")

meta_167 %>%
  group_by(GISAID_clade) %>%
  summarise(num_clades = n())%>%
  print()

pang_167 <- meta_167 %>%
  #group_by(GISAID_clade) %>%
  group_by(GISAID_clade, pangolin_lineage) %>%
  
  summarise(num_clades = n())

## 1.3. Ukraine 1081 meta

jerya_ua_all 
write_csv(jerya_ua_all, "data/ua_1081_meta.csv")

j <- jerya_ua_all %>%
  group_by(GISAID_clade, pangolin_lineage) %>%
  summarise(num_clades = n())

## 1.4 All War sequences from UA January-August 2022 (308)

war_meta <- read_tsv("raw_data/gisaid_auspice_input_hcov-19_2023_07_07_10_war_308/1688727117183.metadata.tsv")
war_mut <- read_tsv("raw_data/gisaid_auspice_input_hcov-19_2023_07_07_10_war_308/covsurver_result7860_perquery.tsv")

df_war_ua <- merge(x = war_meta, y = war_mut, by.x = "strain", by.y = "Query")

write_csv(df_war_ua, "data/ua_war_308_meta_mut.csv")

## 1.5 All Ukrainian sequences 
## from GISAID update 17.07.2023

all_ua_meta <- read_tsv("raw_data/all_ua/1689581281062.metadata.tsv")

## 1.6 All UA sequences where samples collected up to 30.06.2022 
## from GISAID update 17.07.2023

all_ua_06_2022_meta <- read_tsv("raw_data/all_ua_to_30-06-2022/1689581777332.metadata.tsv")



### ------ 2. POLAND DATA ------ 

# 18747 sequences from Poland (February-August 2022)
# meta - GISAID
# mut - Covsurver from GISAID

pl_02_1_metadata <- read_tsv("raw_data/poland_2022_02_1-5/1676236582660.metadata.tsv")
pl_02_1_mut <- read_tsv("raw_data/poland_2022_02_1-5/poland_02_2022_1.tsv")
df_pl_02_1 <- merge(x = pl_02_1_metadata, y = pl_02_1_mut, by.x = "strain", by.y = "Query")

pl_02_2_metadata <- read_tsv("raw_data/poland_2022_02_6-12/1676236808147.metadata.tsv")
pl_02_2_mut <- read_tsv("raw_data/poland_2022_02_6-12/poland_02_2022_2.tsv")
df_pl_02_2 <- merge(x = pl_02_2_metadata, y = pl_02_2_mut, by.x = "strain", by.y = "Query")

pl_02_3_metadata <- read_tsv("raw_data/poland_2022_02_13-28/1676237024523.metadata.tsv")
pl_02_3_mut <- read_tsv("raw_data/poland_2022_02_13-28/poland_02_2022_3.tsv")
df_pl_02_3 <- merge(x = pl_02_3_metadata, y = pl_02_3_mut, by.x = "strain", by.y = "Query")

poland_03_metadata <- read_tsv("raw_data/poland_2022_03/1676237205898.metadata.tsv")
poland_03_mutations <- read_tsv("raw_data/poland_2022_03/poland_3.tsv")

df_pl_03 <- merge(x = poland_03_metadata, y = poland_03_mutations, by.x = "strain", by.y = "Query")

poland_04_08_metadata <- read_tsv("raw_data/poland_2022_04_01-2022_08_31/1676237440244.metadata.tsv")
poland_04_08_mutations <- read_tsv("raw_data/poland_2022_04_01-2022_08_31/poland_4-8.tsv")

df_pl_04_08 <- merge(x = poland_04_08_metadata, y = poland_04_08_mutations, by.x = "strain", by.y = "Query")

pl_df <- rbind(df_pl_02_1, df_pl_02_2, df_pl_02_3, df_pl_03, df_pl_04_08)


write_csv(pl_df, "data/pl_meta_mut.csv")

