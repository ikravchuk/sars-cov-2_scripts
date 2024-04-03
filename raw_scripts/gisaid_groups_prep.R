library(tidyverse)

#161
metadata_ua_mir <- read_tsv("raw_data/gisaid_auspice_input_hcov-19_2023_07_05_10_mironenko_00/1688553782280.metadata.tsv")

#1081
jerya_ua_all <- read_tsv("raw_data/ua-all.metadata.tsv")
#drop last column
jerya_ua_all <- jerya_ua_all[1:(length(jerya_ua_all)-1)]

#167
mir_167 <- read_csv("raw_data/167_mir.csv")

#114
metadata <- read_tsv("raw_data/1673988258516.metadata.tsv")

#167-114
ids_167<- mir_167["Accession ID"]
colnames(ids_167) <- c("gisaid_epi_isl")

ids_167_114 <- setdiff(ids_167, metadata["gisaid_epi_isl"])

#161-114
ids_161_114 <- setdiff(metadata_ua_mir["gisaid_epi_isl"],
                       metadata["gisaid_epi_isl"])
                       
#167-161
ids_167_161 <- setdiff(ids_167, metadata_ua_mir["gisaid_epi_isl"])
#setdiff(metadata_ua_mir["gisaid_epi_isl"], ids_167)

meta_plus<- read_tsv("raw_data/diff/1688647984356.metadata.tsv")
#setdiff(ids_167_161, meta_plus["gisaid_epi_isl"])

#setdiff(metadata_ua_mir["gisaid_epi_isl"], meta_plus["gisaid_epi_isl"])

#setdiff(ids_167, meta_plus["gisaid_epi_isl"])

meta_161plus8 <- rbind(metadata_ua_mir, meta_plus)
#setdiff( meta_161plus8["gisaid_epi_isl"], ids_167)


# meta_all
meta_167 <- filter(meta_161plus8,
                   gisaid_epi_isl != "EPI_ISL_7398746" &
                     gisaid_epi_isl !=  "EPI_ISL_7987086")

#setdiff(jerya_ua_all["gisaid_epi_isl"], meta_167["gisaid_epi_isl"])
#setdiff(meta_167["gisaid_epi_isl"], jerya_ua_all["gisaid_epi_isl"])

#setdiff(jerya_ua_all["gisaid_epi_isl"], metadata["gisaid_epi_isl"])
#setdiff(metadata["gisaid_epi_isl"], jerya_ua_all["gisaid_epi_isl"])


war_meta <- read_tsv("raw_data/gisaid_auspice_input_hcov-19_2023_07_07_10_ua_war/1688725791973.metadata.tsv")

setdiff(war_meta["gisaid_epi_isl"], metadata["gisaid_epi_isl"])

