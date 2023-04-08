### part 2

library(dplyr)
library(tidyr)
library(readxl)
library(readr)
setwd("~/dev/Denise")


df <- read_csv("data4analysis.csv")
anxiety <- read_excel("anxiety.xlsx")
depression <- read_excel("depression.xlsx")
df_psychometrics <- merge(anxiety, depression, by = c("ID", "PersCode", "Group"))


## preparing eye tracker data ('df')

result <- df %>%
  group_by(id_num, type, group) %>%
  summarize(
    mean_peak_vel = mean(peak_vel, na.rm = TRUE),
    mean_sacc_time = mean(sacc_time, na.rm = TRUE),
    cum_errors = sum(error, na.rm = TRUE)
    ) %>%
  pivot_wider(
    id_cols = c(id_num, group),
    names_from = type,
    values_from = c(mean_peak_vel, mean_sacc_time, cum_errors),
    names_sep = "_"
  ) %>%
  rename(
    mean_peak_vel_pro = mean_peak_vel_1,
    mean_peak_vel_anti = mean_peak_vel_0,
    mean_sacc_time_pro = mean_sacc_time_1,
    mean_sacc_time_anti = mean_sacc_time_0,
    cum_errors_pro = cum_errors_1,
    cum_errors_anti = cum_errors_0
  )




#################
# join df and psychometrics data frames based on the id_num and id columns
merged_df <- merge(result, df_psychometrics, by.x = "id_num", by.y = "ID", all.x = TRUE)

# Remove rows with NA values
merged_df <- na.omit(merged_df)

# Save as CSV file
write.csv(merged_df, "table_ready_with_psycho.csv", row.names = FALSE)


