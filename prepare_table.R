###PART1
library(stringr)
library(dplyr)
setwd("~/dev/Denise")
allsbj <- read.csv("allSubj_output.csv")
allsbj <- na.omit(allsbj)


#  extract the id_num column from the obj_id column
allsbj$id_num <- as.integer(str_extract(allsbj$sbj_id, "(?<=VP)\\d+"))
num_ofsubj_et = unique(allsbj$id_num)
length(num_ofsubj_et) #71

# Create the keys table as provided in Denise's excel file
group_0 <- c(37, 39, 32, 22, 48, 47, 15, 29, 40, 38, 38, 28, 35, 1, 49, 36, 14, 42, 34, 45, 41, 46, 11, 9, 6, 16, 23, 44, 2, 12, 13, 4, 21, 33, 20, 52, 25, 28, 27, 26, 30, 55, 5, 51, 10, 56, 59, 57, 58, 69, 53, 60, 63, 66, 67, 68, 70, 75, 61, 76, 74, 72, 80, 78, 79, 77)
group_1 <- c(24, 43, 18, 17, 19, 3, 7, 64, 65, 71, 73, 76) #here 12 subjects

# Combine the two vectors into a single vector and create a vector for 
# the group labels
ids <- c(group_0, group_1)
groups <- c(rep(0, length(group_0)), rep(1, length(group_1)))
# Combine the two vectors into a data frame
df <- data.frame(id = ids, group = groups)
#how many subj
num_ofsubj_excel = unique(df$id)
length(num_ofsubj_excel) #75
##########################################################################
##########################################################################
##########################################################################

# join allsbj and df data frames based on the id_num and id columns
merged_df <- merge(allsbj, df, by.x = "id_num", by.y = "id", all.x = TRUE)

# Rename the group column to "group"
colnames(merged_df)[ncol(merged_df)] <- "group"


df = merged_df

#sanity check
unique_subjects <- unique(df$id_num)
length(unique_subjects) #71
unique_subjects_group1 <- unique(subset(merged_df, group == 1)$id_num)
unique_subjects_group1 #missing -> 73, 65, 7 (had abort)
#######################################
#If type is 1 and sacc_dir is equal to stim_dir, then error is set to 0.
#If type is 1 and sacc_dir is not equal to stim_dir, then error is set to 1.
#If type is 0 and sacc_dir is equal to stim_dir, then error is set to 1.
#If type is 0 and sacc_dir is not equal to stim_dir, then error is set to 0.

add_error_column <- function(df) {
  df$error <- ifelse(df$type == 1 & df$sacc_dir == df$stim_dir, 0, 
                     ifelse(df$type == 1 & df$sacc_dir != df$stim_dir, 1, 
                            ifelse(df$type == 0 & df$sacc_dir == df$stim_dir, 1, 0)))
  return(df)
}


df <- add_error_column(df)


write.csv(df, file = "data4analysis.csv", row.names = FALSE)
##########################################
###########     ERROR RATE.    ###########
##########################################
total_errors <- sum(df$error) #16260 -> 23.47%
error_rate_pro <- mean(df$error[df$type == 1])  #16.09%
error_rate_anti <- mean(df$error[df$type == 0]) #30.85%

# now, for checking group differences
# Filter out rows with NA in the 'group' column
unique_subjects <- unique(subset(df)$id_num)
unique_subjects_withoutgroup = unique(df$id_num[is.na(df$group)])

df_filtered <- df[!is.na(df$group),]
unique_subjects <- unique(subset(df_filtered)$id_num)

# Calculate the error rate for  group = 1
error_rate_anti_group1 <- mean(df_filtered$error[df_filtered$type == 0 & df_filtered$group == 1])
#30.27%
error_rate_pro_group1 <- mean(df_filtered$error[df_filtered$type == 1 & df_filtered$group == 1])
# 12.6%
#and control group
error_rate_pro_group0 <- mean(df_filtered$error[df_filtered$type == 1 & df_filtered$group == 0])
#16.6%
error_rate_anti_group0 <- mean(df_filtered$error[df_filtered$type == 0 & df_filtered$group == 0])
#31.4%

#########################################################################
#Calculate RT for group =1
mean_sacc_time_pro_group1 <- mean(df_filtered$sacc_time[df_filtered$type == 1 & df_filtered$group == 1])
#409.1835. 
mean_sacc_time_anti_group1 <- mean(df_filtered$sacc_time[df_filtered$type == 0 & df_filtered$group == 1])
#412.31

#Calculate RT for group = 0
mean_sacc_time_pro_group0 <- mean(df_filtered$sacc_time[df_filtered$type == 1 & df_filtered$group == 0])
#403.20 
mean_sacc_time_anti_group0 <- mean(df_filtered$sacc_time[df_filtered$type == 0 & df_filtered$group == 0])
#404.24


