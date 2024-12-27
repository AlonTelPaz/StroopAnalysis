# R course for beginners
# Week 7
# assignment by Alon Tel-Paz, ID 208201095

#### load data and build raw data ----
library ("dplyr")
sub_data <- dir ("data")
df <- data.frame ()
for (file in sub_data) {
  temp_data <- read.csv (file.path ("data", file))
  df <- rbind (df, temp_data)
}
print (df)
data_raw <- df
write.csv (data_raw, "./data_raw/data_raw.csv")

df <- data_raw |>
  mutate (
    task = ifelse (grepl ("ink_naming", condition), "ink_naming", "word_reading"),
    congruency = ifelse(grepl("incong", condition), "incongruent", "congruent"),
    accuracy  = ifelse (correct_response == participant_response, "1", "0")
  )
# fillter uninfotment collum
df <- df|>
  mutate(
          subject = as.factor (subject),
          block = as.numeric (block),
          trial = as.numeric (trial),
          task = as.factor (task),
          congruency = as.factor (congruency),
          accuracy = as.numeric (accuracy),
          rt = as.numeric (rt)) |>
select (
        subject,
        block,
        trial,
        task,
        congruency,
        accuracy,
        rt)
contrasts (df$task) # ink_naming = 0, word_reading = 0
contrasts (df$congruency) #congruent = 0, incongruent = 1
#saving raw data
save (df, file =  "./data_raw/raw_data.rdata")


#### creating filltered data ----
n_distinct (df$subject) # there are 30 subjects

# how much trails for each subject
df |>
  group_by (subject) |>
  summarise (n()) # 400 trails for each one
# remove the NA values
df <- 
  na.omit (df)
#remove rt < 300 ms or > 3000 ms
logical_check_bigger_than_3000 <- df$rt <= 3000
logical_check_smaller_than_300 <- df$rt >= 300

df <- df |>
  filter(rt <- logical_check_bigger_than_3000 | logical_check_smaller_than_300) # i don't know why but they is one row the is not filtered (12) rt = 4602
percentage <- df |>
  group_by (subject) |>
  summarise (perc = (n ()/400)*100)

print (percentage)

mean_rt <- mean(percentage$perc)
sd_rt <- sd(percentage$perc)

summarise_pec <- data.frame(mean_rt = mean_rt, sd_rt = sd_rt)
print(summarise_pec)

#save data
save (df, file = "./filtered_data.rdata")