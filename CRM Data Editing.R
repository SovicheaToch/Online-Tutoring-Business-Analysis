library(caret)
library(googlesheets4)
library(dplyr)
library(lubridate)
library(purrr)
library(tidyr)
library(stringr)
library(fastDummies)

# Create example data frame
df <- data.frame(
  Category = c("A", "B", "A", "C", "B"),
  Value = c(1, 2, 3, 4, 5)
)

# One-hot encode the 'Category' column
dummy <- dummyVars(" ~ .", data = df)
df_encoded <- data.frame(predict(dummy, newdata = df))

# Print the result
print(df_encoded)

# Importing data
url <- "https://docs.google.com/spreadsheets/d/1kH7ac4qJZmRR0zgMTdpaD2M7und-Qn8xwMvpVu-COyM/edit?gid=1236849028#gid=1236849028"
sheet <- gs4_get(url)

# Read the cr sheet
data1 <- range_read(sheet, sheet = "CR Analysis" )

# Read the stage history sheet
data2 <- range_read(sheet, sheet = "Stage History R")

# State to timezone mapping
state_timezones <- c(
  AL = "America/Chicago", AK = "America/Anchorage", AZ = "America/Phoenix",
  AR = "America/Chicago", CA = "America/Los_Angeles", CO = "America/Denver",
  CT = "America/New_York", DE = "America/New_York", FL = "America/New_York",
  GA = "America/New_York", HI = "Pacific/Honolulu", ID = "America/Boise",
  IL = "America/Chicago", IN = "America/New_York", IA = "America/Chicago",
  KS = "America/Chicago", KY = "America/New_York", LA = "America/Chicago",
  ME = "America/New_York", MD = "America/New_York", MA = "America/New_York",
  MI = "America/New_York", MN = "America/Chicago", MS = "America/Chicago",
  MO = "America/Chicago", MT = "America/Denver", NE = "America/Chicago",
  NV = "America/Los_Angeles", NH = "America/New_York", NJ = "America/New_York",
  NM = "America/Denver", NY = "America/New_York", NC = "America/New_York",
  ND = "America/Chicago", OH = "America/New_York", OK = "America/Chicago",
  OR = "America/Los_Angeles", PA = "America/New_York", RI = "America/New_York",
  SC = "America/New_York", SD = "America/Chicago", TN = "America/Chicago",
  TX = "America/Chicago", UT = "America/Denver", VT = "America/New_York",
  VA = "America/New_York", WA = "America/Los_Angeles", WV = "America/New_York",
  WI = "America/Chicago", WY = "America/Denver"
)

# Convert to POSIXct datetime
data1 <- data1 %>%
  mutate(
    `Date of First Email Sent` = ymd_hms(`Date of First Email Sent`, quiet = TRUE),
    `Date of First Received Email` = ymd_hms(`Date of First Received Email`, quiet = TRUE)
  )

# Custom function to format time differences
format_time_diff <- function(start, end) {
  if (is.na(start) | is.na(end)) {
    return(NA)
  }
  
  diff <- as.numeric(difftime(end, start, units = "secs"))
  
  days <- floor(diff / (24 * 3600))
  diff <- diff %% (24 * 3600)
  
  hours <- floor(diff / 3600)
  diff <- diff %% 3600
  
  minutes <- floor(diff / 60)
  seconds <- diff %% 60
  
  result <- paste0(days, "d ", hours, "h ", minutes, "m ", round(seconds), "s")
  if (result == "0d 0h 0m 0s") {
    return(NA)
  }
  return(result)
}

# Calculate time difference
data1 <- data1 %>%
  rowwise() %>%
  mutate(
    time_to_reply = format_time_diff(`Date of First Email Sent`, `Date of First Received Email`)
  ) %>%
  ungroup()

data1 <- data1 %>%
  mutate(time_to_reply = ifelse(`Stages (Grouped)` == "New Contacts, Sent Mail Merges", NA, time_to_reply))

data1 <- data1 %>%
  filter(`Current Stage` != "Backlog")

# Combine old and new stages from data2
data2_long <- data2 %>%
  select(Name, `Old Stage`, `New Stage`) %>%
  pivot_longer(cols = c(`Old Stage`, `New Stage`), names_to = "Stage_Type", values_to = "Stage") %>%
  select(Name, Stage) %>%
  distinct()

# Add current stage from data1
combined_data <- data2_long %>%
  bind_rows(data1 %>% select(Name, Stage = `Current Stage`)) %>%
  distinct()

# Group by school and create comma-separated list of stages
all_stages <- combined_data %>%
  group_by(Name) %>%
  summarize(All_Stages = paste(unique(Stage), collapse = ", "))

# Merge the result with data1
data1 <- data1 %>%
  left_join(all_stages, by = "Name")

# Create new column All_Stages(Grouped) with comma-separated list
data1 <- data1 %>%
  mutate(`All_Stages(Grouped)` = case_when(
    grepl("Imported", All_Stages) ~ "Imported",
    TRUE ~ ""
  )) %>%
  rowwise() %>%
  mutate(`All_Stages(Grouped)` = paste(
    unique(c(
      if (grepl("Imported", All_Stages)) "Imported" else NULL,
      if (grepl("Sent Intro MM|Sent Unresponsive -- Intro|Received Summer Drip", All_Stages)) "Sent Mail Merges" else NULL,
      if (grepl("Engaged|Scheduled phone call", All_Stages)) "Warm Leads" else NULL,
      if (grepl("Passed our info to school|Shared/will share our info with families", All_Stages)) "Outreached our Services" else NULL,
      if (grepl("Connected us with families", All_Stages)) "Connected us with families" else NULL,
      if (grepl("Will call \\(unresponsive 2x\\)|Called but no answer|Contact later", All_Stages)) "Became Unresponsive" else NULL,
      if (grepl("Ghosted", All_Stages)) "Ghosted" else NULL,
      if (grepl("Passed b/c own tutoring \\+ EF", All_Stages)) "Passed b/c own tutoring, EF / non-virtual" else NULL,
      if (grepl("Blocked|Unsubscribe", All_Stages)) "Blocked/Unsubscribe" else NULL
    )),
    collapse = ", "
  ))

# Create new column Role (Grouped) with comma-separated list
data1 <- data1 %>%
  rowwise() %>%
  mutate(`Role(Grouped)` = paste(
    unique(c(
      if (grepl("counselor|advisor", Role, ignore.case = TRUE)) "Counselor & Advisor" else NULL,
      if (grepl("head|president", Role, ignore.case = TRUE)) "Head of School" else NULL,
      if (grepl("principal", Role, ignore.case = TRUE)) "Principal" else NULL,
      if (grepl("director", Role, ignore.case = TRUE)) "Director" else NULL,
      if (grepl("dean", Role, ignore.case = TRUE)) "Dean" else NULL,
      if (grepl("support|specialist", Role, ignore.case = TRUE) && !grepl("director", Role, ignore.case = TRUE)) "Learning Specialist & Student Support" else NULL
    )),
    collapse = ", "
  )) %>%
  mutate(`Role(Grouped)` = ifelse(`Role(Grouped)` == "", "Others", `Role(Grouped)`)) %>%
  ungroup()

# Create new column Tuition (Grouped)
data1 <- data1 %>%
  mutate(`Tuition (Grouped)` = case_when(
    grepl("10 - 15K|5 - 10K", Tuition, ignore.case = TRUE) ~ "<15K",
    grepl("40 - 45K|45 - 50K|50 - 55K|55 - 60K|55k\\+|60 - 65K", Tuition, ignore.case = TRUE) ~ ">40K",
    TRUE ~ Tuition
  ))


# Create new column Sent Email Count(Grouped)
data1 <- data1 %>%
  mutate(`Sent Email Count(Grouped)` = case_when(
    `Sent Email Count` >= 1 & `Sent Email Count` <= 5 ~ "1 - 5",
    `Sent Email Count` >= 6 & `Sent Email Count` <= 10 ~ "6 - 10",
    `Sent Email Count` >= 11 & `Sent Email Count` <= 15 ~ "11 - 15",
    `Sent Email Count` >= 16 & `Sent Email Count` <= 20 ~ "16 - 20",
    `Sent Email Count` > 20 ~ ">20",
    TRUE ~ as.character(`Sent Email Count`)
  ))

# Function to format the time difference
format_time_diff <- function(time_diff) {
  if (time_diff == 0) {
    return("")
  } else {
    days <- as.integer(time_diff %/% ddays(1))
    hours <- as.integer((time_diff %% ddays(1)) %/% dhours(1))
    minutes <- as.integer((time_diff %% dhours(1)) %/% dminutes(1))
    seconds <- as.integer((time_diff %% dminutes(1)) %/% dseconds(1))
    return(paste0(days, "d ", hours, "h ", minutes, "m ", seconds, "s"))
  }
}

# Update the time_to_reply column
data1 <- data1 %>%
  rowwise() %>%
  mutate(time_to_reply = ifelse(
    grepl("Imported, Sent Mail Merges, Passed b/c own tutoring, EF / non-virtual", `All_Stages(Grouped)`, ignore.case = TRUE),
    format_time_diff(as.numeric(difftime(`Date of First Received Email`, `Date of First Email Sent`, units = "secs"))),
    time_to_reply
  )) %>%
  ungroup()

# Function to convert formatted time difference to hours
convert_to_hours <- function(time_str) {
  if (is.na(time_str)) {
    return(NA)
  }
  time_components <- str_match(time_str, "(\\d+)d (\\d+)h (\\d+)m (\\d+)s")
  if (is.na(time_components[1, 1])) {
    return(NA)
  }
  days <- as.numeric(time_components[2])
  hours <- as.numeric(time_components[3])
  minutes <- as.numeric(time_components[4])
  seconds <- as.numeric(time_components[5])
  total_hours <- days * 24 + hours + minutes / 60 + seconds / 3600
  return(total_hours)
}

# Create new column time_to_reply(hour) based on the time_to_reply column
data1 <- data1 %>%
  mutate(`time_to_reply(hour)` = sapply(time_to_reply, convert_to_hours))

# Create histogram for the 'time_to_reply(hour)' column
ggplot(data1, aes(x = `time_to_reply(hour)`)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histogram of Time to Reply (in hours)",
       x = "Time to Reply (hours)",
       y = "Frequency") +
  theme_minimal()

# Function to extract the number of days from the formatted time difference
extract_days <- function(time_str) {
  if (is.na(time_str) || time_str == "") {
    return(NA)
  }
  time_components <- str_match(time_str, "(\\d+)d (\\d+)h (\\d+)m (\\d+)s")
  if (is.na(time_components[1, 1])) {
    return(NA)
  }
  days <- as.numeric(time_components[2])
  return(days)
}

# Create new column time_to_reply(day) based on the time_to_reply column
data1 <- data1 %>%
  mutate(`time_to_reply(day)` = sapply(time_to_reply, extract_days))

# Filter rows where time_to_reply(day) is 100 or less
filtered_data <- data1 %>%
  filter(`time_to_reply(day)` <= 100)

# Create histogram for the 'time_to_reply(day)' column
ggplot(filtered_data, aes(x = `time_to_reply(day)`)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histogram of Time to Reply (in days) for values 100 or less",
       x = "Time to Reply (days)",
       y = "Frequency") +
  theme_minimal()

# Filter rows where time_to_reply(day) is more than 100
filtered_data <- data1 %>%
  filter(`time_to_reply(day)` > 100)

### Create histogram for the 'time_to_reply(day)' column
ggplot(filtered_data, aes(x = `time_to_reply(day)`)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  labs(title = "Histogram of Time to Reply (in days) for values more than 100",
       x = "Time to Reply (days)",
       y = "Frequency") +
  theme_minimal()

# Remove the 'time_to_reply(hour)' column
data1 <- data1 %>%
  select(-`time_to_reply(hour)`)

### Create new column time_to_reply(day_grouped) based on the time_to_reply(day) column
data1 <- data1 %>%
  mutate(`time_to_reply(day_grouped)` = case_when(
    `time_to_reply(day)` >= 0 & `time_to_reply(day)` <= 2 ~ "0 - 2",
    `time_to_reply(day)` >= 3 & `time_to_reply(day)` <= 5 ~ "3 - 5",
    `time_to_reply(day)` >= 6 & `time_to_reply(day)` <= 8 ~ "6 - 8",
    `time_to_reply(day)` > 8 ~ ">8",
    TRUE ~ "No/Bot Response"
  ))

# Ensure the 'Date of First Email Sent' column is in the correct timezone
data1$`Date of First Email Sent` <- with_tz(data1$`Date of First Email Sent`, tzone = "America/Los_Angeles")

# Create new column with recipient's local datetime
data1 <- data1 %>%
  rowwise() %>%
  mutate(`Recipient Local Datetime` = with_tz(`Date of First Email Sent`, tzone = state_timezones[[State]]))

# Revert the 'Date of First Email Sent' column to the original timezone
data1$`Date of First Email Sent` <- force_tz(data1$`Date of First Email Sent`, tzone = "America/Los_Angeles")

current_datetime <- Sys.time()
print(current_datetime)

sheet1_name <- "CR Analysis"  # Replace with the actual name of your sheet
data1_copy <- range_read(sheet, sheet = sheet1_name)

View(data1_copy)

# Perform a left join to replace the 'Date of First Email Sent' in data1
data1 <- data1 %>%
  left_join(select(data1_copy, Name, `Date of First Email Sent`), by = "Name", suffix = c("", ".new")) %>%
  mutate(`Date of First Email Sent` = coalesce(`Date of First Email Sent.new`, `Date of First Email Sent`)) %>%
  select(-`Date of First Email Sent.new`)


# Remove rows where "Current Stage" is "Backlog"
data1 <- data1 %>% filter(`Current Stage` != "Backlog")

# Remove rows 593 and 595
data1 <- data1 %>%
  slice(-c(593, 595))

# Create a copy of data1 and call it main_data
main_data <- data1
View(main_data)

# Experiment
exp_time = data1[7, "Date of First Email Sent"]
exp_time
with_tz(exp_time, state_timezones["NY"])
exp_time

# Method 2: Print the timezone attribute directly using attr()
print(attr(exp_time, "tzone"))
timezone <- attr(exp_time, "tzone")
print(timezone)

# Ensure 'Date of First Email Sent' has CA timezone
main_data <- main_data %>%
  mutate(`Date of First Email Sent` = force_tz(`Date of First Email Sent`, tzone = "America/Los_Angeles"))

# Create new column with recipient's local datetime
main_data <- main_data %>%
  rowwise() %>%
  mutate(`Recipient Local Datetime` = with_tz(`Date of First Email Sent`, tzone = state_timezones[[State]]))

# Check the current system timezone
system_timezone <- Sys.timezone()
print(system_timezone)

# Check the timezone of the current R session
session_info <- Sys.getenv("TZ")
print(session_info)

# Set the global timezone for the R session to America/Los_Angeles
Sys.setenv(TZ = "America/Los_Angeles")

# Verify the change
system_timezone <- Sys.timezone()
print(system_timezone)

# Create new column with grouped hour data
data1 <- data1 %>%
  mutate(`Date of First Email Sent(Grouped)` = case_when(
    hour(`Date of First Email Sent`) >= 0 & hour(`Date of First Email Sent`) <= 5 ~ "12am - 5am",
    hour(`Date of First Email Sent`) >= 6 & hour(`Date of First Email Sent`) <= 11 ~ "6am - 11am",
    hour(`Date of First Email Sent`) >= 12 & hour(`Date of First Email Sent`) <= 17 ~ "12pm - 5pm",
    hour(`Date of First Email Sent`) >= 18 & hour(`Date of First Email Sent`) <= 23 ~ "6pm - 11pm",
    TRUE ~ "Missing data"
  ))

# Function to format hours to 12-hour format with am/pm
format_hour <- function(datetime) {
  hour <- hour(datetime)
  if (hour == 0) {
    return("12am")
  } else if (hour == 12) {
    return("12pm")
  } else if (hour < 12) {
    return(paste0(hour, "am"))
  } else {
    return(paste0(hour - 12, "pm"))
  }
}

# Create new column with the formatted hour
data1 <- data1 %>%
  mutate(`Time of First Email Sent(Grouped)` = sapply(`Date of First Email Sent`, format_hour))

# Update rows that say 2am or 8am to "others"
data1 <- data1 %>%
  mutate(`Time of First Email Sent(Grouped)` = ifelse(`Time of First Email Sent(Grouped)` %in% c("2am", "6am"), "others", `Time of First Email Sent(Grouped)`))

# Create the bar graph
ggplot(data1, aes(x = `Time of First Email Sent(Grouped)`)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Email Sent Times",
       x = "Time of First Email Sent (Grouped)",
       y = "Frequency") +
  theme_minimal()

# Create the new dataframe with the selected columns
dummy_variable_data <- data1 %>%
  select(Name, Region, `All_Stages(Grouped)`, `Role(Grouped)`, `Tuition (Grouped)`, `Day of First Email Sent`)

View(dummy_variable_data)

# Perform one-hot encoding for all categorical columns
dummy_variable_data_encoded <- dummy_variable_data %>%
  dummy_cols(select_columns = c("Region", "All_Stages(Grouped)", "Role(Grouped)", "Tuition (Grouped)", "Sent Email Count(Grouped)", "time_to_reply(day_grouped)", "Time of First Email Sent(Grouped)"))

# Remove the original categorical columns
dummy_variable_data_encoded <- dummy_variable_data_encoded %>%
  select(-c("Region", "All_Stages(Grouped)", "Role(Grouped)", "Tuition (Grouped)", "Sent Email Count(Grouped)", "time_to_reply(day_grouped)", "Time of First Email Sent(Grouped)"))

View(dummy_variable_data_encoded)

# Split the comma-separated values into individual items
dummy_variable_data_split <- dummy_variable_data %>%
  separate_rows(`All_Stages(Grouped)`, sep = ", ") %>%
  separate_rows(`Role(Grouped)`, sep = ", ")

# Perform one-hot encoding for all columns
dummy_variable_data_encoded <- dummy_variable_data_split %>%
  mutate(across(c(Region, `All_Stages(Grouped)`, `Role(Grouped)`, `Tuition (Grouped)`, `Sent Email Count(Grouped)`, `time_to_reply(day_grouped)`, `Time of First Email Sent(Grouped)`), as.factor)) %>%
  pivot_wider(names_from = c(`All_Stages(Grouped)`, `Role(Grouped)`), values_from = c(`All_Stages(Grouped)`, `Role(Grouped)`), values_fill = list(`All_Stages(Grouped)` = 0, `Role(Grouped)` = 0), values_fn = list(`All_Stages(Grouped)` = ~1, `Role(Grouped)` = ~1)) %>%
  mutate(across(starts_with("All_Stages(Grouped)_") | starts_with("Role(Grouped)_"), ~ ifelse(is.na(.), 0, .))) %>%
  pivot_wider(names_from = c(Region, `Tuition (Grouped)`, `Sent Email Count(Grouped)`, `time_to_reply(day_grouped)`, `Time of First Email Sent(Grouped)`), values_from = c(Region, `Tuition (Grouped)`, `Sent Email Count(Grouped)`, `time_to_reply(day_grouped)`, `Time of First Email Sent(Grouped)`), values_fill = list(Region = 0, `Tuition (Grouped)` = 0, `Sent Email Count(Grouped)` = 0, `time_to_reply(day_grouped)` = 0, `Time of First Email Sent(Grouped)` = 0), values_fn = list(Region = ~1, `Tuition (Grouped)` = ~1, `Sent Email Count(Grouped)` = ~1, `time_to_reply(day_grouped)` = ~1, `Time of First Email Sent(Grouped)` = ~1)) %>%
  mutate(across(starts_with("Region_") | starts_with("Tuition (Grouped)_") | starts_with("Sent Email Count(Grouped)_") | starts_with("time_to_reply(day_grouped)_") | starts_with("Time of First Email Sent(Grouped)_"), ~ ifelse(is.na(.), 0, .))) %>%
  group_by(Name) %>%
  summarise(across(everything(), max)) %>%
  ungroup()

# Function to one-hot encode a single column
one_hot_encode_column <- function(data, column) {
  data %>%
    separate_rows(!!sym(column), sep = ", ") %>%
    mutate(!!sym(column) := as.factor(!!sym(column))) %>%
    pivot_wider(names_from = !!sym(column), values_from = !!sym(column), 
                values_fill = list(`All_Stages(Grouped)` = 0, `Role(Grouped)` = 0), 
                values_fn = list(`All_Stages(Grouped)` = length, `Role(Grouped)` = length)) %>%
    mutate(across(starts_with(column), ~ ifelse(is.na(.), 0, .)))
}

# One-hot encode 'Region'
dummy_variable_data <- dummy_variable_data %>%
  mutate(Region = as.factor(Region)) %>%
  pivot_wider(names_from = Region, values_from = Region, values_fill = list(Region = 0), values_fn = list(Region = length)) %>%
  mutate(across(starts_with("Region"), ~ ifelse(is.na(.), 0, .)))

# One-hot encode 'Tuition (Grouped)'
dummy_variable_data <- dummy_variable_data %>%
  mutate(`Tuition (Grouped)` = as.factor(`Tuition (Grouped)`)) %>%
  pivot_wider(names_from = `Tuition (Grouped)`, values_from = `Tuition (Grouped)`, values_fill = list(`Tuition (Grouped)` = 0), values_fn = list(`Tuition (Grouped)` = length)) %>%
  mutate(across(starts_with("Tuition (Grouped)"), ~ ifelse(is.na(.), 0, .)))

# One-hot encode 'Sent Email Count(Grouped)'
dummy_variable_data <- dummy_variable_data %>%
  mutate(`Sent Email Count(Grouped)` = as.factor(`Sent Email Count(Grouped)`)) %>%
  pivot_wider(names_from = `Sent Email Count(Grouped)`, values_from = `Sent Email Count(Grouped)`, values_fill = list(`Sent Email Count(Grouped)` = 0), values_fn = list(`Sent Email Count(Grouped)` = length)) %>%
  mutate(across(starts_with("Sent Email Count(Grouped)"), ~ ifelse(is.na(.), 0, .)))

# One-hot encode 'time_to_reply(day_grouped)'
dummy_variable_data <- dummy_variable_data %>%
  mutate(`time_to_reply(day_grouped)` = as.factor(`time_to_reply(day_grouped)`)) %>%
  pivot_wider(names_from = `time_to_reply(day_grouped)`, values_from = `time_to_reply(day_grouped)`, values_fill = list(`time_to_reply(day_grouped)` = 0), values_fn = list(`time_to_reply(day_grouped)` = length)) %>%
  mutate(across(starts_with("time_to_reply(day_grouped)"), ~ ifelse(is.na(.), 0, .)))

# One-hot encode 'Time of First Email Sent(Grouped)'
dummy_variable_data <- dummy_variable_data %>%
  mutate(`Time of First Email Sent(Grouped)` = as.factor(`Time of First Email Sent(Grouped)`)) %>%
  pivot_wider(names_from = `Time of First Email Sent(Grouped)`, values_from = `Time of First Email Sent(Grouped)`, values_fill = list(`Time of First Email Sent(Grouped)` = 0), values_fn = list(`Time of First Email Sent(Grouped)` = length)) %>%
  mutate(across(starts_with("Time of First Email Sent(Grouped)"), ~ ifelse(is.na(.), 0, .)))

copy_dummy_data <- dummy_variable_data
View(copy_dummy_data)

copy_dummy_data <- one_hot_encode_column(copy_dummy_data, "All_Stages(Grouped)")
copy_dummy_data <- one_hot_encode_column(copy_dummy_data, "Role(Grouped)")

# One-hot encode 'Day of First Email Sent'
copy_dummy_data <- copy_dummy_data %>%
  mutate(`Day of First Email Sent` = as.factor(`Day of First Email Sent`)) %>%
  pivot_wider(names_from = `Day of First Email Sent`, values_from = `Day of First Email Sent`, values_fill = list(`Day of First Email Sent` = 0), values_fn = list(`Day of First Email Sent` = length)) %>%
  mutate(across(starts_with("Day of First Email Sent"), ~ ifelse(is.na(.), 0, .)))

# Check the timezone of the 'Date of First Email Sent' column
timezone <- attr(data1$`Date of First Email Sent`, "tzone")
print(timezone)

# Change timezone to 'Asia/Manila' without altering the actual time values
data1$`Date of First Email Sent` <- force_tz(data1$`Date of First Email Sent`, tzone = "Asia/Manila")

# Convert 'Date of First Email Sent' to 'America/Los_Angeles' time
data1 <- data1 %>%
  mutate(`Date of First Email Sent(CA)` = with_tz(`Date of First Email Sent`, tzone = "America/Los_Angeles"))

# Remove rows where "Name" is "Kohelet Yeshiva High School"
data1 <- data1 %>% filter(Name != "Kohelet Yeshiva High School")

# Remove rows where "Name" is "Kohelet Yeshiva High School"
main_data <- main_data %>% filter(Name != "Kohelet Yeshiva High School")

# Remove rows where "Name" is "Kohelet Yeshiva High School"
copy_dummy_data <- copy_dummy_data %>% filter(Name != "Kohelet Yeshiva High School")

# Create the Time of First Email Sent(Grouped) column in the desired format
main_data <- main_data %>%
  mutate(`Time of First Email Sent(Grouped)` = format(`Date of First Email Sent(CA)`, format = "%I%p"))

# Ensure the AM/PM is in lowercase and remove leading zeros
main_data$`Time of First Email Sent(Grouped)` <- tolower(gsub("^0", "", main_data$`Time of First Email Sent(Grouped)`))

# Create a histogram for the Time of First Email Sent(Grouped) column
ggplot(main_data, aes(x = `Time of First Email Sent(Grouped)`)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Email Sent Times",
       x = "Time of First Email Sent (Grouped)",
       y = "Frequency") +
  theme_minimal()

# Update the Time of First Email Sent(Grouped) column to replace "3pm" and "5pm" with "Others"
main_data <- main_data %>%
  mutate(`Time of First Email Sent(Grouped)` = ifelse(`Time of First Email Sent(Grouped)` %in% c("3pm", "5pm"), "others", `Time of First Email Sent(Grouped)`))

# Convert 'Date of First Email Sent(CA)' to recipient's local time based on state
main_data <- main_data %>%
  rowwise() %>%
  mutate(`Recipient Local Datetime` = with_tz(`Date of First Email Sent(CA)`, tzone = state_timezones[[State]]))

# Check the timezone of the 'Date of First Email Sent' column
timezone <- attr(main_data$`Date of First Email Sent(CA)`, "tzone")
print(timezone)

# Define correct offsets for each state relative to 'America/Los_Angeles'
state_offsets <- c(
  AL = 2, AK = 1, AZ = 0, AR = 2, CA = 0, CO = 1,
  CT = 3, DE = 3, FL = 3, GA = 3, HI = -2, ID = 1,
  IL = 2, IN = 3, IA = 2, KS = 2, KY = 3, LA = 2,
  ME = 3, MD = 3, MA = 3, MI = 3, MN = 2, MS = 2,
  MO = 2, MT = 1, NE = 2, NV = 0, NH = 3, NJ = 3,
  NM = 1, NY = 3, NC = 3, ND = 2, OH = 3, OK = 2,
  OR = 0, PA = 3, RI = 3, SC = 3, SD = 2, TN = 2,
  TX = 2, UT = 1, VT = 3, VA = 3, WA = 0, WV = 3,
  WI = 2, WY = 1
)

# Add the offsets column based on state
main_data <- main_data %>%
  mutate(Offset = state_offsets[State])

# Update the Recipient Local Datetime column by adding the offset
main_data <- main_data %>%
  mutate(`Recipient Local Datetime` = `Date of First Email Sent(CA)` + hours(Offset))

# Create the Recipient Local Datetime(Grouped) column in the desired format
main_data <- main_data %>%
  mutate(`Recipient Local Datetime(Grouped)` = format(`Recipient Local Datetime`, format = "%I%p"))

# Ensure the AM/PM is in lowercase and remove leading zeros
main_data$`Recipient Local Datetime(Grouped)` <- tolower(gsub("^0", "", main_data$`Recipient Local Datetime(Grouped)`))

# Create a histogram for the Recipient Local Datetime(Grouped) column
ggplot(main_data, aes(x = `Recipient Local Datetime(Grouped)`)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Recipient Local Datetime",
       x = "Recipient Local Datetime (Grouped)",
       y = "Frequency") +
  theme_minimal()

# Update the Recipient Local Datetime(Grouped) column to replace "5pm" and "8pm" with "Others"
main_data <- main_data %>%
  mutate(`Recipient Local Datetime(Grouped)` = ifelse(`Recipient Local Datetime(Grouped)` %in% c("5pm", "8pm"), "others", `Recipient Local Datetime(Grouped)`))

# Assuming your data frame is called main_data
main_data <- main_data %>%
  mutate(Engagement = case_when(
    `Current Stage` %in% c("Imported", "Received Summer Drip", "Blocked", "Unsubscribe", "Passed b/c own tutoring, EF / non-virtual") ~ "Not Engaged",
    `Current Stage` %in% c("Ghosted", "Contact Later", "Called but no answer", "Will call (unresponsive 2x)") ~ "Unsure",
    `Current Stage` %in% c("Passed our info to school", "Shared/will share our info with families", "Connected us with families") ~ "Engaged",
    TRUE ~ NA_character_  # Default case if none of the conditions match
  ))

# Assuming your data frame is called main_data
colnames(main_data)

# Create a density plot
ggplot(main_data, aes(x = `Received Email Count`, fill = Engagement)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("Engaged" = "green", "Not Engaged" = "red", "Unsure" = "yellow")) +
  labs(title = "Density Plot of Received Email Count by Engagement",
       x = "Received Email Count",
       y = "Density",
       fill = "Engagement") +
  theme_minimal()

# Filter the data to include only rows where Received Email Count is not 0
filtered_data <- main_data %>% filter(`Received Email Count` != 0)

# Create a density plot with the filtered data
ggplot(filtered_data, aes(x = `Received Email Count`, fill = Engagement)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("Engaged" = "green", "Not Engaged" = "red", "Unsure" = "yellow")) +
  labs(title = "Density Plot of Received Email Count by Engagement (Non-zero counts only)",
       x = "Received Email Count",
       y = "Density",
       fill = "Engagement") +
  theme_minimal()

# Remove the column using select
copy_dummy_data <- copy_dummy_data %>% select(-`EF / non-virtual`)

# Remove the specified columns using base R
copy_dummy_data <- copy_dummy_data[, !names(copy_dummy_data) %in% c("6 - 10", ">20", "11 - 15", "16 - 20", "1 - 5")]

# Remove the specified columns using base R
copy_dummy_data <- copy_dummy_data[, !names(copy_dummy_data) %in% c("1am", "11pm", "12am", "10pm", "8am", "others")]

# Create the new column with the day of the week
main_data <- main_data %>%
  mutate(`Day of First Email Sent(Local)` = wday(`Recipient Local Datetime`, label = TRUE, abbr = FALSE))

# Remove the specified columns using base R
copy_dummy_data <- copy_dummy_data[, !names(copy_dummy_data) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")]

# Select the relevant columns from main_data
selected_columns <- main_data %>% select(Name, `Sent Email Count`, `Received Email Count`)

# Join the columns to copy_dummy_data based on the common identifier
copy_dummy_data <- copy_dummy_data %>%
  left_join(selected_columns, by = "Name")

# Remove rows 609 and 610
copy_dummy_data <- copy_dummy_data %>% slice(-c(609, 610))

# Remove duplicate rows
main_data <- main_data %>%
  distinct()

# Remove duplicate rows
copy_dummy_data <- copy_dummy_data %>%
  distinct()

# Remove the specified rows using base R
copy_dummy_data <- copy_dummy_data[-c(594, 595), ]

# Change the data in the Name column for row 593
main_data[607, "Name"] <- "Notre Dame Academy CA"

# Change the data in the Name column for row 593
copy_dummy_data[607, "Name"] <- "Notre Dame Academy CA"

# Rename the column
main_data <- main_data %>%
  rename(`Time of First Email Sent(Local)` = `Recipient Local Datetime(Grouped)`)

# Remove the specified column using select
main_data <- main_data %>%
  select(-`Day of First Email Sent (Local)`)

# Select the relevant column from main_data
selected_columns <- main_data %>% select(Name, `Day of First Email Sent(Local)`)

# Join the column to copy_dummy_data based on the common identifier
copy_dummy_data <- copy_dummy_data %>%
  left_join(selected_columns, by = "Name")

# Get the position of the "Day of First Email Sent (Local)" column
col_position <- which(names(copy_dummy_data) == "Day of First Email Sent(Local)")

# Select columns up to "Day of First Email Sent (Local)"
copy_dummy_data <- copy_dummy_data[, 1:col_position]

# One-hot encode 'Day of First Email Sent(Local)'
copy_dummy_data <- copy_dummy_data %>%
  mutate(`Day of First Email Sent` = as.factor(`Day of First Email Sent(Local)`)) %>%
  pivot_wider(names_from = `Day of First Email Sent(Local)`, values_from = `Day of First Email Sent(Local)`, values_fill = list(`Day of First Email Sent(Local)` = 0), values_fn = list(`Day of First Email Sent(Local)` = length)) %>%
  mutate(across(starts_with("Day of First Email Sent(Local)"), ~ ifelse(is.na(.), 0, .)))

# Create a histogram for the "Day of First Email Sent(Local)" column
ggplot(main_data, aes(x = `Day of First Email Sent(Local)`)) +
  geom_bar() +
  labs(title = "Histogram of Day of First Email Sent (Local)",
       x = "Day of the Week",
       y = "Count") +
  theme_minimal()

# Create a histogram for the "Day of First Email Sent (Local)" column
ggplot(main_data, aes(x = `Day of First Email Sent (Local)`)) +
  geom_bar() +
  labs(title = "Histogram of Day of First Email Sent (Local)",
       x = "Day of the Week",
       y = "Count") +
  theme_minimal()

# Select the relevant column from main_data
selected_columns <- main_data %>% select(Name, `Time of First Email Sent(Local)`)

# Join the column to copy_dummy_data based on the common identifier
copy_dummy_data <- copy_dummy_data %>%
  left_join(selected_columns, by = "Name")

# One-hot encode 'Time of First Email Sent(Local)'
copy_dummy_data <- copy_dummy_data %>%
  mutate(`Time of First Email Sent(Local)` = as.factor(`Time of First Email Sent(Local)`)) %>%
  pivot_wider(names_from = `Time of First Email Sent(Local)`, 
              values_from = `Time of First Email Sent(Local)`, 
              values_fill = list(`Time of First Email Sent(Local)` = 0), 
              values_fn = list(`Time of First Email Sent(Local)` = length)) %>%
  mutate(across(starts_with("Time of First Email Sent(Local)"), ~ ifelse(is.na(.), 0, .)))

# Remove the specified column using select
copy_dummy_data <- copy_dummy_data %>%
  select(-`Day of First Email Sent`)

# Create a copy of copy_dummy_data and name it Model_Set
Model_Set <- copy_dummy_data

View(Model_Set)

# Create a histogram for the "Role (Grouped)" column
ggplot(main_data, aes(x = `Role(Grouped)`)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Histogram of Role (Grouped)",
       x = "Role (Grouped)",
       y = "Count") +
  theme_minimal()

# Remove the "7pm" column using select
Model_Set <- Model_Set %>%
  select(-`Wednesday`)

# Create a bar plot for the "Time of First Email Sent (Local)" column
ggplot(main_data, aes(x = `Time of First Email Sent(Local)`)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Plot of Time of First Email Sent (Local)",
       x = "Time of First Email Sent (Local)",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Remove the specified columns using select
Model_Set <- Model_Set %>%
  select(-`No/Bot Response`, -`6 - 8`, -`>8`, -`3 - 5`, -`0 - 2`)

Model_Set1 <- Model_Set
View(Model_Set1)

# Modify the Current Stage column for specific rows
main_data <- main_data %>%
  mutate(`Current Stage` = ifelse(Name %in% c("Fusion Academy", "Sacred Heart Greenwich"), "Connected us with families", `Current Stage`))

main_data <- main_data %>%
  mutate(`Current Stage` = ifelse(Name %in% c("Bellarmine College Prep", "Monterey Bay Academy", "Rumson Country Day School"), "Connected us with families", `Current Stage`))

main_data <- main_data %>%
  mutate(Engaged = 1)

main_data <- main_data %>%
  mutate(Engaged = ifelse(`Current Stage` == "Received Summer Drip" & 
                            `All_Stages(Grouped)` %in% c("Imported, Sent Mail Merges", 
                                                         "Sent Mail Merges", 
                                                         "Imported, Sent Mail Merges, Blocked/Unsubscribe"), 
                          0, Engaged))

main_data <- main_data %>%
  mutate(Engaged = ifelse(`Current Stage` %in% c("Blocked", "Unsubscribe", "Passed b/c own tutoring, EF / non-virtual"), 0, Engaged))

main_data <- main_data %>%
  mutate(`Referred to a Student` = 0)

main_data <- main_data %>%
  mutate(`Referred to a Student` = ifelse(grepl("Connected us with families", `All_Stages`), 1, `Referred to a Student`))

Model_Set <- Model_Set %>%
  left_join(main_data %>% select(Name, Engaged, `Referred to a Student`), by = "Name")

Model_Set <- Model_Set %>% select(-Midwest, -`>40K`, -Principal)

Engaged_Set <- Model_Set
View(Engaged_Set)

Engaged_Set <- Engaged_Set %>%
  select(-`Referred to a Student`, -Imported, -`Sent Mail Merges`, -`Warm Leads`, 
         -`Outreached our Services`, -`Became Unresponsive`, -Ghosted, -`Blocked/Unsubscribe`, 
         -`Passed b/c own tutoring`, -`Connected us with families`)

# Reorder the columns in Engaged_Set to make Engaged the first column
Engaged_Set <- Engaged_Set %>%
  select(Engaged, everything())

# Remove the Name column from Engaged_Set
Engaged_Set <- Engaged_Set %>%
  select(-Name)

Engaged_Set <- Engaged_Set %>% select(-`Received Email Count`)

# Create a copy of Model_Set and name it Referred_Set
Referred_Set <- Model_Set
View(Referred_Set)

# Filter out rows with Engaged equal to 0 from Referred_Set
Referred_Set <- Referred_Set %>%
  filter(Engaged != 0)

# Remove the specified columns from Referred_Set
Referred_Set <- Referred_Set %>%
  select(-Engaged, -`Connected us with families`, -Ghosted, -`Blocked/Unsubscribe`, -`Passed b/c own tutoring`)

# Make the last column the first column
last_col_name <- colnames(Referred_Set)[ncol(Referred_Set)]
Referred_Set <- Referred_Set %>%
  select(all_of(last_col_name), everything())

# Remove the Name column
Referred_Set <- Referred_Set %>%
  select(-Name)

# Step 1: Copy the original column
main_data <- main_data %>%
  mutate(`Copy_Date` = `Date of First Received Email`)

# Step 2: Create a new column and change the timezone to Philippine Time
main_data <- main_data %>%
  mutate(`Date of First Received Email(PH time)` = 
           force_tz(`Copy_Date`, tzone = "Asia/Manila"))

# Calculate the time difference in a custom format
main_data <- main_data %>%
  mutate(time_difference = difftime(`Date of First Received Email(PH time)`, `Date of First Email Sent`, units = "secs")) %>%
  mutate(time_difference_formatted = sprintf(
    "%d days %d hours %d min",
    as.integer(time_difference) %/% 86400,  
    (as.integer(time_difference) %% 86400) %/% 3600,  
    (as.integer(time_difference) %% 3600) %/% 60  
  ))

# Create the new column "Time to Response" based on the conditions
main_data <- main_data %>%
  mutate(`Time to Response` = ifelse(
    Engaged == 1 & 
      as.numeric(difftime(`Date of First Received Email(PH time)`, `Date of First Email Sent`, units = "mins")) > 1 & 
      as.numeric(difftime(`Date of First Received Email(PH time)`, `Date of First Email Sent`, units = "days")) <= 7,
    time_difference_formatted,
    NA  
  ))