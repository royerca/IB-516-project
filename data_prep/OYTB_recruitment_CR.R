# Load required packages
library("dplyr")
library("readxl")

# Load datasets
oytb_data <- read.delim("Recruitment2024.csv", stringsAsFactors = FALSE) %>%
  filter(spp == "OYTB")  # Select only OYTB species

props_sfla <- read_xlsx("props_sfla_less_columns.xlsx")

# Create a lookup vector for month abbreviations
month_lookup <- setNames(1:12, c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", 
                                 "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"))

# Convert abbreviated months in props_sfla to numbers
props_sfla <- props_sfla %>%
  mutate(month = month_lookup[month])

# Ensure consistent data types for merging
oytb_data <- oytb_data %>%
  mutate(day = as.integer(day),
         collection = as.integer(collection),
         mooring = as.character(mooring),
         mr = as.character(mr),
         interval = as.numeric(interval),
         year = as.integer(year))


props_sfla <- props_sfla %>%
  mutate(day = as.integer(day),
         collection = as.integer(collection),
         mooring = as.character(mooring),
         mr = as.character(mr),
         month = as.integer(month),
         interval = as.numeric(interval),
         year = as.integer(year))

head(props_sfla)
head(oytb_data)

# Remove duplicate columns from props_sfla that already exist in oytb_data
props_sfla_clean <- props_sfla %>%
  select(-c(site, mr, collection, year, interval))  # Adjust based on the columns causing duplicates

# Remove duplicates based on day and mooring
props_sfla_clean <- props_sfla_clean %>%
  distinct(day, mooring, .keep_all = TRUE)

#########################
##TROUBLESHOOTING#######
########################
# Check if there's a matching mooring and day in both datasets
merged_check <- oytb_data %>%
  anti_join(props_sfla_clean, by = c("day", "mooring"))

# Print any mismatched rows to see where the issue might be
head(merged_check)

# Check for missing mooring or day values
sum(is.na(oytb_data$mooring))
sum(is.na(oytb_data$day))

# Check for duplicates in the props_sfla_clean dataset
props_sfla_clean %>%
  count(day, mooring) %>%
  filter(n > 1)
missing_row <- merged_data %>%
  filter(is.na(sfla_settlement)) %>%
  select(day, mooring)

print(missing_row)
oytb_data <- oytb_data %>%
  mutate(mooring = ifelse(day == 170914 & mooring == "", "RR01", mooring))


####################
# Perform the merge
merged_data <- oytb_data %>%
  left_join(props_sfla_clean, by = c("day", "mooring"))


# # Select the required columns, ensuring no duplicates
# final_data <- merged_data %>%
#   select(day, mooring, spp, count, region, site, mr, collection, date, year, 
#          interval, julian, month, sfla_settlement, smel_settlement, total_oytb_settlement)

final_data <- merged_data

# Preview the result
head(final_data)


final_data <- merged_data

# View first rows to confirm
head(final_data)

final_data <- final_data %>%
  mutate(year = as.numeric(year)) %>%
  filter(year != 2011, julian %in% c(121:255)) # June 1 - Sept 15; 121 is May 1


##Reorganize dataframe so that all settlement rates are under rate column
##creates new rows (4 for each day/mooring) for sfla, smel, un_ID, and total

# Step 1: Rename 'rate' column temporarily and pivot the data
df_long <- final_data %>%

# Temporarily rename 'rate' to avoid conflict during pivot
  rename(temp_rate = rate) %>%
  pivot_longer(cols = c(sfla_settlement, smel_settlement, total_oytb_settlement),
               names_to = "fish_type", values_to = "rate") %>%
  mutate(fish_type = case_when(
    fish_type == "sfla_settlement" ~ "sfla",
    fish_type == "smel_settlement" ~ "smel",
    fish_type == "total_oytb_settlement" ~ "total",
    TRUE ~ fish_type
  )) %>%
  select(-temp_rate)  # Drop the temporary 'rate' column

df_long <- df_long %>%
  select(-spp) %>%
  rename(spp = fish_type)

# Preview the results
head(df_long)


################################

# # Select required columns
# final_data <- merged_data %>%
#   select(day, site, collection, mooring, mr, interval, month, year, 
#          sfla_settlement, smel_settlement, julian, distance, n.moorings)

# Save output file
write.table(df_long, "OYTB_Recruitment2024.csv", 
            append = FALSE, quote = FALSE, sep = "\t", 
            row.names = FALSE, col.names = TRUE)


