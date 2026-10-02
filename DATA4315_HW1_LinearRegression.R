library(readr)
library(dplyr)

#Load the data sets
Batting <- read_csv("Batting.csv")
Fielding <- read_csv("Fielding.csv")
Pitching <- read_csv("Pitching.csv")
People <- read_csv("People.csv")
Salaries <- read_csv("Salaries.csv")

#----------------------------------------------
# Define US Consumer Price Index (CPI-U) annual average factors (1985–2025)
# Sourced from US Bureau of Labor Statistics (1982-1984 = 100 baseline)
cpi_data <- data.frame(
  yearID = 1985:2025,
  cpi = c(
    107.6, 109.6, 113.6, 118.3, 124.0, 130.7, 136.2, 140.3, 144.5, 148.2,  # 1985-1994
    152.4, 156.9, 160.5, 163.0, 166.6, 172.2, 177.1, 179.9, 184.0, 188.9,  # 1995-2004
    195.3, 201.6, 207.3, 215.3, 214.5, 218.1, 224.9, 229.6, 233.0, 236.7,  # 2005-2014
    237.0, 240.0, 245.1, 251.1, 255.7, 258.8, 271.0, 292.7, 304.7, 314.2,  # 2015-2024
    320.0                                                                 # 2025
  )
)

# Reference 2025 CPI for inflation scaling
cpi_2025 <- cpi_data$cpi[cpi_data$yearID == 2025]

# Filter Salaries table and apply transformations
salaries_adj <- Salaries %>%
  # Filter for consistent reporting era (1985 to 2025)
  filter(yearID >= 1985 & yearID <= 2025) %>%
  # Join with CPI lookup table
  left_join(cpi_data, by = "yearID") %>%
  # Perform salary transformations
  mutate(
    # Log-transformed salary (handles standard exponential growth in contracts)
    log_salary = log(salary),
    
    # Real salary in constant 2025 dollars
    salary_adj_2025 = salary * (cpi_2025 / cpi),
    
    # Log of adjusted 2025 salary
    log_salary_adj = log(salary_adj_2025)
  ) %>%
  # Remove temporary CPI working column
  select(-cpi)

#----------------------------------------------
# Aggregate Batting Stats per season
batting_clean <- Batting %>%
  group_by(playerID, yearID) %>%
  summarize(
    G = sum(G, na.rm = TRUE),
    AB = sum(AB, na.rm = TRUE),
    R = sum(R, na.rm = TRUE),
    H = sum(H, na.rm = TRUE),
    HR = sum(HR, na.rm = TRUE),
    RBI = sum(RBI, na.rm = TRUE),
    SB = sum(SB, na.rm = TRUE),
    BB = sum(BB, na.rm = TRUE),
    SO = sum(SO, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    BA = ifelse(AB > 0, H / AB, 0),
    OBP = ifelse((AB + BB) > 0, (H + BB) / (AB + BB), 0),
    SLG = ifelse(AB > 0, (H + 2 * (H - BB - SO) + 3 * HR) / AB, 0),
    OPS = OBP + SLG
  )

# Aggregate Salaries per season (in case traded mid-year)
salaries_clean <- salaries_adj %>%
  group_by(playerID, yearID) %>%
  summarize(
    salary = sum(salary, na.rm = TRUE),
    .groups = "drop"
  )

# Join Batting and Salaries
master_df <- salaries_clean %>%
  inner_join(batting_clean, by = c("playerID", "yearID")) %>%
  left_join(People %>% select(playerID, birthYear, nameGiven), by = "playerID") %>%
  mutate(Age = yearID - birthYear)

train_df <- master_df %>%
  filter(yearID >= 1985 & yearID <= 2018)

score_df <- master_df %>%
  filter(yearID >= 2019)


# C'est fini! Export the master data frame to a CSV file
#write_csv(master_df, "battingSalaryData.csv")
#write.csv(train_df, "battingSalaryData_Train.csv")
#write.csv(score_df, "battingSalaryData_Score.csv")
