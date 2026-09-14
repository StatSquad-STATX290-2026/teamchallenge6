
# ============================================================
# STATX290 - TEAM CHALLENGE 6
# US PRESIDENTIAL POLLING DATA 2012/2016
# DATA CLEANING + BINARY GLM PREPARATION
# ============================================================

# ============================================================
# STEP 0 - LOAD PACKAGES
# ============================================================

# load package 
library(tidyr)

# stringr:
# Used for manipulating and cleaning character strings
library(stringr)

# dplyr:
# Used for data manipulation such as mutate(), select(),
# rename(), relocate() and bind_rows()
library(dplyr)

# lubridate:
# Used for working with dates
library(lubridate)

# ggplot2:
# Used later for exploratory visualisation
library(ggplot2)

# readr:
# Used to import CSV files
library(readr)



# ============================================================
# CLEANING 1.1 - CLEAN 2012 POLLING DATA
# ============================================================

# Import the raw 2012 polling dataset
polls_2012_raw <- read_csv("state_polls_2012.csv")


# Create a cleaned version of the 2012 dataset
polls_2012_clean <- polls_2012_raw |>
  
  # Split poll_info into two new columns using "-president-"
  # as the separator.
  #
  # Example idea:
  # 2012-alabama-president-obama-vs-romney
  #
  # becomes:
  # year_state = 2012-alabama
  # major_party_candidates = obama-vs-romney
  separate_wider_delim(
    col = "poll_info",
    delim = "-president-",
    names = c(
      "year_state",
      "major_party_candidates"
    )
  ) |>
  
  # Remove "2012-" from year_state to obtain the state name.
  # Also create a year variable equal to 2012.
  mutate(
    state = str_remove(
      year_state,
      pattern = "2012-"
    ),
    year = 2012
  ) |>
  
  # year_state is no longer required because its information
  # has now been separated into state and year.
  select(-year_state) |>
  
  # "Other" contains text such as "Not included in poll".
  #
  # First convert this text to NA,
  # then convert the remaining values to numeric.
  mutate(
    Other = as.numeric(
      na_if(
        Other,
        "Not included in poll"
      )
    )
  ) |>
  
  # sample_size uses -1 to represent missing information.
  # Replace -1 with proper NA.
  mutate(
    sample_size = na_if(
      sample_size,
      -1
    )
  ) |>
  
  # Move these important variables to the front
  # to make the dataset easier to read.
  relocate(
    major_party_candidates,
    year,
    state
  )


# ------------------------------------------------------------
# CHECK THE 2012 CLEANING
# ------------------------------------------------------------

glimpse(polls_2012_clean)

summary(polls_2012_clean)

# Check missing values
colSums(is.na(polls_2012_clean))

# Check state values
sort(unique(polls_2012_clean$state))

# Check that sample size does not contain -1 anymore
summary(polls_2012_clean$sample_size)

# ============================================================
# CLEANING 1.2 - CLEAN 2016 POLLING DATA
# ============================================================

# Import the raw 2016 polling dataset
polls_2016_raw <- read_csv("state_polls_2016.csv")


polls_2016_clean <- polls_2016_raw |>
  
  # Split poll_info using "-president".
  #
  # The format differs slightly from the 2012 dataset,
  # which is why the delimiter is not exactly the same.
  separate_wider_delim(
    col = "poll_info",
    delim = "-president",
    names = c(
      "year_state",
      "major_party_candidates"
    )
  ) |>
  
  # Standardise the major-party candidate description
  # for all 2016 observations.
  mutate(
    major_party_candidates =
      "trump-vs-clinton"
  ) |>
  
  # Remove "2016-" from year_state
  # and create the year variable.
  mutate(
    state = str_remove(
      year_state,
      pattern = "2016-"
    ),
    year = 2016
  ) |>
  
  # Remove the original combined variable.
  select(-year_state) |>
  
  # Convert "Not included in poll" to NA
  # and then convert Other to numeric.
  mutate(
    Other = as.numeric(
      na_if(
        Other,
        "Not included in poll"
      )
    )
  ) |>
  
  # Convert -1 sample sizes to NA.
  mutate(
    sample_size = na_if(
      sample_size,
      -1
    )
  ) |>
  
  # Move key variables to the beginning of the dataset.
  relocate(
    major_party_candidates,
    year,
    state
  )


# ------------------------------------------------------------
# CHECK THE 2016 CLEANING
# ------------------------------------------------------------

glimpse(polls_2016_clean)

summary(polls_2016_clean)

colSums(is.na(polls_2016_clean))

sort(unique(polls_2016_clean$state))

summary(polls_2016_clean$sample_size)


# ============================================================
# CLEANING 1.3 - STANDARDISE 2016 DATA
# ============================================================

polls_2016_standardised <- polls_2016_clean |>
  
  # Rename Democratic and Republican candidates
  # into common variable names.
  #
  # Clinton = Democrat
  # Trump   = Republican
  rename(
    Dem = Clinton,
    Rep = Trump
  ) |>
  
  # Combine Johnson and McMullin into one Minor-party variable.
  #
  # Also record which candidates are represented by Minor.
  mutate(
    Minor = Johnson + McMullin,
    
    minor_party_candidates =
      "aggregate of johnson and mcmullin"
  ) |>
  
  # The individual minor candidate columns are no longer needed
  # after creating the combined Minor variable.
  select(
    -Johnson,
    -McMullin
  ) |>
  
  # Calculate how many days before Election Day
  # each poll started and ended.
  #
  # 2016 Election Day = 8 November 2016
  mutate(
    days_to_election_start =
      as.Date("2016-11-08") - start_date,
    
    days_to_election_end =
      as.Date("2016-11-08") - end_date
  ) |>
  
  # Remove original date columns because the new
  # relative-to-election variables have been created.
  select(
    -start_date,
    -end_date
  ) |>
  
  # Standardise Washington DC spelling so that
  # state names are consistent across datasets.
  mutate(
    state = replace_when(
      state,
      state == "washington-d-c" ~
        "washington-dc"
    )
  )
# ============================================================
# CLEANING 1.4 - STANDARDISE 2012 DATA
# ============================================================

polls_2012_standardised <- polls_2012_clean |>
  
  # Rename candidate-specific columns into common party names.
  #
  # Obama  = Democrat
  # Romney = Republican
  rename(
    Dem = Obama,
    Rep = Romney
  ) |>
  
  # The supplied 2012 dataset does not have equivalent
  # Johnson / McMullin columns.
  #
  # Therefore create Minor and its candidate description
  # as missing values so the structure matches 2016.
  mutate(
    Minor = NA_real_,
    minor_party_candidates = NA_character_
  ) |>
  
  # Calculate number of days before the 2012 Election Day.
  #
  # 2012 Election Day = 6 November 2012
  mutate(
    days_to_election_start =
      as.Date("2012-11-06") - start_date,
    
    days_to_election_end =
      as.Date("2012-11-06") - end_date
  ) |>
  
  # Remove original dates after creating
  # the election-distance measures.
  select(
    -start_date,
    -end_date
  )

# ============================================================
# CLEANING 1.5 - MERGE 2012 AND 2016
# ============================================================

polls_merged <- bind_rows(
  polls_2012_standardised,
  polls_2016_standardised
) |>
  
  # Move the main analysis variables to the front
  # to make the dataset easier to inspect.
  relocate(
    year,
    state,
    Dem,
    Rep,
    Undecided,
    Other,
    Minor,
    days_to_election_start,
    days_to_election_end
  ) |>
  
  # Move candidate-description variables to the end.
  relocate(
    major_party_candidates,
    minor_party_candidates,
    .after = last_col()
  ) |>
  
  # Poll candidate values were originally percentages
  # such as 45, 48, etc.
  #
  # Divide by 100 to convert them to proportions
  # such as 0.45 and 0.48.
  mutate(
    Dem = Dem / 100,
    Rep = Rep / 100,
    Undecided = Undecided / 100,
    Other = Other / 100,
    Minor = Minor / 100
  )


# ============================================================
# CHECK THE MERGED DATASET
# ============================================================

glimpse(polls_merged)

summary(polls_merged)

colSums(is.na(polls_merged))

table(polls_merged$year)

sort(unique(polls_merged$state))


# Check basic ranges
range(
  polls_merged$Dem,
  na.rm = TRUE
)

range(
  polls_merged$Rep,
  na.rm = TRUE
)

range(
  polls_merged$Undecided,
  na.rm = TRUE
)

# ============================================================
# TASK 3
# ARE ANY CLEANING STEPS MISSING?
# ============================================================

# The original script explicitly says that intermediate
# cleaning checks are not performed.
#
# We should therefore check several things.


# ------------------------------------------------------------
# CHECK 1 - DUPLICATED ROWS
# ------------------------------------------------------------

sum(
  duplicated(polls_merged)
)


# ------------------------------------------------------------
# CHECK 2 - MISSING VALUES
# ------------------------------------------------------------

colSums(
  is.na(polls_merged)
)


# ------------------------------------------------------------
# CHECK 3 - IMPOSSIBLE POLLING PROPORTIONS
# ------------------------------------------------------------

polls_merged |>
  filter(
    Dem < 0 | Dem > 1 |
      Rep < 0 | Rep > 1
  )


# ------------------------------------------------------------
# CHECK 4 - SAMPLE SIZE
# ------------------------------------------------------------

summary(
  polls_merged$sample_size
)


# ------------------------------------------------------------
# CHECK 5 - STATE NAMES
# ------------------------------------------------------------

sort(
  unique(
    polls_merged$state
  )
)


# ------------------------------------------------------------
# CHECK 6 - DAYS TO ELECTION
# ------------------------------------------------------------

summary(
  polls_merged$days_to_election_start
)

summary(
  polls_merged$days_to_election_end
)


# DISCUSSION:
#
# Potential additional cleaning steps include:
#
# 1. Checking duplicated observations.
#
# 2. Checking missing values after merging the two datasets.
#
# 3. Checking that polling percentages/proportions are
#    within sensible ranges.
#
# 4. Checking whether sample_size contains impossible
#    or unusual values.
#
# 5. Checking state naming consistency.
#
# 6. Checking that date-derived variables have sensible values.
#
# 7. Checking whether categorical variables need to be
#    converted to factors before modelling.



# ============================================================
# TASK 4
# CREATE THE BINARY VARIABLE: Dem_lead
# ============================================================

polls_merged <- polls_merged |>
  mutate(
    Dem_lead = Dem > Rep
  )


# Check the new variable
table(
  polls_merged$Dem_lead
)

prop.table(
  table(
    polls_merged$Dem_lead
  )
)


# GIẢI THÍCH:
#
# Dem_lead = TRUE
# when Democratic support is greater than Republican support.
#
# Dem_lead = FALSE
# when Democratic support is not greater than Republican support.
#
# Đây chính xác là rule của đề:
#
# Dem_lead = Dem > Rep



# ============================================================
# PREPARE Dem_lead FOR BINARY GLM
# ============================================================

polls_merged <- polls_merged |>
  mutate(
    Dem_lead = as.integer(
      Dem_lead
    )
  )


table(
  polls_merged$Dem_lead
)


# EXPLAIN:
#
# FALSE becomes 0
# TRUE becomes 1
#
# Therefore:
#
# 0 = Democrats are not leading
# 1 = Democrats are leading
#
# This is binary response suitable for logistic regression.



# ============================================================
# IMPORTANT - AVOID DATA LEAKAGE
# ============================================================

# DO NOT use Dem or Rep as predictors of Dem_lead.
#
# Why?
#
# Dem_lead was literally defined using:
#
# Dem > Rep
#
# If we use Dem and Rep as predictors,
# the model already has the exact information used to create
# the outcome.
#
# This would cause severe data leakage and make the model
# uninformative for prediction.



# ============================================================
# INSPECT POSSIBLE PREDICTORS
# ============================================================

names(
  polls_merged
)

glimpse(
  polls_merged
)


# GIẢI THÍCH:
# Before choosing final model,
# inspect all remaining variables.
#
# Potential useful predictors may include:
#
# year
# state
# sample_size
# days_to_election_start
# days_to_election_end
#
# and any poll characteristics retained from the raw files.
#
# Do NOT automatically include everything.
# Predictor choice should have a reasonable interpretation.



# ============================================================
# EXPLORATORY PLOT 1
# DEMOCRATIC LEAD BY YEAR
# ============================================================

ggplot(
  polls_merged,
  aes(
    x = factor(year),
    fill = factor(Dem_lead)
  )
) +
  geom_bar(
    position = "fill"
  ) +
  labs(
    title = "Democratic lead by election year",
    x = "Election year",
    y = "Proportion",
    fill = "Dem lead"
  ) +
  theme_minimal()


# EXPLAIN:
# This shows whether the proportion of polls with a Democratic
# lead differs between 2012 and 2016.



# ============================================================
# EXPLORATORY PLOT 2
# DAYS TO ELECTION
# ============================================================

ggplot(
  polls_merged,
  aes(
    x = factor(Dem_lead),
    y = as.numeric(days_to_election_end)
  )
) +
  geom_boxplot() +
  labs(
    title = "Days to election by Democratic lead",
    x = "Democratic lead",
    y = "Days to election"
  ) +
  theme_minimal()


# EXPLAIN:
# This examines whether polls taken closer to or further from
# Election Day show different probabilities of a Democratic lead.



# ============================================================
# EXPLORATORY PLOT 3
# SAMPLE SIZE
# ============================================================

ggplot(
  polls_merged,
  aes(
    x = factor(Dem_lead),
    y = sample_size
  )
) +
  geom_boxplot() +
  labs(
    title = "Sample size by Democratic lead",
    x = "Democratic lead",
    y = "Sample size"
  ) +
  theme_minimal()


# GIẢI THÍCH:
# This checks whether poll sample size appears related
# to whether the Democratic candidate is leading.



# ============================================================
# BUILD A STARTER BINARY GLM
# ============================================================

model_dem_lead <- glm(
  Dem_lead ~
    factor(year) +
    as.numeric(days_to_election_end) +
    sample_size,
  data = polls_merged,
  family = binomial()
)


summary(
  model_dem_lead
)


# Convert coefficients to odds ratios
exp(
  coef(
    model_dem_lead
  )
)


# 95% confidence intervals on odds-ratio scale
exp(
  confint(
    model_dem_lead
  )
)


# GIẢI THÍCH:
# Because Dem_lead is binary, use:
#
# glm(..., family = binomial())
#
# This fits a LOGISTIC REGRESSION.
#
# The model estimates:
#
# P(Dem_lead = 1)
#
# based on available poll characteristics.
#
# The coefficients from summary() are on the log-odds scale.
#
# exp(coef()) converts them into odds ratios.
#
# IMPORTANT:
# This is a sensible STARTER model using variables definitely
# present in the supplied cleaning script.
#
# Before choosing a final model, inspect the full dataset
# because there may be additional poll characteristics worth using.



# ============================================================
# TWO MODEL / DATA LIMITATIONS
# ============================================================

# LIMITATION 1:
#
# Polls are not necessarily independent or directly comparable.
# Different pollsters may use different sampling frames,
# survey modes, weighting procedures and population definitions.
# These methodological differences may influence Dem_lead
# but may not be fully captured by the model.
#
#
# LIMITATION 2:
#
# Dem_lead only records whether Democratic support is greater
# than Republican support.
#
# It ignores the SIZE of the polling lead and does not directly
# account for sampling uncertainty.
#
# A poll with:
#
# Dem = 0.451, Rep = 0.450
#
# receives the same Dem_lead = 1 outcome as:
#
# Dem = 0.60, Rep = 0.30
#
# even though the strength of evidence is very different.

