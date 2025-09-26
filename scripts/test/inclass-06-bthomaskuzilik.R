
# --------------------------- #
# --------------------------- #
#### ..... OBJECTIVE ..... ####
# --------------------------- #
# --------------------------- #

# Develop our own custom functions and use iteration to eliminate redundancy.

# We will do this by:

# 1) Writing a function for finding the top 5 donors for each representative. Post screenshot to Slack.
# 2) Write a ggplot function that creates a list of plots for each donor (alternative to facet_wrap). Post screenshot to Slack.

# ------------------------ #
# ------------------------ #
# ..... LOAD DATA ..... ####
# ------------------------ #
# ------------------------ #

library(tidyverse)  # loads all associated packages

# ------------------------------------------------ #
# .......... load objects separately .......... ####
# ------------------------------------------------ #

# There are lots of ways to load in data, some clunkier than others. What we did last week, which is the least ideal/readable option, is four separate pieces of nearly-identical code. This is hard to read and prone to error (e.g., cut and paste, but forget to change something), but it does work.
simpson_receipts <- read_delim("data/original/simpson_fec_17_26.csv", delim = ",")
glimpse(simpson_receipts)
fulcher_receipts <- read_delim("data/original/fulcher_fec_17_26.csv", delim = ",")
crapo_receipts <- read_delim("data/original/crapo_fec_17_26.csv", delim = ",")
risch_receipts <- read_delim("data/original/risch_fec_17_26.csv", delim = ",")

# # We now have four separate objects/dataframes in our environment. But there's easier and cleaner ways to do it... (in the interest of making sure everything is actually reading in correctly, I'm going to clear the environment after each attempt)
# rm(crapo_receipts, fulcher_receipts, risch_receipts, simpson_receipts)

# -------------------------------------------------------------------- #
# .......... slightly better (starting to use iteration) .......... ####
# -------------------------------------------------------------------- #

# map() from the purrr package is a iterative function similar to the apply functions in base R; it takes a series of things and does something to each of them. 

# Here, we are going to first create a list of csv files in our data file that follows a set pattern - how we tell R where to find the raw data we want to read in. I tell R where to look for the files with here::here(), tell it how to recognize the files I want with the pattern = argument (this works because all our files have the same naming conventions), and tell it I want the full pathways with full.names = TRUE. This will return a character vector with the 4 full pathways 
contribution_files <- list.files(here::here("data/original/"), pattern = "17_26.csv", full.names = TRUE)
contribution_files

# Next, we are going to use the map() function to bring in each of the files named in our contribution_files character vector. The first argument specifies we want map() to act on each object in the "contribution_files" vector, then we write a one-time function that tells R for each object, we want to apply the read_delim() function and that the delimiter is a ","
contribution_data <- map(contribution_files,
                         function(x) read_delim(x, delim = ","))
# This produces a list with 4 elements (one for each of the contribution datasets named in "contribution_files", each element is a tibble/dataframe)
contribution_data

# Clean environment
rm(contribution_data, contribution_files)

# ---------------------------------------------------------- #
# .......... even better (iteration w/ a pipe) .......... ####
# ---------------------------------------------------------- #

# We can improve this a little bit by using a pipe instead of having to create two separate named objects (contribution_data, contribution_files). We first use the list.files() function and then pipe the results of that directly into the map() function using the . in the first argument
contribution_data <- list.files(here::here("data/original/"),
                                           pattern = "17_26.csv",
                                           full.names = TRUE) %>% 
                     map(., function(x) read_delim (x, delim = ","))
# This produces the same things as above: a list with 4 elements (one for each of the contribution datasets retrieved using list.files(), where each element in the list is a tibble/dataframe)
contribution_data

# Clean environment
rm(contribution_data)

# ----------------------------------------------------------------------- #
# .......... even better (iteration w/ a pipe and read_csv) .......... ####
# ----------------------------------------------------------------------- #

# We can improve on our streamlined reading in process by using the read_csv() function vs. read_delim() function. This eliminates the need to write our our own function
contribution_data <- list.files(here::here("data/original/"),
                                pattern = "17_26.csv",
                                full.names = TRUE) %>% 
                     map(., read_csv)
# This produces the same things as above: a list with 4 elements (one for each of the contribution datasets retrieved using list.files(), where each element in the list is a tibble/dataframe) 
contribution_data

# Clean environment
rm(contribution_data)

# ------------------------------------ #
# .......... best option .......... ####
# ------------------------------------ #

# The last option is pretty good - readable, iterative... but we want to know who each dataset belongs to. Right now we don't explicitly know whose dataset in the list is whose: we could figure it out by looking at campaign names, or knowing that the function reads in the objects alphabetically (I assume). But we can write a use a piped function inside of map() that will create a new column in each tibble/dataframe of the list called "representation." This column will be populated with a string of characters that is extracted from that iteration's file name, in this case everything before "_fec"; each row in that dataframe will have the same repeated value (i.e., "crapo", "fulcher", "risch", "simpson")
contribution_data <- list.files(here::here("data/original/"), 
                                pattern = "17_26.csv", 
                                full.names = TRUE) %>%  
                     map(~ read_csv(.x) %>%
                         mutate(representative = str_extract(basename(.x), 
                                                             "^[^_]+(?=_fec_)")))
# Besides the new column, this produces the same things as above: a list with 4 elements (one for each of the contribution datasets retrieved using list.files(), where each element in the list is a tibble/dataframe) 
contribution_data

# We've now got our data read in. Now we want to take a look at it.

# Lists are a new format and are hard to look at, partly because they are so flexible (each object within the list can be a different class, e.g., character vector vs. tibble vs. matrix, as long as they are in their own slot). We can double-check that our contribution_data object is in fact a list
class(contribution_data)
# We can specify which slot in a list we want to act on by using [[]] within the () of a function. For example, we can use the glimpse() function on the first object/slot of our list by using [[1]]
glimpse(contribution_data[[1]])

# ---------------------------- #
# ---------------------------- #
# ..... ORGANIZE DATA ..... ####
# ---------------------------- #
# ---------------------------- #

# ------------------------------------------------- #
# .......... clean tibbles separately .......... ####
# ------------------------------------------------- #

# Like reading in, last time we filtered out unnecessary rows and columns in each tibble separately. We first created a character vectors with the column names we wanted to keep
select_vars <- c("is_individual", "contributor_name",  "contribution_receipt_amount", "committee_name", "report_year")

# For each tibble, we then used a pipe to first subset to only the specified columns in "select_vars", then we filtered so we only kept rows where "is_individual" was true
simpson_subset <- simpson_receipts %>% 
  select(all_of(select_vars)) %>% 
  filter(is_individual == TRUE)
glimpse(simpson_subset)

fulcher_subset <- fulcher_receipts %>% 
  select(all_of(select_vars)) %>% 
  filter(is_individual == TRUE)
glimpse(fulcher_subset)

crapo_subset <- crapo_receipts %>% 
  select(all_of(select_vars)) %>% 
  filter(is_individual == TRUE)
glimpse(crapo_subset)

risch_subset <- risch_receipts %>% 
  select(all_of(select_vars)) %>% 
  filter(is_individual == TRUE)
glimpse(risch_subset)

# This works but it is clunky... we no longer need the full individuals datasets, can remove to clean the enviornment
rm(crapo_receipts, fulcher_receipts, risch_receipts, simpson_receipts)

# ------------------------------------------------------------------- #
# .......... slightly better (writing our own function) .......... ####
# ------------------------------------------------------------------- #

# We can improve on this by creating a function that will subset our tibbles in the same way, but be cleaner. The first step in writing a function is to tell R what information/arguments we want to deal with and how we want to act on them. We have to name the function, which will be call like any other R function - in this case, we are naming it "select_ind_donors". We tell R we are writing a function with function(), and in the (), we specify the names of the arguments our function will need to run (these have not been created yet) - in this case, the function will need: 1) a dataframe or tibble ("df", first argument), 2) the column names we want to keep ("vars", 2nd argument, in {{}} because we will pass the function a character vector object/variable vs. individual values), and 3) the rows we want to keep ("donor_type", 3rd argument). We then tell R what we want to happen in the background when our "select_ind_donors" function is run (the things instead the {}). First, we want to act on "df", then we want to only keep columns specified in the "vars" character vector, then we want to only keep rows where "is_individual" is TRUE. 
select_ind_donors <- function(df, vars, donor_type){
  df %>% 
    select(all_of({{ vars }})) %>% 
    filter(is_individual == donor_type)
}

# We can test to make sure everything is working. We already have our "select_vars" character vector in the environment, so it should run. We're only going to apply our function to the first slot in our "contribution_data" list for now, just until we are sure everything is working.
test_fun <- select_ind_donors(df = contribution_data[[1]],
                              vars = select_vars,
                              donor_type = TRUE)

# We can check that everything went according to plan by checking that the dimensions of the resulting tibble against the tibble we got from using exisitng functions (3979, 5)
identical(dim(test_fun), dim(crapo_subset))
glimpse(test_fun)

# It worked! Remove the test object from the environment
rm(test_fun)

# ------------------------------------------------------------------- #
# .......... even better (our own function + iteration) .......... ####
# ------------------------------------------------------------------- #

# We can improve our workflow by using map() with our select_ind_donrs() function. This will apply the function to each slot in our "contribution_data" list in one go. Since our function requires multiple arguments, we have to explicitly call "function (x)" before select_ind_donors(). This allows map() to act on multiple slots in the contribution_data list and do all the things we need in our function (still a little fuzzy on this, but seems like you don't have to call function(x) if the function you want to use only requires one argument, like glimpse(). So that would just be map(contribution_data, glimpse))...
ind_donors <- map(contribution_data, 
                  function(x) select_ind_donors(df = x,
                                                vars = select_vars,
                                                donor_type = TRUE))

# The result is a new list: now each slot is the subsetted tibble. We can check this
glimpse(ind_donors[[1]])

# Remove from the environment so we can do even better
rm(ind_donors)

# ------------------------------------------------------------------------ #
# .......... best (our own function + iteration + single df) .......... ####
# ------------------------------------------------------------------------ #

# We can make our resulting object even more useable if we produce a single dataframe/tibble vs. a list. We can do that with a pipe, where we pass the output of our function - a list with four individual tibbles - to bind_rows(), which will combine everything in the list into a single dataframe/tibble. bind_rows() is similar to rbind(), but it handles mismatches better
ind_donors <- map(contribution_data, 
                  function(x) select_ind_donors(df = x,
                                                vars = select_vars,
                                                donor_type = TRUE)) %>% 
                  bind_rows()

# Check that it worked and the result makes sense
glimpse(ind_donors)
