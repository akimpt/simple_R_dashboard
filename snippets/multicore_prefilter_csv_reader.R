library(tidyverse)
library(furrr)

#https://stackoverflow.com/questions/59482431/how-to-filter-a-very-large-csv-in-r-prior-to-opening-it

# Make a CSV file out of the NASA stock dataset for demo purposes
raw_data_path <- tempfile(fileext = ".csv")
GGally::nasa	 %>% as_tibble() %>% write_csv(raw_data_path)

# Get the row count of the raw data, incl. header row, without loading the
# actual data
raw_data_nrow <- length(count.fields(raw_data_path))

# Hard-code the largest batch size you can, given your RAM in relation to the
# data size per row
batch_size    <- 1e3

# Set up parallel processing of multiple chunks at a time, leaving one virtual
# core, as usual
plan(multicore, workers = availableCores() - 1)

filtered_data <-
  # Define the sequence of start-point row numbers for each chunk (each number
  # is actually the start point minus 1 since we're using the seq. no. as the
  # no. of rows to skip)
  seq(from = 0,
      # Add the batch size to ensure that the last chunk is large enough to grab
      # all the remainder rows
      to = raw_data_nrow + batch_size,
      by = batch_size) %>%
  future_map_dfr(
    ~ read_csv(
      raw_data_path,
      skip      = .x,
      n_max     = batch_size,
      # Can't read in col. names in each chunk since they're only present in the
      # 1st chunk
      col_names = FALSE,
      # This reads in each column as character, which is safest but slowest and
      # most memory-intensive. If you're sure that each batch will contain
      # enough values in each column so that the type detection in each batch
      # will come to the same conclusions, then comment this out and leave just
      # the guess_max
      col_types = cols(.default = "c"),
      guess_max = batch_size
    ) %>%
      # This is where you'd insert your filter condition(s)
      filter(TRUE),
    # Progress bar! So you know how many chunks you have left to go
    .progress = TRUE
  ) %>%
  # The first row will be the header values, so set the column names to equal
  # that first row, and then drop it
  set_names(slice(., 1)) %>%
  slice(-1)