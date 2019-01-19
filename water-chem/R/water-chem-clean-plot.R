# Water Chem Extract, Transform, Load, Plot

# this helps install packages if missing (feel free to ignore) ----------
pkgs <- c('tidyverse', 'readxl', 'here', 'plotly')
for(i in pkgs) {
  if(!require(i, character.only = TRUE)) {
    invisible(install.packages(i))
  }
}


# (0) setup ---------------------------------------------------------------
library(tidyverse) # all purpose tools we will use
library(readxl) # specific for loading excel files
library(here) # used to efficiently pathing files
library(plotly)

# !!! NOTE: i decided to specifically use the `package::some_function()` style
#   e.g. `tidyr::gather()` vs `gather()` to learn where functions come from.
#   i think this can be scary and sometimes hard to read. i initially didnt do it. 
#   but i think its important to know where functions come from.
# !!! i recommend going to "Preferences > Code > Display" and 
#   click the box "Highlight R function calls" to help see packages and functions


# `pattern = '.*xlsx'` will search for files with xlsx in the name
# `full.names = TRUE` gives full path to file for easy reading
data_raw_dir <- here::here('data-raw') # where the raw data is
files <- list.files(data_raw_dir, pattern = '.*xlsx', full.names = TRUE) # find files
files # look at files. only one file in this case.
dat <- files %>% # iterate over files
  purrr::map(~{
    .x %>% # each element in `files` is mapped to .x
    readxl::read_xlsx() %>% # read .xlsx file with read_xlsx()
    # replace any one or more spaces or hyphens in a row with a single underscore "_"
    dplyr::rename_all(~{ # note that we wrap code in ~{}
      stringr::str_replace_all(., '[ ]+|-+', '_') %>% #
      tolower()
    }) %>%
    dplyr::rename_all(~{
      # !!! regular expressions (regex) are confusing and complicated and a whole
      #   beast all of their own. don't feel bad if you don't get them. its hard.
      # issue: column names like "cl\r\n\chloride"
      # want: "cl\r\n\chloride" -> "cl"
      # aka: cut out everything after and inlucding "\r\n.*" but keep first part
      # "(.*)\\r\\n.*" is a regex expression that says:
      #   - match to anything matching ".*\\r\\n.*"
      #   - create a group including anything "(.*)" UNTIL "\\r\\n.*"
      #   - replace entire match string with whats in first group, "\\1"
      stringr::str_replace_all(., '(.*)\\r\\n.*', '\\1')
    })
  }) %>%
  # if a list of tibbles is return, stack them (only have one here but if we have more sheets or xlsx files, this would capture and stack/row bind all of them)
  dplyr::bind_rows() %>% 
  # gather all variables *not* date_sample_collected or sample_location etc.
  # `tidyr::gather()` can also be quite hard to understand.
  # goal is to create a tibble with a (id, time) key for a set of variable and their values in "long" (versus the original "wide") format.
  # need to gather data and put in "long" format because ggplot2 much prefers such data
  tidyr::gather(variable, value, -date_sample_collected, -sample_location, -sample_id) %>%
  dplyr::arrange(sample_location, date_sample_collected) %>%
  # !!! IMPORTANT KEY problem here is that, due to how the data was formatted 
  #   in excel, what SHOULD be numerical data (by this point the `value` column)
  #   was converted to character. we have to clean it and convert to number
  dplyr::mutate(
    value = 
      # remove the '<' characters which will break converting to numbers
      stringr::str_replace_all(value, '<', '') %>%
      as.double()
  )

# (1) lets plot all the elements across avaible time -------------------
g <- dat %>%
  # take our new long data, mapping x = data, y = to value, color to variable (or elements)
  ggplot2::ggplot(data = ., mapping = ggplot2::aes(x = date_sample_collected, y = value, color = sample_location)) +
  ggplot2::geom_point(size = 2) + # add points. make a little bigger
  ggplot2::geom_line(alpha = .5, size = 1.5) + # add lines that are partially see through and increase thickness/size
  # facet_wrap() will create separate subplots along some column (this case, variable)
  # option `scale = 'free_y` lets each value per element to have its own axis
  ggplot2::facet_wrap(variable ~ ., scales = 'free_y') # subplots of `variable` vs everything else `.`
g

# quickly create interactive version of plot with plotly's ggplotly()
plotly::ggplotly(g)

# (2) plot `value`` of specific elements/molucule from `variable` e.g. "co2" and `acidity` -----
g1 <- dat %>%
  # filter only rows where `variable` is in the set `co2` or `acidity`
  dplyr::filter(variable %in% c('co2', 'acidity')) %>%
  ggplot2::ggplot(data = ., mapping = ggplot2::aes(x = date_sample_collected, y = value, color = sample_location)) +
  ggplot2::geom_point(size = 2) + 
  ggplot2::geom_line(alpha = .5, size = 1.5) + 
  ggplot2::facet_wrap(variable ~ ., scales = 'free_y')
g1

plotly::ggplotly(g1)
