################################################################################
# This file will run all code needed for the analyses for Practice Effects
# Paper 1 by Ruijia Chen et al.
# Please edit lines 22-27 to point to the raw data files for KHANDLE, STAR,
# and LA90 and then source this file.
################################################################################
# load libraries
if (!require("pacman")){
  install.packages("pacman", repos='http://cran.us.r-project.org')
}
library(pacman)
p_load("here", "glue")
################################################################################
# Kaiser team: make sure this is set to "Kaiser" when running the analyses
# can also take values, "Kaitlin" and "Ruijia"
users <- "Kaiser"

# edit this line to point to directory with code
path_to_code <- "code"

# edit these lines to point to the raw data files for KHANDLE, STAR, and LA90
# dat_KHANDLE   <- "write_the_path_here"
# dat_STAR      <- "write_the_path_here"
# dat_LA90_wide <- "write_the_path_here"
# dat_LA90_long <- "write_the_path_here"
# dat_LA90_date <- "write_the_path_here"
# dat_LA90_mhc  <- "write_the_path_here"
################################################################################
# source helper functions
source(glue("{path_to_code}/kaitlin_functions.R"))

# Create folder structure for saving results
dirs <- c(
  "results",
  "results/01_data",
  "results/02_output"
)

for(dir in dirs){
  if(!dir.exists(dir)) dir.create(dir)
}

log_file_name <- glue("log_{format(Sys.time(), '%Y-%m-%dT%H%M')}.txt")
log_file <- glue("results/02_output/{log_file_name}")

files_to_run <- c(
  "01_make_df.R",
  "02_fit_models.R",
  "03_table1.R"
)

for(f in files_to_run){
  log_step(glue("Starting file: {f} \n"), log_file)
  source(glue("{path_to_code}/{f}"))
}
