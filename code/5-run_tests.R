rm(list = ls())
gc()

# Cole Tanigawa-Lau
# Wed Jul 19 16:41:56 2017
# Description: Run tests on combinations of profiles, cleaning methods, and name estimation functions.

library(data.table)
library(stringr)
library(readr)
library(parallel)

source("code/functions.R")

comb_args <- readRDS("data/test/comb_args.rds")
false_names <- readRDS("data/test/false_names.rds")

conds_fns <- list.files("data/final_conds", full = TRUE)
false_conds <- lapply(conds_fns,
                     readRDS)
names(false_conds) <- 
  str_extract(conds_fns, "(?<=/)conds.*(?=.rds)")

save_dir <- "data/tmp_matrices/"

set.seed(10)

# Takes about 15 minutes.
system.time({
for(ii in 1:nrow(comb_args)){
  # We're indexing 360 combinations of function arguments: 
  # 2 x conds years, 2 x ps years, 3 x conds cleaning methods, 3 x ps cleaning methods
  # 2 x nameEst functions
  targs <- comb_args[ii, ]
  
  # Specify arguments to pass to TRIAL_name_est()
  vr_yr <- str_extract(targs$vr_yr, "[0-9]{2}")
  targ_yr <- str_extract(targs$targ_yr, "[0-9]{2}")
  
  conds_clean <- paste0("conds", vr_yr, "_", targs[["conds_clean"]])
  names_clean <- paste0("names", targ_yr, "_", targs[["targ_clean"]])
  
  # Here, we're running the five profiles (i.e., user-defined ethnicity distributions) in parallel
  ethl <- mclapply(1:5,
             function(profile_no) TRIAL_name_est(namesv  = false_names[[names_clean]][profile_no, ], 
                                                 conds   = false_conds[[conds_clean]], 
                                                 est_fun = get(targs$est_fun), 
                                                 dir_str = save_dir),
             mc.cores = 5
    )
  names(ethl) <- paste0("prof_", 1:5)
  df_eth <- rbindlist(ethl, idcol = "prof_id")
  
  # Save as an R object identifying (by row number) which combination of arguments were used.
  tfn <- paste0("data/test/", ii, ".rds")
  saveRDS(df_eth, tfn)
  
  gc()
  
  if(ii %% 10 == 0) print(ii)  
}
})