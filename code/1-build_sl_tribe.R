rm(list = ls())
gc()

# Cole Tanigawa-Lau
# Thu Jul 13 09:59:57 2017
# Description: Build data.table of counts of each ethnic group per sublocation 
# from 1999 census data.

library(readr)
library(stringr)
library(data.table)
library(parallel)

source("code/functions.R")

# Read in files
tribe_key <- fread("data/tribekey99.csv", colClasses = c("ch", "ch"))

files <- list.files("data/tribe", full = TRUE)
tribes <- mclapply(files, read_tribe_no, mc.cores = 2)
# Name files after tribe key
names(tribes) <- str_extract(files, "(?<=/tribe)[0-9]+")

# Bind together, using file name (tribe key) as an id column
tribes_dt <- rbindlist(tribes, idcol = "code")
# Join to add tribe name
tribes_dt[tribe_key, 
          group := toupper(group), 
          on = "code"
          ]

# Reshape data.table from wide to long, keeping counts as values
sl_tribes <- dcast(tribes_dt, sl_id ~ group, value.var = "n")
# Change counts to integers
cn <- colnames(sl_tribes)[-1]
sl_tribes[ , 
           (cn) := lapply(.SD, as.integer),
           .SDcols = cn
           ]
# Combine ethnic sub-groups
kalenjin <- c("KEIYO", "NANDI", "MARAKWET", "TUGEN", "KIPSIGIS", "KALENJIN_STATED")
somali <- c("OGADEN", "DEGODIA", "HAWIYAH", "GURREH", "SOMALI_SO_STATED")
sl_tribes[ , `:=`(KALENJIN = rowSums(.SD[ , kalenjin, with = FALSE]),
                  SOMALI = rowSums(.SD[ , somali, with = FALSE])
                  )
           ]
sl_tribes2 <- 
  sl_tribes[ , 
             !kalenjin, 
             with = FALSE
             ][ , !somali,
                with = FALSE
                ]

# Save object with ethnicity counts
saveRDS(sl_tribes2, "data/sl_tribe99.rds")

# Calculate proportion of each ethnic group at each polling station
sl_tprop <- sl_tribes2[, -1]/rowSums(as.matrix(sl_tribes2[ , -1]))
# Figure out which is the plurality ethnic group
v1 <- max.col(sl_tprop)
v2 <- colnames(sl_tprop)[v1]

# Find row-wise max proportion
sl_tprop[ , `:=`(max_prop = do.call(pmax, sl_tprop),
                 plur_eth = v2,
                 sl_id = as.numeric(sl_tribes$sl_id)
                 )
          ]
# Save object with ethnicity proportions
saveRDS(sl_tprop, "data/sl_tribe_prop.rds")