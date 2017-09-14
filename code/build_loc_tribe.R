rm(list = ls())
gc()

# Cole Tanigawa-Lau
# Thu Jul 13 09:59:57 2017
# Description: Build data.table of counts of each ethnic group per LOCATION
# from 1999 census data.

library(data.table)

sl_tribes <- readRDS("data/sl_tribe99.rds")
sl_tribes[ , loc_id := gsub("[0-9]{2}$", rep = "", x = sl_id)]

# Aggregate to location!
cn <- colnames(sl_tribes)
loc_tribes <- sl_tribes[ , 
                         lapply(.SD, sum),
                         by = "loc_id",
                         .SDcols = cn[2:53]
                         ]

# Calculate proportion of each ethnic group at each polling station
loc_tprop <- loc_tribes[, -1]/rowSums(as.matrix(loc_tribes[ , -1]))
# Figure out which is the plurality ethnic group
v1 <- max.col(loc_tprop)
v2 <- colnames(loc_tprop)[v1]

# Find row-wise max proportion
loc_tprop[ , `:=`(max_prop = do.call(pmax, loc_tprop),
                  plur_eth = v2,
                  loc_id = as.numeric(loc_tribes$loc_id)
                  )
           ]
# Save object with ethnicity proportions
saveRDS(loc_tprop, "data/loc_tribe_prop.rds")