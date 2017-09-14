rm(list = ls())
gc()

# Cole Tanigawa-Lau
# Mon Jul 17 11:34:42 2017
# Description: Apply varying proportion thresholds to take name strings from the 
# 2007 and late-2012 voter registers of ethnically homogenous sublocations.

library(data.table)
library(dplyr)
library(sf)
library(purrr)

source("code/functions.R")

# Read and prep data ----
names07 <- readRDS("data/vr07_names.rds")
names12 <- readRDS("data/vr12_names.rds")

ps <- st_as_sf(readRDS("data/ps.rds"))

no_rv12 <- readRDS("data/no_rv12.rds")
no_rv07 <- readRDS("data/no_rv07.rds")
# Add number of registered voters to polling stations object
ps2 <- left_join(ps, no_rv07, by = "psid07") %>% 
  left_join(no_rv12, by = c(psid13 = "psid12"))

sl <- st_read("data/ken_admin/kensublocations.shp")
st_crs(sl) <- 4326
sl2 <- st_transform(sl, 32737)

# Add ethnicity proportions to sublocations sf object
sl_ethprop <- readRDS("data/sl_tribe_prop.rds")
sl3 <- left_join(sl2, sl_ethprop, by = c(SLID = "sl_id"))

# Calculate approximate count for individuals in each sublocation's plurality ethnicity
slps <- st_intersects(sl3, ps2)
ps2_df <- as.data.frame(ps2)
sl_rv <- map_df(slps, 
                function(x) {
                  summarize(ps2_df[x, ], 
                            # Some sublocations have zero polling stations (no registered voters), 
                            # and some polling stations didn't exist in 2007 or 2012 (no rv for that year)
                            no_rv07 = sum(no_rv07, na.rm = TRUE),
                            no_rv12 = sum(no_rv12, na.rm = TRUE)
                            )
                  }
  )
sl4 <- data.table(sl3, sl_rv)
sl4[ , psrows := slps]

# Calculate proportion thresholds needed to meet four different
# requirements for the numbers of voters ----
thresh_rv <- c(125000, 100000, 75000, 50000)

thresh <- lapply(thresh_rv,
                   function(x) {
                     sl4[ , 
                          .(prop_thresh07 = find_thresh(.SD, sort_col = "max_prop", 
                                                        val_col = "no_rv07", min_val = x),
                            prop_thresh12 = find_thresh(.SD, sort_col = "max_prop", 
                                                        val_col = "no_rv12", min_val = x)
                          ),
                          by = plur_eth
                          ]
                     
                   }
                 )
names(thresh) <- thresh_rv

# Build a data.table for thresholds, keeping only ethnicities we will need to estimate
eths <- c("KALENJIN", "KAMBA", "KIKUYU", "KISII", "LUHYA", "LUO", "MASAI", "MERU", "MIJIKENDA", "POKOT", "SOMALI", "TURKANA")
threshDT <- rbindlist(thresh, 
                      idcol = "no_rv"
                      )[plur_eth %in% eths]

names07 <- data.table(names07)
setnames(names07, 1, "psid07")
setkey(names07, psid07)

names12 <- data.table(names12)
setkey(names12, psid12)

# Build empty matrices to fill
nm07_mats <- lapply(names07[ , .(fullname, name, rm_bnames)], 
                    function(x) {
  tnms <- unique(unlist(x))
  m <- matrix(ncol = length(eths), nrow = length(tnms))
  rownames(m) <- tnms
  colnames(m) <- eths
  return(m)
  }
)

nm12_mats <- lapply(names12[ , .(fullname, name, rm_bnames)], 
                    function(x) {
  tnms <- unique(unlist(x))
  m <- matrix(ncol = length(eths), nrow = length(tnms))
  rownames(m) <- tnms
  colnames(m) <- eths
  return(m)
  }
)

for(ii in 1:nrow(threshDT)) {
  t07 <- threshDT[ii, prop_thresh07]
  t12 <- threshDT[ii, prop_thresh12]
  teth <- threshDT[ii, plur_eth]
  
  vps07 <- unlist(sl4[plur_eth == teth & max_prop >= t07, psrows])
  vps12 <- unlist(sl4[plur_eth == teth & max_prop >= t12, psrows])
  
  ps07 <- na.omit(ps2$psid07[unique(vps07)])
  ps12 <- na.omit(ps2$psid13[unique(vps12)])
  
  
  tnm07 <- lapply(names07[ps07, ], unlist)
  tnm12 <- lapply(names12[ps12, ], unlist)
  
  for(jj in c("fullname", "name", "rm_bnames")) {
    tprop07 <- prop.table(table(tnm07[[jj]]))
    nm07_mats[[jj]][names(tprop07), teth] <- tprop07
    
    tprop12 <- prop.table(table(tnm12[[jj]]))
    nm12_mats[[jj]][names(tprop12), teth] <- tprop12
  }
  
  print(ii)
}
nm07_mats2 <- lapply(nm07_mats, function(x) { x[is.na(x)] <- 0; x[rowSums(x) > 0, ] })
nm12_mats2 <- lapply(nm12_mats, function(x) { x[is.na(x)] <- 0; x[rowSums(x) > 0, ] })

fn07 <- paste0("conds07_", names(nm07_mats2))
fn12 <- paste0("conds12_", names(nm12_mats2))

save_list(nm07_mats2, dir = "data/final_conds", names = fn07, cores = 3)
save_list(nm12_mats2, dir = "data/final_conds", names = fn12, cores = 3)