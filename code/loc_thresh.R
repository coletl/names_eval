rm(list = ls())
gc()

# Cole Tanigawa-Lau
# Thu Jul 13 11:47:25 2017
# Description: Set threshold for homogenous sublocations to use for name estimation.

library(data.table)
library(dplyr)
library(sf)

# Read data ----
# vr12 <- readRDS("data/VR2012dt.Rdata")
# vr12[, CONSTITUENCY_CODE := str_pad(CONSTITUENCY_CODE, 3, pad = "0", "left")]
# vr12[, POLLING_STATION_CODE := str_pad(POLLING_STATION_CODE, 3, pad = "0", "left")]
# vr12[ , psid12 := paste0(CONSTITUENCY_CODE, "/", POLLING_STATION_CODE)]
# no_rv12 <- vr12[ , .(no_rv12 = .N), by = psid12]
# saveRDS(no_rv12, "data/no_rv12.rds")

loc_tprop <- readRDS("data/loc_tribe_prop.rds")

ps <- st_as_sf(readRDS("data/ps.rds"))

no_rv12 <- readRDS("data/no_rv12.rds")

sl <- st_read("data/ken_admin/kensublocations.shp")
# Aggregate to locations, union-ing the geometries
loc <- aggregate(sl, list(sl$LOCID), FUN = unique)
st_crs(loc) <- 4326
loc2 <- st_transform(loc, 32737)

# Extract number of registered voters for 2007 polling stations
text07 <- readLines("data/vr07.txt")
text07b <- str_extract(text07, "[0-9]{3}/[0-9]{3}.*")
psid07 <- str_extract(text07b, "[0-9]{3}/[0-9]{3}")
tot07 <- str_extract(text07b, "[0-9,]*$") %>% gsub(",", "", .) %>% as.numeric()

no_rv07 <- na.omit(data.table(psid07, no_rv07 = tot07))

# Look for an appropriate threshold ----
# Add number of registered voters to polling stations object
ps2 <- left_join(ps, no_rv07, by = "psid07") %>% 
  left_join(no_rv12, by = c(psid13 = "psid12"))
# Add proportions to sublocations sf object
loc3 <- left_join(loc2, loc_tprop, by = c(LOCID = "loc_id"))

# Calculate number of registered voters in each homogenous subloc
linter <- st_intersects(loc3, ps2)
loc3$no_rv07 <- sapply(linter, function(x) sum(ps2[x, ]$no_rv07, na.rm = TRUE))
loc3$no_rv12 <- sapply(linter, function(x) sum(ps2[x, ]$no_rv12, na.rm = TRUE))

# Subset to homogenous sublocations
test <- as.data.frame(filter(loc3, max_prop > 0.90)) %>% 
  group_by(plur_eth) %>% summarize(no_rv07 = sum(no_rv07, na.rm = TRUE),
                                   no_rv12 = sum(no_rv12, na.rm = TRUE)) %>% 
  arrange(no_rv07)
test2 <- filter(test, plur_eth %in% c("KIKUYU", "LUO", "KAMBA", "LUHYA", "KALENJIN", "MASAI", "MERU", "KISII", "SOMALI", "POKOT"))
test2$no_rv07/sum(test2$no_rv07)
test2$no_rv12/sum(test2$no_rv12)
test2$plur_eth

filter(sl_tprop, LUHYA > 0.8)$LUHYA %>% {plot(density(., na.rm = T))}
filter(sl_tprop, LUHYA > 0.8)$LUHYA %>% rug()

filter(sl_tprop, KIKUYU > 0.8)$KIKUYU %>% {plot(density(., na.rm = T))}
filter(sl_tprop, KIKUYU > 0.8)$KIKUYU %>% rug()

filter(sl_tprop, LUO > 0.8)$LUO %>% {plot(density(., na.rm = T))}
filter(sl_tprop, LUO > 0.8)$LUO %>% rug()

filter(sl_tprop, KALENJIN > 0.8)$KALENJIN %>% {plot(density(., na.rm = T))}
filter(sl_tprop, KALENJIN > 0.8)$KALENJIN %>% rug()

loc <- group_by(sl3, LOCID) %>% do(plur_eth = unique(plur_eth))