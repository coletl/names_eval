rm(list = ls())
gc()

# Cole Tanigawa-Lau
# Sun Jul 16 10:21:19 2017
# Description: Prepare name strings from 2007 and late-2012 voter registers.

library(data.table)
library(stringr)
library(dplyr)
library(multidplyr)
library(parallel)
library(babynames)

source("code/functions.R")

# Clean up 2012 name strings with various methods ----
vr12 <- readRDS("data/vr12.rds")

# Concatenate common US baby names to remove when cleaning
# (these names won't be informative)
# Retain common Muslim, which will be helpful in guessing ethnicity
comm_muslim <- c(agrep("mohammed", x = babynames$name, ignore.case = TRUE, value = TRUE),
                 agrep("abdul", x = babynames$name, ignore.case = TRUE, value = TRUE),
                 agrep("hussein", x = babynames$name, ignore.case = TRUE, value = TRUE),
                 agrep("fatima", x = babynames$name, ignore.case = TRUE, value = TRUE),
                 agrep("sumaiya", x = babynames$name, ignore.case = TRUE, value = TRUE),
                 "Ayesha", "Aisha", "Farah", "Omar", "Issa", "Hamza", "Ali", "Ibrahim") %>% unique()
# Retains roughly 1.7% of unique names, about 78% of the original amount of occurences
bnames <- filter(babynames, 
                 prop > 6e-4, 
                 ! name %in% comm_muslim
                 )$name %>%
            unique() %>% toupper()

# Intialize cluster for parallel processing, split across polling station IDs ----
vr12p <- partition(vr12, psid12)
cluster_copy(vr12p, clean_func)
cluster_copy(vr12p, bnames)
cluster_library(vr12p, "dplyr")

# Clean first, middle, and last names first, then paste together later
vr12p2 <- group_by(vr12p, psid12) %>%
            do(first     = clean_func(.$FIRST_NAME) %>% str_split("\\s") %>% unlist(),
               middle    = clean_func(.$MIDDLE_NAME) %>% str_split("\\s") %>% unlist(),
               last      = clean_func(.$SURNAME) %>% str_split("\\s") %>% unlist()
               )
# Build vector of (split) names according to polling station
vr12p3 <- group_by(vr12p2, psid12) %>%
            do(.,
               first     = unlist(.$first),
               middle    = unlist(.$middle),
               last      = unlist(.$last),
               fullname  = unlist(c(.$first, .$middle, .$last)),
               name      = unlist(c(.$middle, .$last))
               )
# Remove baby names
vr12p4 <- group_by(vr12p3, psid12) %>%
           do(.,
              first     = unlist(.$first),
              middle    = unlist(.$middle),
              last      = unlist(.$last),
              fullname  = unlist(.$fullname),
              name      = unlist(.$name),
              rm_bnames = unlist(.$fullname)[which(! unlist(.$fullname) %in% bnames)]
              )
# Collect all 2012 names in a single tibble and save -----
vr12_names <- data.table(collect(vr12p4))
# Apply clean_func() once more to all columns, just in case
for(jj in 2:ncol(vr12_names)) set(vr12_names, j = jj, value = mclapply(vr12_names[[jj]], clean_func, mc.cores = 6))

saveRDS(vr12_names, "data/vr12_names.rds")
# vr12_names <- readRDS("data/vr12_names.rds")

# Clear up some memory
rm(vr12, vr12p, vr12p2, vr12p3, vr12p4)
gc()

# Clean 2007 name strings -----
vr07 <- readRDS("data/VR07dt.Rdata")
vr07p <- partition(vr07, id07)
cluster_copy(vr07p, clean_func)
cluster_copy(vr07p, bnames)
cluster_library(vr07p, "dplyr")
cluster_library(vr07p, "stringr")

# Clean first, middle, and last names first, then paste together later
vr07p2 <- group_by(vr07p, id07) %>%
  do(first     = str_extract(clean_func(.$oth), "^[A-Z]+") %>% unlist(),
     middle    = gsub(clean_func(.$oth), patt = "^[A-Z]+ ", 
                      repl = "", perl = TRUE
                      ) %>% 
                        str_split("\\s") %>% unlist(),
     last      = clean_func(.$sur) %>% str_split("\\s") %>% unlist()
     )
# Build vector of (split) names according to polling station
vr07p3 <- group_by(vr07p2, id07) %>%
  do(.,
     first     = unlist(.$first),
     middle    = unlist(.$middle),
     last      = unlist(.$last),
     fullname  = unlist(c(.$first, .$middle, .$last)),
     name      = unlist(c(.$middle, .$last)
                        )
     )
# Remove baby names
vr07p4 <- group_by(vr07p3, id07) %>%
  do(.,
     first     = unlist(.$first),
     middle    = unlist(.$middle),
     last      = unlist(.$last),
     fullname  = unlist(.$fullname),
     name      = unlist(.$name),
     rm_bnames = unlist(.$fullname)[which(! unlist(.$fullname) %in% bnames)]
     )

# Collect names and save
vr07_names <- collect(vr07p4)
# Apply clean_func() once more to all columns, just in case
system.time(for(jj in 2:ncol(vr07_names)) set(vr07_names, j = jj, value = mclapply(vr07_names[[jj]], clean_func, mc.cores = 6)))

saveRDS(vr07_names, "data/vr07_names.rds")