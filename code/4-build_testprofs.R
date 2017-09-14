# Cole Tanigawa-Lau
# Wed Jul 19 13:05:05 2017
# Description: Build ethnicity profiles of fake polling stations to use for testing
# and the names to match those profiles.

library(data.table)
library(dplyr)
library(stringr)
source("code/functions.R")

set.seed(10)

prof_prop <- 
  matrix(NA_real_, nrow = 5, ncol = 6,
       dimnames = list(1:5, c("KIKUYU", "KALENJIN", "LUO", 
                               "SOMALI", "KAMBA", "LUHYA")
                       )
       )

prof_prop[1, ] <- c(0.8, 0.15, 0.03, 0.02, 0.0, 0.0)
prof_prop[2, ] <- c(0.5, 0.1, 0.1, 0.1, 0.1, 0.1)
prof_prop[3, ] <- 0.2
prof_prop[4, ] <- c(0.15, 0.8, 0, 0, .03, .02)
prof_prop[5, ] <- c(0.1, 0.5, 0.1, 0.1, 0.1, 0.1)

vr_yrs <- c("vr07sd", "vr12sd")
conds_clean <- c("fullname", "name", "rm_bnames")
targ_clean <- conds_clean
est_fun <- c("nameEst", "nameEstW4")
profile <- split(prof_prop, rownames(prof_prop))

comb_args <- 
  expand.grid(vr_yr = vr_yrs, conds_clean = conds_clean,
              targ_clean = targ_clean,
              targ_yr = gsub("vr", "targ", vr_yrs),
              est_fun = est_fun, profile = profile,
              stringsAsFactors = FALSE)

saveRDS(comb_args, "data/test/comb_args.rds")

# Build false conds
prof_count <- data.table(prof_prop * 1000)

conds_fns <- list.files("data/final_conds", full = TRUE)
conds_list <- lapply(conds_fns,
                     readRDS)

conds_list2 <- lapply(conds_list,
                      function(conds) {
                        rownames(conds) <- gsub(pattern = 'Á', fixed = T, x = rownames(conds), replacement = 'A')
                        rownames(conds) <- gsub(pattern = '`', fixed = T, x = rownames(conds), replacement = '')
                        rownames(conds) <- gsub(pattern = ';', fixed = T, x = rownames(conds), replacement = '')
                        rownames(conds) <- gsub(pattern = 'Y', fixed = T, x = rownames(conds), replacement = 'Y')
                        rownames(conds) <- gsub(pattern = 'Ú', fixed = T, x = rownames(conds), replacement = 'U')
                        rownames(conds) <- gsub(pattern = '\\', fixed = T, x = rownames(conds), replacement = '')
                        rownames(conds) <- gsub(pattern = '_', fixed = T, x = rownames(conds), replacement = '')
                        rownames(conds) <- gsub(pattern = '<', fixed = T, x = rownames(conds), replacement = '')
                        rownames(conds) <- gsub(pattern = '=', fixed = T, x = rownames(conds), replacement = '')
                        rownames(conds) <- gsub(pattern = '[[:punct:]]', x = rownames(conds), replacement = '')
                        
                        return(conds)
                      }
)


vsample <- Vectorize(sample, vectorize.args = "size")
# vsample(rownames(tconds), size = prof_count[[teth]], replace = TRUE, prob = tconds[, teth])

names_list <- list()
for(ii in 1:length(conds_list2)) {
  tconds <- conds_list2[[ii]]
  
  tl <- 
    lapply(names(prof_count),
           function(teth) {
             vsample(rownames(tconds), size = prof_count[[teth]],
                     replace = TRUE, prob = tconds[ , teth]
             )
             }
           )
  names(tl) <- names(prof_count)
  
  tnames_dt <- data.table(do.call(cbind, tl))
  
  names_list[[ii]] <- tnames_dt
}

names(names_list) <- 
  str_extract(conds_fns, "(?<=/conds).*(?=.rds)") %>%
  paste0("names", .)

saveRDS(names_list,
        "data/test/false_names.rds")
prof_count[ , prof_id := paste0("prof_", 1:5)]

prof_prop <- setDT(as.data.frame(prof_prop))[]
prof_prop[ , prof_id := paste0("prof_", 1:5)]
saveRDS(prof_prop, "data/test/prof_prop.rds")