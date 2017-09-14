rm(list = ls())
gc()

# Cole Tanigawa-Lau
# Thu Jul 20 15:19:18 2017
# Description: Model the results of trials run in script 5, looking at differences in error rates
# across preprocessing steps and ethnicities.
# RESULTS IN PDF HERE: https://www.sharelatex.com/project/59705d0c5009a94c3c0157e2

library(dplyr)
library(stringr)
library(data.table)
library(parallel)
library(stargazer)
library(matrixStats)

comb_args <- readRDS("data/test/comb_args.rds")
prof_prop <- readRDS("data/test/prof_prop.rds")


vfn <- list.files("data/test", full = TRUE, patt = "[0-9].rds")
lf <- lapply(vfn, readRDS)
lf_order <- str_extract(vfn, "[0-9]+")
# Match the results to the combination of arguments that produced them
comb_tags <- apply(as.matrix(comb_args[ , 1:5]), 1, paste, collapse = "_")
names(lf) <- comb_tags[as.numeric(lf_order)]

results_dt <- rbindlist(lf, idcol = "comb_tag")
results2 <- results_dt[ ,
                        lapply(.SD, mean),
                        by = .(comb_tag, prof_id)
                        ]

# Some ethnicities were set to zero in all profiles
zero_prof <- c("KISII", "MASAI", "MERU", "MIJIKENDA", "POKOT", "TURKANA")
prof_prop[ , 
           (zero_prof) := 0
           ]

# Calculate outcome variables ----
# Ethnicity columns we'll be working with
cn <- c(names(prof_prop)[1:6], zero_prof)
results2[prof_prop,
         # Calculate difference for all ethnicities
         (cn) := setDT(mget(cn)) - mget(paste0('i.', cn)),
         on = .(prof_id)
         ][ , 
            # Square differences
            (cn) := lapply(.SD, `^`, 2),
            .SDcols = cn
            ]
# Split combination tags into separate columns to use as regression parameters
results2[ ,
          (names(comb_args)[1:5]) := as.data.table(str_split_fixed(comb_tag, "(?<!rm)_", n = 5))
          ]
# Summarize error rates across ethnicities
results2[ ,
          `:=`(err2_tot = rowSums(.SD),
               err2_max = rowMaxs(as.matrix(.SD)),
               err2_min = rowMins(as.matrix(.SD))
          ),
          .SDcols = cn
          ]

ivs <- paste(names(comb_args[1:5]), collapse = " + ") %>%
  paste("vr_yr*targ_yr", "conds_clean*targ_clean", sep = " + ")
form  <- as.formula(paste0("err2_tot ~ ", ivs))
form2 <- as.formula(paste0("err2_max ~ ", ivs))
form3 <- as.formula(paste0("err2_min ~ ", ivs))

regs <- group_by(results2, prof_id) %>% 
  do(., 
     model = lm(form, data = .))

stargazer(list(regs$model, lm(form, results2)), style = "default",
          covariate.labels = c("cond year == 2012", "cond cleaning == partial name", "cond cleaning == rm_bnames",
                               "ps cleaning == partial name", "ps cleaning == rm_bnames", "ps yr == 2012", "nameEstW4",
                               "cond year == 2012 * ps yr == 2012", "C clean == part.name * PS clean == part.name", 
                               "C clean == rmb * PS clean == rmb", "C clean == part.name * PS clean == rmb",
                               "C clean == rmb * PS clean == rmb")
)

regs2 <- group_by(results2, prof_id) %>% 
  do(., 
     model = lm(form2, data = .))

stargazer(list(regs2$model, lm(form2, results2)), style = "default",
          covariate.labels = c("cond year == 2012", "cond cleaning == partial name", "cond cleaning == rm_bnames",
                               "ps cleaning == partial name", "ps cleaning == rm_bnames", "ps yr == 2012", "nameEstW4",
                               "cond year == 2012 * ps yr == 2012", "C clean == part.name * PS clean == part.name", 
                               "C clean == rmb * PS clean == rmb", "C clean == part.name * PS clean == rmb",
                               "C clean == rmb * PS clean == rmb")
)

regs3 <- group_by(results2, prof_id) %>% 
  do(., 
     model = lm(form3, data = .))

stargazer(list(regs3$model, lm(form3, results2)), style = "default",
          covariate.labels = c("cond year == 2012", "cond cleaning == partial name", "cond cleaning == rm_bnames",
                               "ps cleaning == partial name", "ps cleaning == rm_bnames", "ps yr == 2012", "nameEstW4",
                               "cond year == 2012 * ps yr == 2012", "C clean == part.name * PS clean == part.name", 
                               "C clean == rmb * PS clean == rmb", "C clean == part.name * PS clean == rmb",
                               "C clean == rmb * PS clean == rmb")
)

# Using the uniformly distributed profile, model error rates for each ethnicity
vethform  <- sapply(cn, function(eth) as.formula(paste0(eth, " ~ ", ivs)))
lethmods <- lapply(vethform,
                   function(x) lm(x, data = results2)
)
names(lethmods) <- cn
stargazer(lethmods, style = "default",
          covariate.labels = c("cond year == 2012", "cond cleaning == partial name", "cond cleaning == rm_bnames",
                               "ps cleaning == partial name", "ps cleaning == rm_bnames", "ps yr == 2012", "nameEstW4",
                               "cond year == 2012 * ps yr == 2012", "C clean == part.name * PS clean == part.name", 
                               "C clean == rmb * PS clean == rmb", "C clean == part.name * PS clean == rmb",
                               "C clean == rmb * PS clean == rmb")
)
