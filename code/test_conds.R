rm(list = ls())

library(sp)
library(data.table)
library(dplyr)
library(stringr)
library(parallel)

source("~/shared_projects/fires/code/functions.R")
vr07 <- readRDS("data/VR07dt.Rdata")
# vr12 <- readRDS("data/VR2012dt.Rdata")
ps <- readRDS("~/shared_projects/ps/data/ps.rds")

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
names(conds_list2) <- str_extract(conds_fns, "(?<=final_conds/).*(?=.rds)")

ps07 <- ps[!is.na(ps$psid07), ] %>% as.data.frame() %>% data.table()
ps07b <- ps07[ps07$dist07 == "NAIROBI", 
              .(name = name07, psid = psid07, const = const07, constid = constid07)]

vr07b <- vr07[id07 %in% ps07b$psid
              ][ , name := paste(gsub(oth, patt = "^[A-Z]+ ", repl = "", perl = TRUE),
                                 sur)
                 ]
rm(vr07)
setkey(vr07b, id07)

vr07b[ , name := clean_func2(name)]

psid07 <- unique(ps07b$psid)
save_dir07 <- "data/tmp_matrices/2007"
dir.create("data/tmp_matrices")
dir.create(save_dir07)

for(ii in 1:length(conds_list2)){
invisible(
  mclapply(psid07,
           function(x) grouped_name_est(DT = vr07b[id07 == x], conds = conds_list2[[ii]],
                                        name_col = "name", est_fun = nameEstW4, 
                                        dir_str = save_dir07, psid_col = "id07"),
           mc.cores = 10
  )
)

eth07 <- mclapply(list.files(save_dir07, full = TRUE),
                  fread, mc.cores = 12)
names(eth07) <- gsub(".csv", "", list.files(save_dir07), fixed = TRUE)
df_eth07 <- rbindlist(eth07, idcol = "psid07")

tfn <- paste0("data/test/", names(conds_list2[ii]), ".rds")
saveRDS(df_eth07, tfn)

gc()
}

no_rv <- vr07b[ , .(no_rv = .N), by = id07]
no_rv[ ,
       `:=`(psid07 = gsub("/", "-", id07, fixed = TRUE),
            id07 = NULL)
       ]

fv <- list.files("data/test/", full = TRUE)
eth_est <- lapply(fv, readRDS)

cn <- colnames(eth_est[[1]])[-1]
eth_est2 <- lapply(eth_est,
                   function(dt) { 
                     dt2 <- left_join(dt, no_rv) 
                     for(col in cn) set(dt2, j = col, value = dt2[[col]]/dt2[["no_rv"]])
                     
                     return(data.table(dt2))
                     }
                   )

eth_est3 <- lapply(eth_est2,
                   function(dt) {
                     dt2 <- 
                       dt[ , 
                           lapply(.SD, mean),
                          .SDcols = cn, 
                          by = .(psid07)
                          ]
                     return(dt2)
                   }
                   )

ps07c <- ps07[ps07$dist07 == "NAIROBI", 
              .(name = name07, psid07 = gsub("/", "-", psid07, fixed = TRUE), 
                const = const07, constid = constid07, ward10)
              ]
eth_est4 <- lapply(eth_est3,
                   function(dt) dt[ps07c, on = "psid07"])

names(eth_est4) <- str_extract(fv, "(?<=/)conds.*(?=.rds)")
View(eth_est4[[1]])
# est07_summ <- summarize_ethcol(col = df_eth07$eth07, fun = "mean")
# est07_summ2 <- data.table(psid = unique(vr12b$psid), est13_summ)
# ps13c <- data.table(inner_join(ps13b, est13_summ2))[ , psid := NULL]
# # ps13c[ , .(KALENJIN, KAMBA, KIKUYU, KISII, LUHYA, LUO, MASAI, MERU, MIJIKENDA, POKOT, SOMALI, TURKANA)
# #        ] %>% rowSums
# fwrite(ps13c, "/Volumes/shared_projects/nairobi_name_est13/ethnic_est_nairobi_2013.csv")