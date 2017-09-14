# Cole Tanigawa-Lau
# Sun Jun  4 12:39:42 2017
# Description: Functions for conds project.

read_tribe_no <- function(tribe_file) {
  if(!require(stringr)) stop("read_trib_no requires stringr")
  if(!require(data.table)) stop("read_trib_no requires data.table")
  
  # Extract sublocation ids and population counts
  chr <- read_lines(tribe_file)
  chr2 <- str_extract(chr, "[0-9]*(?=</font>)")
  
  m1 <- matrix(na.omit(chr2), byrow = TRUE, ncol = 2)[-1, ]
  
  dt <- data.table(m1)
  setnames(dt, c("sl_id", "n"))
  
  return(dt)
}

# Revised version of Andy's name cleaning function
clean_func <- function(x){
  if(!require(stringr)) stop("clean_func requires the stringr package")
  
  x2 <- toupper(x)
  x3 <-  gsub(x2, pattern = "0", replacement = "O", fixed = T)
  x4 <-  gsub(x3, pattern = "[0-9[:punct:]]+", replacement = "", perl = TRUE)
  x5 <-  gsub(x4, pattern = "(?<=\\b)[A-Z](?=\\b)", replacement = "", perl = TRUE)
  x6 <-  gsub(x5, pattern = "[ÁÂª]+", replacement = "A", perl = TRUE)
  x7 <-  gsub(x6, pattern = "É+", replacement = "E", perl = TRUE)
  x8 <-  gsub(x7, pattern = "Í+", replacement = "I", perl = TRUE)
  x9 <-  gsub(x8, pattern = "Ó+", replacement = "O", perl = TRUE)
  x10 <- gsub(x9, pattern = "Ú+", replacement = "U", perl = TRUE)
  x11 <- gsub(x10, pattern = "Ÿ+", replacement = "Y", perl = TRUE)
  x12 <- gsub(x11, pattern = "´+", replacement = "", perl = TRUE)
  x13 <- str_trim(x12)
  names(x13) <- names(x)
  
  return(x13[which(x13 != "")])
}

# Sorts by descending sort_col and finds the point at which val_col meets min_val.
# Used to find the proportion of an ethnicity (sort_col) that will provide 
# a sufficient (min_val) number of names (val_col) for the conditional probabilities.
find_thresh <- 
  function(df, sort_col, val_col, min_val) {
    if(!require(data.table)) stop("find_thresh depends on data.table's fast ordering")
    
    
    if(!is.data.table(df)) df <- data.table(df)
    setorderv(df, sort_col, order = -1L)
    v1 <- cumsum(df[[val_col]])
    
    df[[sort_col]][which(v1 >= min_val)[1]]
    
  }

save_list <- 
  function(x, dir, ext = ".rds", names = NULL, 
           cores = 1, ...){
    if(!file.exists(dir)) dir.create(dir)
    
    if(is.null(names)) names <- names(x) else names(x) <- names
    require(parallel)
    
    dir2 <- ifelse(grepl(dir, patt = "/$"), dir, paste0(dir, "/"))
    
    if(grepl(ext, patt = "\\.?rds$", ignore.case = TRUE)){
      mclapply(names, function(i) saveRDS(x[[i]],
                                          paste0(dir2, i, ext),
                                          ...),
               mc.cores = cores
      )
    }
    
    if(grepl(ext, patt = "\\.?csv$", ignore.case = TRUE)){
      if(require(data.table)){
        mclapply(names, function(i) fwrite(x[[i]],
                                           file = paste0(dir2, i, ext),
                                           ...),
                 mc.cores = cores
        )
      }else{
        mclapply(names, function(i) write.csv(x[[i]],
                                              file = paste0(dir2, i, ext),
                                              ...),
                 mc.cores = cores
        )
      }
    }
    
    if(grepl(ext, patt = "\\.?shp$", ignore.case = TRUE)){
      if(require(maptools)){
        mclapply(names, function(i) writeSpatialShape(x[[i]],
                                                      paste0(dir2, i, ext),
                                                      ...),
                 mc.cores = cores
        )
      }else{ stop("Package 'maptools' must be installed to save spatial objects") }
    }
  }

ent.func2 <- function(x){
  xx <- x/rowSums(x)
  v2 <- -rowSums(xx * log(xx + 1e-16))
  
  return(v2)
}

nameEst <- function(cond, targ){
  if(!require(quadprog)) stop("Name estimation requires the quadprog package")
  if(!require(corpcor)) stop("Name estimation requires the corpcor package")
  
  Y <- targ
  X <- cond
  Yt <- Y
  orderind <- intersect(names(Yt), rownames(X))
  Yt <- Yt[orderind]
  X <- X[orderind, ]
  Yt <- Yt[row.names(X)]
  designmat <- model.matrix(Yt ~ -1 + X)
  Dmat <- crossprod(designmat, designmat)
  pdind <- is.positive.definite(Dmat)
  if(pdind == FALSE){Dmat <- make.positive.definite(Dmat)}
  dvec <- crossprod(designmat, Yt)
  #Amat <- cbind(1, diag(nrow(Dmat)))
  #bvec <- c(1, rep(0, nrow(Dmat)))
  Amat <- cbind(
    matrix(1, nr=length(dvec), nc=1),
    diag(length(dvec)),
    -diag(length(dvec))
  )
  bvec <- c(1, rep(0, length(dvec)), rep(-1, length(dvec)))
  meq <- 1
  resTmp <- tryCatch(expr = solve.QP(Dmat, dvec, Amat, bvec, meq, factorized = F)[[1]], error = function(e) rep(NA, ncol(X)))
  out <- round(resTmp, 5)
  names(out) <- colnames(X)
  
  return(out)
}

nameEstW4 <- function(cond, targ){
  if(!require(quadprog)) stop("Name estimation requires the quadprog package")
  if(!require(corpcor)) stop("Name estimation requires the corpcor package")
  
  Y <- targ
  X <- cond
  Yt <- Y
  
  orderind <- intersect(names(Yt), rownames(X))
  Yt <- Yt[orderind]
  X <- X[orderind, ]
  dat <- data.frame(targ = Yt, X)
  
  w <- ent.func2(X)
  Yt <- Yt[row.names(X)]	
  ws <- diag(1/exp(w))
  
  Dmat <- t(X) %*% ws %*% X
  dvec <- t(Yt)%*% ws %*% X
  
  pdind <- is.positive.definite(Dmat)
  
  if(pdind == FALSE){Dmat <- make.positive.definite(Dmat)}
  
  Amat <- cbind(
    matrix(1, nr=length(dvec), nc=1),
    diag(length(dvec)),
    -diag(length(dvec))
  )
  resTmp <- tryCatch(expr = solve.QP( Dmat = Dmat, dvec = dvec, Amat = Amat, 
                                      bvec = c(1, rep(0, length(dvec)), rep(-1, length(dvec))),
                                      meq = 1)[[1]], error = function(e) rep(NA, ncol(X)))
  out <- round(resTmp, 5)
  names(out) <- colnames(X)
  
  return(out)
}

TRIAL_name_est <- function(namesv, conds,
                            est_fun = c(nameEst, nameEstW, 
                                        nameEstW3, nameEstW4),
                            dir_str
                            ) {
  
  tnm <- unlist(namesv)
  tnm2 <- tnm[which(tnm %in% row.names(conds))]
  
  res_mat <- matrix(NA, ncol = ncol(conds), nrow = 25)
  colnames(res_mat) <- colnames(conds)
  
  for(jj in 1:nrow(res_mat)){
    tab_tnm <- prop.table(table(sample(tnm2, length(tnm2), replace = T)))
    # tab_tnm <- prop.table(table(tnm2))
    
    res_mat[jj, ] <- tryCatch({
      
      est_fun(cond = conds, 
              targ = tab_tnm
      )
      
    },
    error = function(e) rep(NA_real_, ncol(conds)) 
    )
    
  }
  
  if(!require(data.table)) {
    res_df <- data.frame(res_mat)
  } else {
    res_df <- data.table(res_mat)  
  }

  return(res_df)
}
