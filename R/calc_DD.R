

#TODO: what should we do when k is missing? (DD<0)
calc_DD_m <- function(tm, k, a, b, t0, beta, c){

  DD_m <- c + beta * tm
  
  gt_k <- which(tm > k)
  
  DD_m[gt_k] <- a[gt_k]/(1 + exp(-(tm[gt_k] - t0[gt_k])/b[gt_k]))
  
  DD_m
}


calc_DD_below_0 <- function(m, tm){
  
  match_lines <- match(m, param$DD_lt_0$Month)
  
  calc_DD_m(tm = tm, 
            k = param$DD_lt_0$k[match_lines],
            a = param$DD_lt_0$a[match_lines],
            b = param$DD_lt_0$b[match_lines],
            t0 = param$DD_lt_0$T0[match_lines],
            beta = param$DD_lt_0$beta[match_lines], 
            c = param$DD_lt_0$c[match_lines]
            )
}


calc_DD_above_5 <- function(m, tm, region){
  
  region[m > 4 & m < 11] <- "All"
  
  match_lines <- match(paste0(region, "_", m), 
                       paste0(param$DD_gt_5$Region, "_", param$DD_gt_5$Month))
  
  calc_DD_m(tm = tm, 
            k = param$DD_gt_5$k[match_lines],
            a = param$DD_gt_5$a[match_lines],
            b = param$DD_gt_5$b[match_lines],
            t0 = param$DD_gt_5$T0[match_lines],
            beta = param$DD_gt_5$beta[match_lines],
            c = param$DD_gt_5$c[match_lines]
            )
}

calc_DD_below_18 <- function(m, tm){
  
  match_lines <- match(m, param$DD_lt_18$Month)
  
  calc_DD_m(tm = tm, 
            k = param$DD_lt_18$k[match_lines],
            a = param$DD_lt_18$a[match_lines],
            b = param$DD_lt_18$b[match_lines],
            t0 = param$DD_lt_18$T0[match_lines],
            beta = param$DD_lt_18$beta[match_lines], 
            c = param$DD_lt_18$c[match_lines]
            )
}

calc_DD_above_18 <- function(m, tm, region){
  
  region[m > 5 & m < 9] <- "All"
  
  match_lines <- match(paste0(region, "_", m), 
                       paste0(param$DD_gt_18$Region, "_", param$DD_gt_18$Month))
  
  calc_DD_m(tm = tm, 
            k = param$DD_gt_18$k[match_lines],
            a = param$DD_gt_18$a[match_lines],
            b = param$DD_gt_18$b[match_lines],
            t0 = param$DD_gt_18$T0[match_lines],
            beta = param$DD_gt_18$beta[match_lines],
            c = param$DD_gt_18$c[match_lines]
            )
}
