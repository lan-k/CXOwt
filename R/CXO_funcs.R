#' Mantel Haenszel Odds Ratio Estimate
#'
#' @param formula formula for estimation in form Outcome ~ strata/exposure
#' @param data input dataframe
#' @param digits number of digits for rounding
#'
#' @return Dataframe with OR estimate and 95% confidence interval
#' @export
#'
#' @examples
#' data(cases)
#' mhor(Event ~ Id/ex, data=cases, digits=2)
#'

mhor <- function(formula, data, digits=2)  {
  #formula is Outcome ~ strata/exposure

  dformat <- paste('%.', digits, 'f', sep='')
  vars <- all.vars(formula)
  outcome <- vars[1]
  exposure <- vars[3]
  stratum <- vars[2]
  mht <- mantelhaen.test(data[[outcome]], data[[exposure]],
                         data[[stratum]])
  res <- data.frame(sprintf(dformat,round(mht$estimate[[1]], digits)),
                    sprintf(dformat,round(mht$conf.int[1], digits)),
                    sprintf(dformat,round(mht$conf.int[2], digits)),
                    format.pval(mht$p.value, digits=2, eps=0.001))

  names(res) <- c("OR", "Lower CI", "Upper CI", "Pr(>|z|)")
  rownames(res) <- c()

  return(res)
}

#' Estimate bias due to conditional logistic regression in CXO study
#'
#' @param data  input dataframe
#' @param exposure  exposure variable
#' @param event     outcome variable
#' @param Id        person Id for strata
#'
#' @return  list with bias, CL and MH estimates
#' @export
#'
#' @examples
#' data(cases)
#' SCL_bias(data=cases, exposure=ex, event=Event, Id=Id)
SCL_bias <- function(data, exposure, event, Id) {

  cases <- data %>%
    group_by(Id, .drop = T) %>%
    rename(ex={{exposure}},
           Event={{event}},
           Id={{Id}}) %>%
    mutate(
      minex=min(ex),
      maxex=max(ex)) %>%
    filter(minex != maxex)  %>%  #remove concordant cases
    ungroup()


  cfit <- clogit(Event ~ ex + strata(Id) , data=cases, method="efron") #+ offset(wt)

  est_scl <- exp(coef(cfit))

  mh <- mhor(formula = Event ~ Id/ex, data=cases)
  mh_OR <- as.numeric(mh$OR)


  bias <- abs(100*( est_scl - mh_OR)/mh_OR)

  sprintf("Bias is %.1f%%", bias)

  return(list(bias, est_scl, mh_OR))

}




#' Weighted Conditional Logistic Regression for CXO study
#'
#' @param data       input dataframe
#' @param exposure   name of exposure variable
#' @param event      name of outcome variable
#' @param Id         person ID
#'
#' @return           weighted regression object from clogit
#' @export
#'
#' @examples
#' data(cases)
#' cfit <- CXO_wt(data=cases, exposure=ex, event=Event, Id=Id)
#' summary(cfit)
CXO_wt <- function(data, exposure, event, Id) {
  cases <- data %>%
    mutate(e={{exposure}},
           Event={{event}},
           Id={{Id}}) %>%
    group_by(Id) %>%
    mutate(
      minex=min(e),
      maxex=max(e),
      case_period = Event == 1,
      control_period = Event !=1,
      c1=as.numeric(e==1 & case_period) #exposed case period
    ) %>%
    filter(minex != maxex)  %>%  #remove concordant cases
    ungroup()


  dpt <- cases %>%
    group_by(Id) %>%
    summarise(c1=max(c1),
              c0=1-c1, #unexposed case period
              PT10 = ifelse(c1==1, sum((1-e)*control_period),0),  #number of unexposed control periods per person
              PT01 = ifelse(c1==0, sum(e*control_period),0),  #number of unexposed control periods per persons
              PT0CXO = sum(1-e, na.rm=T), # number of unexposed (case or control) periods
              PT1CXO = sum(e, na.rm=T) # number of exposed (case or control) periods
    ) %>%
    mutate(a1=sum(c1, na.rm=T), #number of exposed case periods
           a0=sum(c0, na.rm=T), # number of unexposed case periods
           PT01m = sum(PT01, na.rm=T)/a0,
           PT10m = sum(PT10, na.rm=T)/a1,
           pi00=1,
           pi10=PT01m/PT10m,
           w0=pi00/PT0CXO,
           w1=pi10/PT1CXO) %>%
    ungroup()

  cases_wt <- left_join(cases, dpt %>% select(Id,w0,w1), by="Id") %>% # number of unexposed case periods
    # rowwise() %>%
    ##calculate weights depending on whether period is exposed or unexposed
    mutate(wt=ifelse(e==1, w1, w0),
           lw=log(wt)) %>%
    ungroup()

  wfit <- clogit(Event ~ e + strata(Id) + offset(lw), data=cases_wt, method="efron")

  return(wfit)

}



#' Internal function for Bootstrapped CIs
#' @keywords internal
#'
#' @param data  input dataframe
#' @param ii    index for bootstrapping
#' @param exposure   exposure variable
#' @param event      outcome
#' @param Id         person ID
#'
#' @return           coefficient of bootstrapped clogit
#' @noRd

.CI_boot <- function(data,ii, exposure, event, Id) {
  ##internal function for bootstrapping the CIs

  df <- data %>%
    mutate(Id= {{Id}})

  cases <- df %>%
    group_by(Id) %>%
    slice_tail(n=1) %>%
    ungroup() %>%
    select(Id)

  #resample the cases
  dd <- cases[ii,] %>%
    mutate(newid = row_number()) %>%
    left_join(df, by="Id")

  cfit <- CXO_wt(dd, exposure = {{exposure}}, event = {{event}}, Id=newid)
  return(coef(cfit))
}



#' Weighted conditional logistic regression for case-crossover with bootstrapped CIs
#'
#' @param data      input dataframe
#' @param exposure  exposure variable
#' @param event     outcome variable
#' @param Id        person Id
#' @param B         number of bootstrapped replicates, default is 500. Minimum 200 recommended
#' @param normal    use normal approximation for bootstrapped CI, default is TRUE
#'                  normal = F uses 2.5% and 97.% quantiles for 95% CI
#'
#' @return         OR estimate and bootstrapped 95% CI
#' @export
#'
#' @examples
#' # Chop ----------------------------------------------------------------------
#' data(cases)
#' try(CXO_wt_boot(data=cases, exposure=ex, event=Event, Id = Id, B=3))
#' # It is recommended that at least 500 bootstrap replications are used
#' # Unchop ----------------------------------------------------------------------
#' \dontrun{
#' CXO_wt_boot(data=cases, exposure=ex, event=Event, Id = Id, B=1000)
#' }
CXO_wt_boot <- function(data, exposure, event, Id, B=500, normal = TRUE) {

  df <- data %>%
    mutate(e={{exposure}},
           Event={{event}},
           Id={{Id}}) %>%
    select(e, Event, Id)

  fitboot <- boot::boot(data=df, statistic = .CI_boot,
                        exposure = e, event = Event, Id=Id,
                        R=B)
  nvars <- dim(fitboot$t)[2]
  ci <- list()
  for (i in (1:nvars)) {
    if (normal) {
      mean = mean(fitboot$t[,i])
      sd = sd(fitboot$t[,i])
      est = exp(mean)
      lower = exp(mean - 1.96*sd)
      upper=  exp(mean + 1.96*sd)
      ci[[i]] <- data.frame(est, lower, upper)
    } else {
      temp <- quantile(fitboot$t[,i],p=c(0.5, 0.025,0.975))
      ci[[i]] <- data.frame(est=exp(temp[1]), lower=exp(temp[2]), upper=exp(temp[3]))
    }


  }


  est0=data.frame(est0= exp(fitboot$t0))
  est0$Variable = rownames(est0)
  est <- bind_cols(est0= est0, bind_rows(ci)) %>%
    select(Variable,est0, est, lower, upper )

  return(est)

}





