#' Weighted conditional logistic regression for case-time-control study
#'
#' @param data  input dataframe
#' @param exposure  exposure variable
#' @param event     outcome variable
#' @param Id        person ID
#'
#'
#' @return         weighted regression object from clogit
#' @export
#'
#' @examples
#' data(casetimecontrols)
#' ctcfit <- CXO_tc_wt(data=casetimecontrols,exposure=ex,event=Event,Id=Id)
#' summary(ctcfit)
CXO_tc_wt <- function(data, exposure, event, Id) {
  # tc = 1 for time-controls, 1 for cases in all periods
  # data is assumed to be sorted so that the case period is the last row per Id
  case_tc <- data %>%
    mutate(e={{exposure}},
           Event={{event}},
           Id={{Id}}) %>%
    group_by(Id) %>%
    mutate(period = row_number(),
           minex=min(e),
           maxex=max(e),
           d=max(Event), #d=1 for cases and 0 for time-controls
           case_period = as.numeric(period == max(period)),
           control_period = 1- case_period,
           c1=as.numeric(e==1 & case_period) #exposed case period
    ) %>%
    filter(minex != maxex)  %>%  #remove concordant cases
    ungroup()


  dpt <- case_tc %>%
    group_by(Id) %>%
    summarise(c1=max(c1),
              c0=1-c1, #unexposed case period
              PT10 = ifelse(c1==1, sum((1-e)*control_period),0),  #number of unexposed control periods per person
              PT01 = ifelse(c1==0, sum(e*control_period),0),  #number of unexposed control periods per persons
              PT0CXO = sum(1-e, na.rm=T), # number of unexposed (case or control) periods
              PT1CXO = sum(e, na.rm=T) # number of exposed (case or control) periods
    ) %>%
    mutate(n1=sum(c1, na.rm=T), #number of exposed case periods for both cases and time-controls
           n0=sum(c0, na.rm=T), # number of unexposed case periods for both cases and time-controls
           PT01m = sum(PT01, na.rm=T)/n0,
           PT10m = sum(PT10, na.rm=T)/n1,
           pi00=1,
           pi10=PT01m/PT10m,
           w0=pi00/PT0CXO,
           w1=pi10/PT1CXO) %>%
    ungroup()

  cases_wt <- left_join(case_tc, dpt %>% select(Id, w0,w1), by="Id") %>% # number of unexposed case periods
    # rowwise() %>%
    ##calculate weights depending on whether period is exposed or unexposed
    mutate(wt=ifelse(e==1, w1, w0),
           lw=log(wt),
           ex_tc=relevel(factor(e), ref="0"),
           ex=relevel(factor(d*e), ref="0")) %>%
    ungroup()

  wfit <- clogit(case_period ~ ex + ex_tc + strata(Id) + offset(lw) ,
                 data=cases_wt, method="efron")
  #coefficient of ex_tc is for time-controls/time-trend
  #coefficient of ex is for exposure-outcome

  return(wfit)

}


#' Internal function for Bootstrapped CIs
#'
#' @param data  input dataframe
#' @param ii    index for bootstrapping
#' @param exposure   exposure variable
#' @param event      outcome
#' @param Id         person ID
#'
#' @return           coefficient of bootstrapped clogit

.CI_tc_boot <- function(data,ii, exposure, event , Id) {
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

  cfit <- CXO_tc_wt(dd, exposure = {{exposure}}, event = {{event}}, Id=newid)
  return(coef(cfit))
}


#' Weighted conditional logistic regression for case-time-control with bootstrapped CIs
#'
#' @param data  input dataframe
#' @param exposure  exposure variable
#' @param event     outcome
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
#' data(casetimecontrols)
#' try(CXO_tc_wt_boot(casetimecontrols, exposure = ex, event = Event, Id=Id, B = 3))
#' # It is recommended that at least 500 bootstrap replications are used
#' # Unchop ----------------------------------------------------------------------
#' \dontrun{
#' CXO_tc_wt_boot(casetimecontrols, exposure = ex, event = Event, Id=Id, B = 1000)
#' }
CXO_tc_wt_boot <- function(data, exposure, event, Id, B=500, normal = TRUE) {

  df <- data %>%
    mutate(e={{exposure}},
           Event={{event}},
           Id={{Id}}) %>%
    select(e, Event, Id)

  fitboot <- boot::boot(data=df, statistic = .CI_tc_boot,
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


