library(tidyverse)
library(magrittr)
library(lubridate)
library(stringr)
library(tibble)
library(broom)
library(ggplot2)
library(gt)
library(knitr)
library(devtools)
library(DiagrammeR)
library(parallel)
library(foreach)
library(tictoc)
suppressMessages(library(EpiModel))
library(incidence)
library(earlyR)

# we can monkey patch the `EpiModel` code by sourcing the extensions.
source('model/_icm.mod.init.seiqhrf.R')
source('model/_icm.mod.status.seiqhrf.R')
source('model/_icm.mod.vital.seiqhrf.R')
source('model/_icm.control.seiqhrf.R')
source('model/_icm.utils.seiqhrf.R')
source('model/_icm.saveout.seiqhrf.R')
source('model/_icm.icm.seiqhrf.R')

# | Compartment | Functional definition                                                            |
# |-------------|----------------------------------------------------------------------------------|
# | S           | Susceptible individuals                                                          |
# | E           | Exposed **and** infected, not yet symptomatic but potentially infectious         |
# | I           | Infected, symptomatic **and** infectious                                         |
# | Q           | Infectious, but (self-)isolated                                                  |
# | H           | Requiring hospitalisation (would normally be hospitalised if capacity available) |
# | R           | Recovered, immune from further infection                                         |
# | F           | Case fatality (death due to COVID-19, not other causes)                          |

grViz("
digraph SEIQHRF {

  # a 'graph' statement
  graph [overlap = false, fontsize = 10] #, rankdir = LR]

  # several 'node' statements
  node [shape = box,
        fontname = Helvetica]
  S[label='S=Susceptible'];
  E[label='E=Exposed and infected,\nasymptomatic,\npotentially infectious'];
  I[label='I=Infected and infectious'];
  Q[label='Q=(Self-)isolated\n(infectious)'];
  H[label='H=Requires\nhospitalisation'];
  R[label='R=Recovered/immune'];
  F[label='F=Case fatality']

  # several 'edge' statements
  S->E[label='a']
  I->S[style='dashed', label='x']
  E->I[label='b']
  E->S[style='dashed', label='y']
  I->Q[label='c']
  Q->S[style='dashed', label='z']
  I->R[label='d']
  I->H[label='e']
  H->F[label='f']
  H->R[label='g']
  Q->R[label='h']
  Q->H[label='i']
}
")

# function to set-up and run the baseline simulations
simulate <- function(
  
  # model type
  type = "SEIQHRF", 
  
  # number of days to simulate (+ 1)
  nsteps = 366,
  
  # number of simulations
  nsims = 8,
  
  # number of CPU cores
  ncores = 4,
  
  # progression E -> I (T -> binomial, F -> weibull)
  prog.rand = FALSE,
  
  # recovery (I, Q, H) -> R (T -> binomial, F -> weibull)
  rec.rand = FALSE,
  
  # fatality H -> F (T -> binomial, F -> random sample)
  fat.rand = TRUE,
  
  # self-isolation I -> Q (T -> binomial, F -> random sample)
  quar.rand = FALSE,
  
  # hospitalization (I, Q) -> H (T -> binomial, F -> random sample)
  hosp.rand = FALSE,
  
  # recovered at hospital H -> R (T -> binomial, F -> random sample)
  disch.rand = TRUE,
  
  # function to implement infection process
  infection.FUN = infection.seiqhrf.icm,
  
  # function to implement recovery process
  recovery.FUN = progress.seiqhrf.icm,
  
  # function to process demographics (deaths NOT COVID and emigration)
  departures.FUN = departures.seiqhrf.icm,
  
  # function to process demographics (births and inmigration)
  arrivals.FUN = arrivals.icm,
  
  # collects prevalence and transition times
  get_prev.FUN = get_prev.seiqhrf.icm,
  
  # init.icm params
  # initial population
  s.num = 9997,
  
  # initial number of exposed
  e.num=0,
  
  # initial number of infected
  i.num = 3,
  
  # initial number of self-isolated
  q.num=0,
  
  # initial number of hospitalized
  h.num=0,
  
  # initial number of recovered 
  r.num = 0,
  
  # initial number of deaths
  f.num = 0,
  
  # param.icm params
  #############################################
  # HYGIENE MEASURES
  #############################################
  # prob. of infection between E and S at each exposure
  inf.prob.e = 0.02, 
  # prob. of infection between I and S at each exposure
  inf.prob.i = 0.05, 
  # prob. of infection between I and S at each exposure
  inf.prob.q = 0.02, 
  
  #############################################
  # SOCIAL DISTANCING
  #############################################
  # number of exposures between E and S per day
  act.rate.e = 10,
  # number of exposures between I and S per day
  act.rate.i = 10,
  # number of exposures between Q and S per day
  act.rate.q = 2.5,
  #############################################
  

  # rate per day for I -> Q  (ratio of self-isolation)
  quar.rate = 1/30, 
  
  # rate per day for I -> H (rate of hospitalization)
  hosp.rate = 1/100,
  
  # rate per day for H -> R (rate from hospitalized to recovered)
  disch.rate = 1/15,
  
  # rate per day for E -> I (rate from asymptomatic to infectious)
  prog.rate = 1/10,
  
  # scale for Weibull distribution for progression
  prog.dist.scale = 5,

  # shape for Weibull distribution for progression
  prog.dist.shape = 1.5,
  
  # rate per day for I -> R (rate from infectious to recovered)
  rec.rate = 1/20,
  
  # scale for Weibull distribution for recovery
  rec.dist.scale = 35,
  
  # shape for Weibull distribution for recovery
  rec.dist.shape = 1.5,
  
  # rate for H -> F (ratio of hospitalized that will die, per day)
  fat.rate.base = 1/50,
  
  # number of hospital beds
  hosp.cap = 40,
  
  # rate per day for I -> H when no hospital bed is available
  fat.rate.overcap = 1/25,
  
  # Time coefficient for increasing mortality rate as time in the 
  # H compartment increases for each individual in it
  fat.tcoeff = 0.5,
  
  # enables demographics
  vital = TRUE,
  
  # background demographics arrival rate (births -> S)
  a.rate = (10.5/365)/1000, 
  
  # ????????????????????????????????????????????????????????
  a.prop.e = 0.01,
  a.prop.i = 0.001,
  a.prop.q = 0.01,
  
  # rates for demographic background (not COVID)
  ds.rate = (7/365)/1000, 
  de.rate = (7/365)/1000, 
  di.rate = (7/365)/1000,
  dq.rate = (7/365)/1000,
  dh.rate = (20/365)/1000,
  dr.rate = (7/365)/1000,
  
  # summary function for results (median, percentiles, see EpiModel doc.)
  out="mean"
) {
  
  control <- control.icm(type = type, 
                         nsteps = nsteps, 
                         nsims = nsims,
                         ncores = ncores,
                         prog.rand = prog.rand,
                         rec.rand = rec.rand,
                         infection.FUN = infection.FUN,
                         recovery.FUN = recovery.FUN,
                         arrivals.FUN = arrivals.FUN,
                         departures.FUN = departures.FUN,
                         get_prev.FUN = get_prev.FUN)
  
  init <- init.icm(s.num = s.num,
                   e.num = e.num,
                   i.num = i.num,
                   q.num = q.num,
                   h.num = h.num,
                   r.num = r.num,
                   f.num = f.num)
  
  param <-  param.icm(inf.prob.e = inf.prob.e, 
                      act.rate.e = act.rate.e,
                      inf.prob.i = inf.prob.i, 
                      act.rate.i = act.rate.i,
                      inf.prob.q = inf.prob.q, 
                      act.rate.q = act.rate.q,                    
                      quar.rate = quar.rate,
                      hosp.rate = hosp.rate,
                      disch.rate = disch.rate,
                      prog.rate = prog.rate,
                      prog.dist.scale = prog.dist.scale,
                      prog.dist.shape = prog.dist.shape,
                      rec.rate = rec.rate,
                      rec.dist.scale = rec.dist.scale,
                      rec.dist.shape = rec.dist.shape,
                      fat.rate.base = fat.rate.base,
                      hosp.cap = hosp.cap,
                      fat.rate.overcap = fat.rate.overcap,
                      fat.tcoeff = fat.tcoeff,
                      vital = vital,
                      a.rate = a.rate, 
                      a.prop.e = a.prop.e,
                      a.prop.i = a.prop.i,
                      a.prop.q = a.prop.q,
                      ds.rate = ds.rate, 
                      de.rate = de.rate, 
                      di.rate = di.rate,
                      dq.rate = dq.rate,
                      dh.rate = dh.rate,
                      dr.rate = dr.rate)
  
  sim <- icm.seiqhrf(param, init, control)
  sim_df <- as.data.frame(sim, out=out)
  
  return(list(sim=sim, df=sim_df))
}

baseline_sim <- simulate(ncores=4)

# define a function to extract timings and assemble a data frame
get_times <- function(simulate_results) {
  
  sim <- simulate_results$sim
  
  for (s in 1:sim$control$nsims) {
    if (s == 1) {
      times <- sim$times[[paste("sim",s,sep="")]]
      times <- times %>% mutate(s=s)
    } else {
      times <- times %>%
        bind_rows(sim$times[[paste("sim",s,sep="")]] %>%
                    mutate(s=s))
    }
  }
  
  times <- times %>%
    mutate(infTime=ifelse(infTime <0, -5, infTime),
           expTime=ifelse(expTime <0, -5, expTime)) %>%
    mutate(incubation_period = infTime - expTime,
           illness_duration = recovTime - expTime,
           illness_duration_hosp = dischTime - expTime,
           hosp_los = dischTime - hospTime,
           quarantine_delay = quarTime - infTime,
           survival_time = fatTime - infTime) %>%
    select(s, 
           incubation_period,
           quarantine_delay,
           illness_duration,
           illness_duration_hosp,
           hosp_los,
           survival_time) %>%
    pivot_longer(-s, names_to="period_type",
                 values_to="duration") %>%
    mutate(period_type = factor(period_type, levels=c("incubation_period",
                                                      "quarantine_delay",
                                                      "illness_duration",
                                                      "illness_duration_hosp",
                                                      "hosp_los",
                                                      "survival_time"),
                                labels=c("Incubation period",
                                         "Delay entering isolation",
                                         "Illness duration",
                                         "Illness duration (hosp)",
                                         "Hospital care required duration",
                                         "Survival time of case fatalities"),
                                ordered = TRUE))
  return(times)
}

times <- get_times(baseline_sim)

times %>%
  filter(duration <= 30) %>%
  ggplot(aes(x=duration)) +
  geom_bar() +
  facet_grid(period_type~., scales="free_y") +
  labs(title="Duration frequency distributions",
       subtitle="Baseline simulation")

baseline_plot_df <- baseline_sim$df %>%
  # use only the prevalence columns
  select(time, s.num, e.num, i.num, q.num, 
         h.num, r.num, f.num) %>%
  # examine only the first 100 days since it
  # is all over by then using the default parameters
  filter(time <= 100) %>%
  pivot_longer(-c(time),
               names_to="compartment",
               values_to="count")

# define a standard set of colours to represent compartments
compcols <- c("s.num" = "yellow", "e.num" = "orange", "i.num" = "red",
              "q.num" = "cyan", "h.num" = "magenta", "r.num" = "lightgreen",
              "f.num" = "black")
complabels <- c("s.num" = "Susceptible", "e.num" = "Infected/asymptomatic", 
                "i.num" = "Infected/infectious", "q.num" = "Self-isolated",
                "h.num" = "Requires hospitalisation", "r.num" = "Recovered",
                "f.num" = "Case fatality")

baseline_plot_df %>%
  ggplot(aes(x=time, y=count, colour=compartment)) +
  geom_line(size=2, alpha=0.7) +
  scale_colour_manual(values = compcols, labels=complabels) +
  theme_dark() +
  labs(title="Baseline simulation",
       x="Days since beginning of epidemic",
       y="Prevalence (persons)")

baseline_plot_df %>%
  filter(compartment %in% c("e.num","i.num",
                            "q.num","h.num",
                            "f.num")) %>%
  ggplot(aes(x=time, y=count, colour=compartment)) +
  geom_line(size=2, alpha=0.7) +
  scale_colour_manual(values = compcols, labels=complabels) +
  theme_dark() +
  labs(title="Baseline simulation",
       x="Days since beginning of epidemic",
       y="Prevalence (persons)")

# get the S-> E compartment flow, which is
# our daily incidence rate
incidence_counts <- baseline_sim$df %>%
  select(time, se.flow)
# uncount them
incident_case_dates <- incidence_counts %>%
  uncount(se.flow) %>%
  pull(time) 
# convert to an incidence object
incidence_all <- incident_case_dates %>%
  incidence(.)

# plot the incidence curve
plot(incidence_all)

# find the peak of the epidemic curve
peak_of_epidemic_curve <- find_peak(incidence_all)

# repeat with just the growth part of the epidemic curve
incident_case_dates_growth_phase <- incidence_counts %>%
  filter(time <= peak_of_epidemic_curve) %>%
  select(time, se.flow) %>%
  uncount(se.flow) %>%
  pull(time)

incidence_growth_phase <- incident_case_dates_growth_phase %>%
  incidence(., last_date=peak_of_epidemic_curve)
# specify serial interval mean and SD
# since the last blog post new studies have appeared
# suggesting 4.5 is a better mean for the SI
si_mean <- 4.5
si_sd <- 3.4

# get a set of MLE estimates for R0 and plot
res <- get_R(incidence_growth_phase, si_mean =si_mean, si_sd = si_sd)
plot(res, "R")

quar_rate_ramp <- function(t) {
  ifelse(t < 15, 0.0333, ifelse(t <= 30, 0.0333 + (t-15)*(0.5 - 0.0333)/15, 0.5))
}
ramp_quar_rate_sim <- simulate(quar.rate = quar_rate_ramp(1:366))

ramp_quar_rate_sim_plot_df <- ramp_quar_rate_sim$df %>%
  # use only the prevalence columns
  select(time, s.num, e.num, i.num, q.num, 
         h.num, r.num, f.num) %>%
  # examine only the first 100 days since it
  # is all over by then using the default parameters
  filter(time <= 100) %>%
  pivot_longer(-c(time),
               names_to="compartment",
               values_to="count")

ramp_quar_rate_sim_plot_df %>%
  filter(compartment %in% c("e.num","i.num",
                            "q.num","h.num",
                            "f.num")) %>%
  ggplot(aes(x=time, y=count, colour=compartment)) +
  geom_line(size=2, alpha=0.7) +
  scale_colour_manual(values = compcols, labels=complabels) +
  theme_dark() +
  labs(title="Ramp up isolation rates from day 15 to day 30",
       x="Days since beginning of epidemic",
       y="Prevalence (persons)")

baseline_plot_df %>%
  mutate(experiment="Baseline") %>%
  bind_rows(ramp_quar_rate_sim_plot_df %>%
              mutate(experiment="Ramp up isolation")) %>%
  filter(compartment %in% c("e.num","i.num",
                            "q.num","h.num",
                            "f.num")) %>%
  ggplot(aes(x=time, y=count, colour=compartment)) +
  geom_line(size=2, alpha=0.7) +
  facet_grid(experiment ~ .) +
  scale_colour_manual(values = compcols, labels=complabels) +
  theme_dark() +
  labs(title="Baseline vs ramping up isolation simulations",
       x="Days since beginning of epidemic",
       y="Prevalence (persons)")

baseline_plot_df %>%
  mutate(experiment="Baseline") %>%
  bind_rows(ramp_quar_rate_sim_plot_df %>%
              mutate(experiment="Ramp up isolation")) %>%
  filter(compartment %in% c("h.num",
                            "f.num")) %>%
  ggplot(aes(x=time, y=count, colour=compartment)) +
  geom_line(size=2, alpha=0.7) +
  geom_hline(yintercept = 40, colour="red", alpha=0.5) + 
  facet_grid(experiment ~ .) +
  scale_colour_manual(values = compcols, labels=complabels) +
  theme_dark() +
  labs(title="Baseline vs ramping up isolation simulations",
       x="Days since beginning of epidemic",
       y="Prevalence (persons)")



## Experiment 2 - more hospital beds

# Over a four week period, let's ramp up hospital capacity to triple 
# the baseline level, starting at day 15. 

hosp_cap_ramp <- function(t) {
  ifelse(t < 15, 40, ifelse(t <= 45, 40 + (t-15)*(120 - 40)/30, 120))
}

raise_hosp_cap_sim <- simulate(hosp.cap = hosp_cap_ramp(1:366))

## Experiment 3 - more social distancing starting at day 15

# Let's step up social distancing (decrease exposure opportunities), 
# starting at day 15, in everyone except the self-isolated, who are already 
# practising it. But we'll leave the self-isolation rate at the baseline desultory 
# rate. The increase in social distancing will, when full ramped up by day 30, 
# halve the number of exposure events between the infected and the susceptible each day.

social_distancing_day15_ramp <- function(t) {
  ifelse(t < 15, 10, ifelse(t <= 30, 10 - (t-15)*(10 - 5)/15, 5))
}

t15_social_distancing_sim <- simulate(act.rate.i = social_distancing_day15_ramp(1:366),
                                      act.rate.e = social_distancing_day15_ramp(1:366))

## Experiment 4 - more social distancing but starting at day 30

# Let's repeat that, but we'll delay starting the social distancing ramp-up until day 30.

social_distancing_day30_ramp <- function(t) {
  ifelse(t < 30, 10, ifelse(t <= 45, 10 - (t-30)*(10 - 5)/15, 5))
}

t30_social_distancing_sim <- simulate(act.rate.i = social_distancing_day30_ramp(1:366),
                                      act.rate.e = social_distancing_day30_ramp(1:366))

## Experiment 5 - increase both social distancing and increased self-isolation 
# rates starting day 15

quar_rate_ramp <- function(t) {
  ifelse(t < 15, 0.0333, ifelse(t <= 30, 0.0333 + (t-15)*(0.5 - 0.0333)/15, 0.5))
}

ramp_quar_rate_sim <- simulate(quar.rate = quar_rate_ramp(1:366))  


t15_soc_dist_quar_sim <- simulate(act.rate.i = social_distancing_day15_ramp(1:366),
                                  act.rate.e = social_distancing_day15_ramp(1:366),
                                  quar.rate = quar_rate_ramp(1:366))


baseline_sim$df %>%
  select(time, s.num, e.num, i.num, q.num, 
         h.num, r.num, f.num) %>%
  mutate(experiment = "0. Baseline") %>%
  bind_rows(ramp_quar_rate_sim$df %>%
              select(time, s.num, e.num, i.num, q.num, 
                     h.num, r.num, f.num) %>%
              mutate(experiment = "1. incr quar @ t=15")) %>%
  bind_rows(raise_hosp_cap_sim$df %>%
              select(time, s.num, e.num, i.num, q.num, 
                     h.num, r.num, f.num) %>%
              mutate(experiment = "2. incr hos cap @ t=15")) %>%
  bind_rows(t15_social_distancing_sim$df %>%
              select(time, s.num, e.num, i.num, q.num, 
                     h.num, r.num, f.num) %>%
              mutate(experiment = "3. incr soc dist @ t=15")) %>%
  bind_rows(t30_social_distancing_sim$df %>%
              select(time, s.num, e.num, i.num, q.num, 
                     h.num, r.num, f.num) %>%
              mutate(experiment = "4. incr soc dist @ t=30")) %>%
  bind_rows(t15_soc_dist_quar_sim$df %>%
              select(time, s.num, e.num, i.num, q.num, 
                     h.num, r.num, f.num) %>%
              mutate(experiment = "5. incr soc dist & quar @ t=15")) %>%
  filter(time <= 150) %>%
  pivot_longer(-c(time, experiment),
               names_to="compartment",
               values_to="count") %>%
  filter(compartment %in% c("e.num","i.num",
                            "q.num","h.num",
                            "f.num")) -> plot_df

plot_df %>%
  ggplot(aes(x=time, y=count, colour=compartment)) +
  geom_line(size=1.5, alpha=0.7) +
  facet_grid(experiment ~ .) +
  scale_colour_manual(values = compcols, labels=complabels) +
  theme_dark() +
  labs(title="Baseline vs experiments",
       x="Days since beginning of epidemic",
       y="Prevalence (persons)")

plot_df %>%
  filter(compartment %in% c("h.num",
                            "f.num")) %>%
  ggplot(aes(x=time, y=count, colour=compartment)) +
  geom_line(size=1.5, alpha=0.7) +
  facet_grid(experiment ~ .) +
  scale_colour_manual(values = compcols, labels=complabels) +
  theme_dark() +
  labs(title="Baseline vs experiments",
       x="Days since beginning of epidemic",
       y="Prevalence (persons)")



### Two more experiments
# What happens if we dramatically increase social distancing through 
# a two week lock-down, which is then relaxed? We’ll use a step function 
# to model this. We test such a lock-down lasting from day 15 to 30, and 
# separately a lock-down from day 30 to day 45 instead. We’ll model the 
# lock-down by reducing the act.rate parameters for all compartments,
# from 10 to 2.5.

twoweek_lockdown_day15_vector <- c(rep(10, 15), rep(2.5, 15), 
                                   rep(10, 336))
twoweek_lockdown_day30_vector <- c(rep(10, 30), rep(2.5, 15), 
                                   rep(10, 321))

twoweek_lockdown_day15_sim <- simulate(act.rate.i = twoweek_lockdown_day15_vector, 
                                       act.rate.e = twoweek_lockdown_day15_vector)

twoweek_lockdown_day30_sim <- simulate(act.rate.i = twoweek_lockdown_day30_vector, 
                                       act.rate.e = twoweek_lockdown_day30_vector)


###################
## PLOTS NEEDED! ##
###################

fourweek_lockdown_day15_vector <- c(rep(10, 15), rep(2.5, 30), 
                                    rep(7.5, 321))
fourweek_lockdown_day30_vector <- c(rep(10, 30), rep(2.5, 30), 
                                    rep(7.5, 306))

fourweek_lockdown_day15_sim <- simulate(act.rate.i = fourweek_lockdown_day15_vector, 
                                        act.rate.e = fourweek_lockdown_day15_vector, 
                                        quar.rate = quarantine_ramp(1:366), 
                                        inf.prob.q = 0.01)

fourweek_lockdown_day30_sim <- simulate(act.rate.i = fourweek_lockdown_day30_vector, 
                                        act.rate.e = fourweek_lockdown_day30_vector, 
                                        quar.rate = quarantine_ramp(1:366), 
                                        inf.prob.q = 0.01)

###################
## PLOTS NEEDED! ##
###################
