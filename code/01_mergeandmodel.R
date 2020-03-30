# merge datas and model

library(ggplot2)
library(dplyr)
library(purrr)
library(stringr)
library(data.table)

table <- function(...) base::table(..., useNA='always')

# get the (curated) data
agetable <- fread(here::here('data', 'agetable.csv'), na.strings = '')
dems     <- fread(here::here('data', 'demographics.csv'), na.strings='')

## normalize 
dems[, Bevolking_prop:=Bevolking / sum(Bevolking)]
### make one with both sexes
grpvars <- c('agegrp_min', 'agegrp_max', 'Leeftijdsgroep')
demsmf <- dems[,list(
  Bevolking=sum(Bevolking),
  Bevolking_prop=sum(Bevolking_prop)
), by = grpvars]
# demsmf[, plot(agegrp_min, Bevolking)]

# make a long version of the data
## remove missing age group (is tiny and not relevant for relative rates)
grpvars <- c('agegrp_min', 'agegrp_max', 'Leeftijdsgroep')
agem <- melt(agetable[!is.na(Leeftijdsgroep)], id.vars=grpvars,
             measure.vars=c('Totaal', 'Ziekenhuisopname', 'Overleden'),
             value.name='casecount')

## calculate proportions
agem[, prop:=casecount / sum(casecount), by = c('variable')]

## standardize per demographics
mvars <- c('Bevolking', 'Bevolking_prop')
### make incidencerates by merging data on age group
agem[demsmf, incidencerate:=casecount / i.Bevolking, on='Leeftijdsgroep']
### make normalized incidence rate (as prop of total)
agem[, ir_norm:=incidencerate / sum(incidencerate), by = 'variable']

## plots

### ecdf of age
demsmf[order(agegrp_min), Bevolking_cum:=cumsum(Bevolking)]
demsmf[, Bevolking_cdf:=Bevolking_cum / sum(Bevolking)]
demsmf[, plot(agegrp_min, Bevolking_cdf, type='l')]

agem %>%
  ggplot(aes(x=agegrp_min,y=prop,group=variable,col=variable)) + 
  geom_line() + theme_minimal() + 
  ggtitle('Relative occurences',  'of known infection, hospitalization and deaths per age group') +
  labs(y='Proportion of total per variable')

## plots
agem %>%
  ggplot(aes(x=agegrp_min,y=incidencerate,group=variable,col=variable)) + 
  geom_line() + theme_minimal() + 
  ggtitle('Incidence rate',  'of known infection, hospitalization and deaths per age group') +
  labs(y='Incindence rate')

agem %>%
  ggplot(aes(x=agegrp_min,y=ir_norm,group=variable,col=variable)) + 
  geom_line() + theme_minimal() + 
  ggtitle('Relative occurences',  'of known infection, hospitalization and deaths per age group') +
  labs(y='Proportion of total per variable')

## make some scenarios
marginaldeathrates <- seq(0.005, 0.035, by=0.005)
herdimmunityprops  <- seq(0.6, 0.8, by=0.05)
freeages           <- c(65, 130)
                          
### look at minimum age to get at least this amount of the people infected
Bevolking_cdf_fn  <- approxfun(x=demsmf$agegrp_max, y=demsmf$Bevolking_cdf)
Bevolking_icdf_fn <- approxfun(x=demsmf$Bevolking_cdf, y=demsmf$agegrp_max)
maxfreeage  <- Bevolking_icdf_fn(herdimmunityprop)
maxfreeages <- Bevolking_icdf_fn(herdimmunityprops)
print(paste0('maximum age for non-restricted movement to get at least ', 
             herdimmunityprop,
             ' herd immunity:'))
print(data.table(herdimmunityprop=herdimmunityprops,
                 maxfreeage=maxfreeages))

#### define settings
settings <- CJ(
  marginaldeathrate = marginaldeathrates,
  herdimmunityprop  = herdimmunityprops,
  freeage           = freeages,
  dumkey            = 1
)
settings[, setting_idx:=1:.N]
settings[, herd_needed:=herdimmunityprop*demsmf[, sum(Bevolking)]]
settings[, herd_avail:=demsmf[agegrp_max<freeage, max(Bevolking_cum)], by = 'setting_idx']
settings[, herd_prop:=herd_needed/herd_avail]

## using these equations:

# R_{marg} = \sum_i{p_i*R_i}
# Where $p_i$ the proportion of the population in age group $i$, 
# $R_i$ the death rate in that age group
# Introduce a single factor R such that $R_i = R * rr_i$, and that $\sum_i{rr_i}=1$
# Then
# R_{marg} = \sum_i{p_i*R*rr_i} = R * \sum_i{p_i*rr_i}
# So now we have that
# R = \frac{R_{marg}}{\sum_{p_i*rr_i}}
# Calculate $\sum_{p_i*rr_i} := ir_mu$
ir_mu = agem[variable=='Overleden'][demsmf, sum(ir_norm*i.Bevolking_prop), on='Leeftijdsgroep']
settings[, Rfactor:=marginaldeathrate / ir_mu]

#### create outer tables for each setting
agem[, dumkey:=1]
dfs <- agem[variable=='Overleden'][settings, on='dumkey', allow.cartesian=T]


#### calculate number infected
dfs[, inherd:=agegrp_max < freeage]
dfs[demsmf, Bevolking:=i.Bevolking, on='Leeftijdsgroep']
dfs[demsmf, Bevolking_prop:=i.Bevolking_prop, on='Leeftijdsgroep']
dfs[, num_infected:=herd_prop*Bevolking*as.numeric(agegrp_max<freeage)]


##### calculate total number of deaths
dfs[, deathrate:=Rfactor*ir_norm]
###### make sure the marginal death rates are ok
drs <- dfs[, list(obs_marginaldeathrate=sum(Bevolking_prop*deathrate)), by=c('setting_idx', 'marginaldeathrate')]
stopifnot(drs[, all.equal(obs_marginaldeathrate, marginaldeathrate)])
dfs[, deaths:=deathrate*num_infected]

### define a baseline (freeage==130)
baseline <- dfs[freeage==130]
dfs[baseline, deathdifference:=deaths - i.deaths, 
    on=c('marginaldeathrate', 'herdimmunityprop', 'Leeftijdsgroep')]
dfs[, excessdeaths:=ifelse(deathdifference>0, deathdifference, 0)]
dfs[, preventeddeaths:=ifelse(deathdifference<0, -deathdifference, 0)]
dfs[, totaldeaths:=sum(deaths), by='setting_idx']

## tables
totaldeaths <- dfs[, list(deaths=sum(deaths), 
                          num_infected=sum(num_infected)),
    by=c('marginaldeathrate', 'herdimmunityprop', 'freeage', 'setting_idx')]
totaldeaths[order(deaths)]
dcast(totaldeaths, herdimmunityprop~freeage+marginaldeathrate, value.var='deaths')

livessaved <- dcast(totaldeaths[freeage==max(freeage)], herdimmunityprop~marginaldeathrate, value.var='deaths') - 
              dcast(totaldeaths[freeage==min(freeage)], herdimmunityprop~marginaldeathrate, value.var='deaths')
livessaved 

## plots
### deathrate
dfs %>%
  .[!duplicated(paste0(marginaldeathrate, Leeftijdsgroep))] %>%
  mutate(marginaldeathrate=factor(marginaldeathrate)) %>%
  ggplot(aes(x=agegrp_min, y=deathrate, grp=marginaldeathrate,
             col=marginaldeathrate)) + 
  geom_line() + 
  theme_minimal() + 
  ggtitle('Age-specific death rate', 'per marginal death rate')

dfs %>% 
  mutate(freeage=factor(freeage)) %>%
  ggplot(aes(x=agegrp_min, y=deaths, group=setting_idx, col=freeage)) + 
  geom_line() + 
  facet_grid(herdimmunityprop~marginaldeathrate) + 
  theme_minimal() + 
  ggtitle('Total number of deaths', 'Per marginal deathrate and herd immunity proportion, and isolation type (freeage)')

dfs %>% 
  .[freeage<130] %>%
  .[excessdeaths>=0,] %>%
  mutate(freeage=factor(freeage)) %>%
  ggplot(aes(x=agegrp_min, y=excessdeaths, group=setting_idx)) + 
  geom_line() + 
  facet_grid(herdimmunityprop~marginaldeathrate) + 
  theme_minimal() + 
  ggtitle('Excess deaths', 'Deaths that would not have occured in the non-selective isolation method')

dfs %>%
  .[freeage<130] %>%
  ggplot(aes(x=totaldeaths, y=excessdeaths, grp=Leeftijdsgroep)) + 
  geom_line() + 
  facet_wrap(~Leeftijdsgroep) + 
  theme_minimal() + 
  ggtitle('Excess deaths', 'Versus total deaths')
  
dfs %>%
  .[freeage<130] %>%
  .[, preventeddeaths:=sum(preventeddeaths), by='setting_idx'] %>%
  ggplot(aes(x=preventeddeaths, y=excessdeaths, grp=Leeftijdsgroep)) + 
  geom_line() + 
  facet_wrap(~Leeftijdsgroep) + 
  theme_minimal() + 
  ggtitle('Excess deaths', 'Versus preventeddeaths')
  

