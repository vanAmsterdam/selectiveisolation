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
  ggplot(aes(x=agegrp_min,y=normalized_ir,group=variable,col=variable)) + 
  geom_line() + theme_minimal() + 
  ggtitle('Relative occurences',  'of known infection, hospitalization and deaths per age group') +
  labs(y='Proportion of total per variable')

## make some scenarios
marginaldeathrates <- seq(1.0, 3.5, by=0.5)
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

#### create outer tables for each setting
agem[, dumkey:=1]
dfs <- agem[variable=='Overleden'][settings, on='dumkey', allow.cartesian=T]


#### calculate number infected
dfs[, inherd:=agegrp_max < freeage]
dfs[demsmf, Bevolking:=i.Bevolking, on='Leeftijdsgroep']
dfs[, num_infected:=herd_prop*Bevolking*as.numeric(agegrp_max<freeage)]


##### calculate total number of deaths
dfs[, deathrate:=marginaldeathrate*ir_norm]
dfs[demsmf, deaths:=deathrate*num_infected, on='Leeftijdsgroep']

## tables
totaldeaths <- dfs[, list(deaths=sum(deaths), num_infected=sum(num_infected)),
    by=c('marginaldeathrate', 'herdimmunityprop', 'freeage', 'setting_idx')]
totaldeaths[order(deaths)]
dcast(totaldeaths, herdimmunityprop~freeage+marginaldeathrate, value.var='deaths')

livessaved <- dcast(totaldeaths[freeage==max(freeage)], herdimmunityprop~marginaldeathrate, value.var='deaths') - 
              dcast(totaldeaths[freeage==min(freeage)], herdimmunityprop~marginaldeathrate, value.var='deaths')
  

## plots
dfs %>% 
  mutate(
    freeage=factor(freeage)
    ) %>%
  ggplot(aes(x=agegrp_min, y=deaths, group=setting_idx, col=freeage)) + 
  geom_line() + 
  facet_grid(herdimmunityprop~marginaldeathrate) + 
  theme_minimal()
  
totaldeaths %>%
  ggplot(aes(x=herdimmunityprop, y=marginaldeathrate)) + 
  geom_tile(aes(fill=deaths)) + 
  facet_wrap(~freeage)
