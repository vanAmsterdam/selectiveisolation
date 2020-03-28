### read in raw data and clean

library(stringr)
library(purrr)
library(data.table)


csvfiles <- list.files(here::here('data', 'incoming', 'rivm-tables'))
csvs <- map(csvfiles, ~fread(here::here('data', 'incoming', 'rivm-tables', .x)))

## find the table with numbers per age
agetable <- csvs[map_lgl(csvs, ~any(colnames(.x) == 'Leeftijdsgroep'))]
agetable <- agetable[[1]]
### remove the row with total
agetable <- agetable[!Leeftijdsgroep=='Totaal gemeld']
agetable[, agegrp_min:=as.integer(str_extract(Leeftijdsgroep, '(^\\d+)'))]
agetable[, agegrp_max:=as.integer(str_extract(Leeftijdsgroep, '(\\d+)$'))]
agetable[!str_detect(Leeftijdsgroep, '\\d'), Leeftijdsgroep:=NA]
agetable[!is.na(agegrp_min)&is.na(agegrp_max), agegrp_max:=125]

### export
fwrite(agetable, here::here('data', 'agetable.csv'), row.names=F)

## get table with demographics
dems <- fread(here::here('data', 'incoming', "nl_demographics_2019.csv"))
setnames(dems, 'Bevolking (aantal)', 'Bevolking')
# setnames(dems,
#          c('Leeftijd', 'Bevolking (aantal)'),
#          c('Leeftijdsgroep', 'Bevolking'))
dems[, agegrp_min:=as.integer(str_extract(Leeftijd, '(^\\d+)'))]
dems[, agegrp_max:=as.integer(str_extract(Leeftijd, '(?<= tot )(\\d+)'))]
dems[, Leeftijdsgroep:=paste0(agegrp_min, '-', agegrp_max-1)]
dems[is.na(agegrp_max), Leeftijdsgroep:=paste0(agegrp_min, '+')]

# check if agegroups match
if (!all.equal(unique(c(dems$Leeftijdsgroep, NA)), unique(c(agetable$Leeftijdsgroep, NA)))) {
  print(cbind(unique(c(dems$Leeftijdsgroep, NA)),
              unique(c(agetable$Leeftijdsgroep, NA)))
  )
}

### export
fwrite(dems, here::here('data', 'demographics.csv'), row.names=F)

