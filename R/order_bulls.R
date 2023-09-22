
library(data.table)
library(stringr)
# read cow info
codes <- rio::import('data/codes.xlsx') |> setDT()
cows <- rio::import('C:/Users/vofde/OneDrive/03 Koeien/Stieren/221022_cows.xlsx') |> setDT()

ss.bulls <- fread('data/ss.bulls.csv')

dt.cows <- merge(cows, codes, by.x = 'aaa_cow', by.y = 'code')
dt.cows <- melt(dt.cows, measure.vars=c(colnames(codes)[-1]), variable.name = 'match', value.name = 'aaa_bull')
dt.cows[, match:=as.numeric(as.character(match))]
dt.cows[,c('reden', 'code', 'stier_1', 'code_1', 'stier_2', 'code_2') := NULL]


# merge cows and bulls
dt <- merge(dt.cows, ss.bulls, by='aaa_bull', allow.cartesian = T)

dt <- dt[!stier %in% c('blauw', 'gust')]

# Inteelt (in ieder geval geen (groot) ouders)
ft <- function(x) str_to_lower(str_replace_all(x, ' |[.]|[-]', ''))

dt[, is_inteelt := F]
dt[ft(bull_name) == ft(dad_name) | ft(bull_name) == ft(dadsdad_name) | ft(bull_name) == ft(momsdad_name), is_inteelt := T]
dt[ft(bull_dad) == ft(dad_name) | ft(bull_dad) == ft(dadsdad_name) | ft(bull_dad) == ft(momsdad_name), is_inteelt := T]
dt[ft(bulls_moms_dad) == ft(dad_name) | ft(bulls_moms_dad) == ft(dadsdad_name) | ft(bulls_moms_dad) == ft(momsdad_name), is_inteelt := T]


# Harde filters -----------------------------------------------------------

# remove inteelt
dt <- dt[is_inteelt == F]

# alleen nl stieren?
dt <- dt[substr(nummer, 1,3) == 'NLD' | kiv==T | kik == T]
# dt <- dt[| kiv==T | kik == T]

# select color
dt <- dt[(color_cow == 'RB' & color_bull == 'RB') | color_cow == 'ZB']

# triple A 0.7
dt <- dt[match>0.75]


# remove 10% ondereind productie

dt <- dt[score_prod > 0] # quantile(ss.bulls$score_prod, q)]   
dt <- dt[score_eiwit > 0]#quantile(ss.bulls$score_eiwit, q) ] 
dt <- dt[score_vet > 0]# quantile(ss.bulls$score_vet, q)]

q <- .1

# functioneel
dt <- dt[score_persistentie > quantile(ss.bulls$score_persistentie, q, na.rm = T)]
# dt <- dt[score_aanhouding > quantile(ss.bulls$score_aanhouding, q, na.rm = T)]
dt <- dt[score_laatrijpheid > quantile(ss.bulls$score_laatrijpheid, q, na.rm = T)]
dt <- dt[score_vruchtbaarheid > quantile(ss.bulls$score_vruchtbaarheid, q, na.rm = T)]
dt <- dt[score_uier > quantile(ss.bulls$score_uier, q, na.rm = T)]
# dt <- dt[score_klauw > 95] # klauw klopt niet helemaal

# remove exterieur uitschieters (vraag Willem)
dt <- dt[ext_beenwerk > quantile(ss.bulls$ext_beenwerk, q, na.rm = T)]
dt <- dt[`ext_beenstand achter` > quantile(ss.bulls$`ext_beenstand achter`, q, na.rm = T)]
dt <- dt[`ext_beenstand zij` > quantile(ss.bulls$`ext_beenstand zij`, q, na.rm = T)]
dt <- dt[ext_klauwhoek > quantile(ss.bulls$ext_klauwhoek, q, na.rm = T)]
dt <- dt[`ext_totaal exterieur` > quantile(ss.bulls$`ext_totaal exterieur`, q, na.rm = T)]

# dt[, n:=.N, by =cow][,n] |> min()
# dt$bull_name|>unique()|>length()

# Let op:



# Index VOF De Pater ------------------------------------------------------
# bedrijfsgebonden index: bgi

# fokkerij focus 
# zwakste schakel bepaalt wanneer een koe het bedrijf verlaat
# focus op hoge gehaltes 
# focus op dat koe lang op bedrijf blijft ()
# laatrijpheid =>



# Set ranking ----------------------------------------------------

# dt[, rank := 1]
# 
# # AAA match 
# dt[match < min_aaa, rank := rank + 3]
# 
# # melkproductie 
# dt[score_prod < min_prod, rank := rank + 1]
# 
# # Eiwit % 
# dt[score_eiwit < min_eiwit, rank := rank + 4]
# 
# # Vet % 
# dt[score_vet < min_vet, rank := rank + 3]
# 
# # Aanhouding 
# dt[score_aanhouding < min_aanhouding , rank := rank + 1]
# 
# # Laatrijpheid 
# dt[score_laatrijpheid < min_laatrijpheid, rank := rank + 2]
# 
# # Persistentie 
# dt[score_persistentie < min_pers, rank := rank + 2]
# 
# # Vruchtbaarheid
# dt[score_vruchtbaarheid < min_vrucht, rank := rank + 1]
# 
# # Uiergezondheid 
# dt[score_uier < min_uier , rank := rank + 1]
# 
# # Klauwgezondheid => klauw klopt niet helemaal
# # dt[score_klauw < min_klauw , rank := rank + 1]
# 
# # Betrouwbaarheid productie
# dt[accuracy < min_betr, rank := rank ]



# Set order ---------------------------------------------------------------

cols <- c('cow', 'bull_name', grep('index_|score_', names(dt), value = T))
# filer top 10 for each cow

dt_tip <- copy(dt)
dt_tip[, n := .N, by = cow]

# als N > 20 => verwijder all AAA matches < 0.8
dt_tip <- dt_tip[!(n>10 & match <0.8)][, n :=.N, by = cow]

# als N > 20 => verwijder all eiwit < 0
dt_tip <- dt_tip[!(n>10 & score_eiwit< 0.1)][, n :=.N, by = cow]

# als N > 20 => verwijder all vet
dt_tip <- dt_tip[!(n>10 & score_vet< 0.1)][, n :=.N, by = cow]
dt_tip <- dt_tip[!(n>10 & score_vet< 100)][, n :=.N, by = cow]

# als N > 20 => verwijder all prod
dt_tip <- dt_tip[!(n>10 & ziekte_celgetal< 100)][, n :=.N, by = cow]
dt_tip <- dt_tip[!(n>10 & score_persistentie < 100)][, n :=.N, by = cow]
dt_tip <- dt_tip[!(n>10 & score_vruchtbaarheid < 100)][, n :=.N, by = cow]
dt_tip <- dt_tip[!(n>10 & score_laatrijpheid < 100)][, n :=.N, by = cow]

# - Bevruchting stier
# - kalvervitaliteit?
# - afkalfgemak
dt_tip <- dt_tip[!(n>5 & score_afkalfgemak <100)][, n := .N, by = cow]
dt_tip <- dt_tip[!(n>5 & match<0.85)][, n := .N, by = cow]
dt_tip <- dt_tip[!(n>5 & ziekte_celgetal<100)][, n := .N, by = cow]
dt_tip <- dt_tip[!(n>5 & score_persistentie<100)][, n := .N, by = cow]
dt_tip <- dt_tip[!(n>5 & score_vruchtbaarheid<100)][, n := .N, by = cow]
dt_tip <- dt_tip[!(n>5 & score_laatrijpheid<100)][, n := .N, by = cow]
dt_tip <- dt_tip[!(n>5 & score_klauw<100)][, n := .N, by = cow]
dt_tip <- dt_tip[!(n>5 & score_melksnelheid<100)][, n := .N, by = cow]


dt_tip <- dt_tip[!(n>3 & score_melksnelheid<100)][, n := .N, by = cow]
dt_tip <- dt_tip[!(n>3 & score_afkalfgemak<100)][, n := .N, by = cow]
dt_tip <- dt_tip[!(n>3 & score_eiwit <0.15)][, n := .N, by = cow]



dt_tip[, rank := frank(-index_inet), by = cow]
# dt_tip <- dt_tip[rank<3]

dt_tip <- dt_tip[, head(.SD, 3), by = cow]

fwrite(dt_tip, 'data/dt_tip.csv')


# Save set ----------------------------------------------------------------

fwrite(dt, 'data/selection.csv')







