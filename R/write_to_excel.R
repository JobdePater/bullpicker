
library(data.table)
library(openxlsx)


# read ordered data -------------------------------------------------------

dt_tip <- fread('data/dt_tip.csv')

dt.sel <- copy(dt_tip)
# renaming
dt.sel[, land:= substr(nummer, 1,3)]
setnames(dt.sel, 'stier', 'bull_current')
dt.sel[, url := paste0('https://apps.crv4all.nl/siresearch/nl/detail/background/', nummer)]


# remove some collumns
dt.sel[, c('color_cow', 'nummer', 'birth', 'color_bull', 'no') := NULL]

# setcolorder(dt.sel, c('cow', 'bull_current', 'code', 'name', 'kik', 'kiv', 'rank', 'index_tip'))


# create bull stats -------------------------------------------------------
dt.bull_stat <- dt.sel[,.(n_1 = sum(ifelse(rank==1, 1, 0)), 
                          n_2 = sum(ifelse(rank==2, 1, 0)), 
                          n_3 = sum(ifelse(rank==3, 1, 0)), 
                          cows = .(paste0(cow, '(', rank, ')'))), 
                       by = .(land, bull_name, code, aaa_bull,accuracy, score_prod, score_eiwit, 
                              score_vet, score_aanhouding, score_laatrijpheid, score_persistentie, 
                              score_vruchtbaarheid, index_inet, index_inet_nvo, index_nvi, index_tip,
                              kik, kiv, url)
                       ]
                              
dt.bull_stat <- dt.bull_stat[order(-n_1)]


# url naar stierzoeker
names(dt.bull_stat$url) <- 'url'
class(dt.bull_stat$url) <- "hyperlink"
names(dt.bull_stat$url) <- dt.bull_stat$name


# create cow stats --------------------------------------------------------

dt.cow_stat <- dt.sel[,.(.N, 
                         kik = sum(kik, na.rm = T),
                         kiv = sum(kiv, na.rm = T), 
                         bulls = .(bull_name)
                         ), by = .(cow)]



# select only relevant collumns -------------------------------------------

wb <- loadWorkbook('C:/Users/Job de Pater/OneDrive/03 Koeien/Stieren/221022_cows.xlsx')
addWorksheet(wb, 'pairs')
addWorksheet(wb, 'bullstats')
addWorksheet(wb, 'cowstats')
writeData(wb, sheet = "pairs", dt.sel, colNames = T)
writeData(wb, sheet = "bullstats", dt.bull_stat, colNames = T)
writeData(wb, sheet = "cowstats", dt.cow_stat, colNames = T)

setColWidths(wb, sheet = 'pairs', cols = 1:8, widths = "auto")
activeSheet(wb) <- 'pairs'
addFilter(wb, 'pairs', row = 1, cols = 1:ncol(dt.sel))





now <- format(Sys.time(), '%y%m%d%H%M')

saveWorkbook(wb,paste0('C:/Users/Job de Pater/OneDrive/03 Koeien/Stieren/', now, '_pairs.xlsx'),overwrite = T)
