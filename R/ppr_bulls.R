

library(data.table)
library(readxl)
library(pdftools)
library(stringr)
library(rio)



# read bull list

# colnames conform standard format 
colnames <- fread('data/names_new.csv')[, names_4]

url_zb <- 'http://www.nvo-veeverbetering.nl/gallery/TIP%20aug22%20om%20stieren%20te%20selecteren%20zb.xlsx'
url_rb <- 'http://www.nvo-veeverbetering.nl/gallery/TIP%20aug22%20om%20stieren%20te%20selecteren%20rb.xlsx'

dt.bulls_zb <- rio::import(url_zb, skip = 2, col_names = colnames) |> setDT()
dt.bulls_rb <- rio::import(url_rb, skip = 2, col_names = colnames) |> setDT()

dt.bulls <- rbind(dt.bulls_zb, dt.bulls_rb)

# kik genomics advies om wel mee te nemen
dt.kik_gen <- rio::import('C:/Users/Job de Pater/OneDrive/03 Koeien/Stieren/kik_genomic_advies.xlsx') |> setDT()

# define triple A codes
dt.bulls[, aaa_bull := as.numeric(substr(aaa, 1,3))]


# selectie ----------------------------------------------------------------

ss.bulls <- dt.bulls[,.(
  nummer=`levensnummer (itb)`, 
  birth=`geb. datum (d/m/y)`, 
  color_bull=haarkleur,
  bull_name=`volledige naam`,
  bull_dad = `vader`, 
  bulls_moms_dad = `moeders vader`, 
  code= `ki-code...2`, 
  aaa_bull, 
  
  accuracy = `prod_% betr`, 
  score_prod = `prod_kg melk`,
  score_kg_eiwit = `prod_kg eiwit`,
  score_kg_vet = `prod_kg vet`,
  score_eiwit = `prod_% eiwit`,
  score_vet = `prod_% vet` ,
  # score_aanhouding = `aanhouding`,
  score_laatrijpheid = `vrbh_laatrijpheid`,
  score_persistentie = vrbh_persistentie,
  score_vruchtbaarheid = vrbh_vruchtbaarheid,
  score_uier = uier_uiergezondheid,
  score_klauw = klauw_klauwgezondheid, 
  score_melksnelheid = melksnelheid, 
  score_afkalfgemak = vrbh_afkalfgemak, 
  
  index_inet = `inet`, 
  index_inet_nvo = `inet_nvo`,
  index_nvi = nvi, 
  index_tip = tip, 
  ziekte_stofwisseling = stofwisselingsaandoeningen,
  ziekte_reproductie = reproductiestoornissen,
  ziekte_celgetal = uier_celgetal,
  # betr_aanhouding = `% betr aanhouding`,
  `ext_frame`,
  `ext_type`,
  `ext_uier`,
  `ext_beenwerk`,
  `ext_totaal exterieur`,
  `ext_hoogtemaat`,
  `ext_voorhand`,
  `ext_inhoud`,  
  `ext_openheid`,
  `ext_conditie score`,
  `ext_kruisligging`,
  `ext_kruisbreedte`,  
  `ext_beenstand achter`,
  `ext_beenstand zij`,
  `ext_klauwhoek`,
  `ext_voorbeenstand`,
  `ext_beengebruik`,
  `ext_vooruieraanhechting`,
  `ext_voorspeenplaatsing`,
  `ext_speenlengte`,  
  `ext_uierdiepte`,
  `ext_achteruierhoogte`,
  `ext_ophangband`,
  `ext_achterspeenplaatsing`
  

)]


# correctie
ss.bulls <- ss.bulls[, score_klauw := as.numeric(score_klauw)]

# vraag Willem:

# kg ds opname belangrijk?
# Levensproductievererving




# settings and filters -----------------------------------------------------------------

# set date birth
ss.bulls[, birth := year(birth)]

# alleen NL stieren





# KI Kampen info -------------------------------------

# lees pdf van KI Kampen
pdf_kampen <- pdftools::pdf_text('data/220913_stierenkaart_kampen.pdf')
pdf_vallei <- pdftools::pdf_text('data/221027_stierenkaart_vallei.pdf')

# bepaal stieren KIK
bulls_kampen <- stringr::str_extract_all(pdf_kampen, '[0-9]{5,6}') |> unlist() |> as.integer()
bulls_kampen_advies <- c(39909, 39905, 39918, 39888, 39825, 39846, 39979, 39969, 39852, 39910, 39826)

# stieren KI De vallei (hier zitten ook sommige triple A codes bij, maar dat maakt nu even niet uit)
bulls_vallei <- stringr::str_extract_all(pdf_vallei, '[0-9]{5,6} ') |> unlist() |> as.integer()

# add info 
ss.bulls[code %in% bulls_kampen, kik := T]
ss.bulls[code %in% bulls_kampen_advies, kik_adv := T]
ss.bulls[code %in% bulls_vallei, kiv := T]

# add a few KIK stieren ---------------------------------------------------

# add index_tip voor gevallen 

ss.bulls <- ss.bulls |> rbind(dt.kik_gen, fill=T)


# Save --------------------------------------------------------------------


fwrite(ss.bulls, 'data/ss.bulls.csv')
