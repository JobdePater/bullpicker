library(openxlsx)

wb <- loadWorkbook("data/aAa-Mate - de Pater 21-06.xlsx")
protectWorkbook(wb, protect = FALSE)

openxlsx::protectWorksheet(wb, 'aAa-mate', protect = F)

saveWorkbook(wb,"data/aaa_mate2.xlsx",overwrite = T)

openxlsx::sheets(wb)
