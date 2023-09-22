

library(data.table)
library(readxl)




# read excel table
names1 <- readxl::read_xlsx('data/TIP dec21 om stieren te selecteren zb.xlsx') |> names()
names2 <- readxl::read_xlsx('data/TIP dec21 om stieren te selecteren zb.xlsx', skip = 1) |> names()
dt.names <- data.table(names1, names2)
dt.names[, names1 := gsub('...[0-9]+', '', names1) |> tolower()]
dt.names[, names2 := gsub('...[0-9]+', '', names2) |> tolower()]
dt.names[names1 == '', names3 := names2]
dt.names[names1 != '', names3 := paste0(names2, '_', names1)]
dt.names[duplicated(dt.names[,names3]), names3 := paste0(names3, '_1')]

fwrite(dt.names, 'data/names.csv') 
