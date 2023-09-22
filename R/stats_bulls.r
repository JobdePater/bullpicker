bulls <- fread('data/selection.csv')


table(bulls$name) |> sort()


s <- bulls[code %in% bulls_kampen]

table(s$name)
