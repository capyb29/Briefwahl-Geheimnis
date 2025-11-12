
btw2025 = read.csv("../data/btw2025_bezirk.csv", sep = ";")

btw2025 = btw2025[,colSums(is.na(btw2025)) == 0] #removes columns with only NA values
btw2025 = btw2025[-(1:4),] #ersten 4 zeilen löschen
colnames(btw2025) = btw2025[1,] #erste zeile als spaltennamen
btw2025 = btw2025[-1,] #erste zeile löschen