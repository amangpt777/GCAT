# R CMD INSTALL "C:/My Documents/GLBRC Projects/10_HTS_1/package/test.package"
library(test.package)

USDA1.controls =c(paste("A0",1:9,sep=""), paste("A",10:12,sep=""),
                 paste("H0",1:9,sep=""), paste("H",10:12,sep=""),
		     "E10", "E11", "F08", "F09", "G08", "G09", "P.7",
		     paste(LETTERS[2:7],"01",sep=""), paste(LETTERS[2:7],"12",sep=""))

Lalle1.controls = c("A11", "H11", paste(LETTERS[1:8],"01",sep=""), 
		paste(LETTERS[1:8],"12",sep=""), "A02")

path = "C:/My Documents/GLBRC Projects/10_HTS_1/data"

# Analyze the Lalle1 Plate
file =  "Lalle1_media1_rep1_082909.csv"

data = load.data(, path, file, column.names = c("Plate.ID", "Well", "OD", "Time"),
			time.format = "%Y-%m-%d %H:%M:%S")
Lalle1 = analyze.screen(data, well.numbers = 1:768, scale = 0.5, remove.points = 1:2, 
				controls = Lalle1.controls , save.jpegs = F, autopilot = T)
write.csv(Lalle1, file = "Lalle1_analysis.csv")

# Step by step
analyze.screen(data, scale = 0.5, remove.points = 1:2, 
				controls = Lalle1.controls , save.jpegs = F, autopilot = F)

#Analyze the USDA1 Plate
file =  "USDA1_Media1_Rep1_061909.csv"
data2 = load.data(, path, file, column.names = c("Plate.ID", "Well", "OD", "Time"),
			time.format = "%Y-%m-%d %H:%M:%S")
USDA1 = analyze.screen(data2, scale = 0.5, remove.points = 1:2, 
				controls = Lalle1.controls , save.jpegs = F, autopilot = T)
write.csv(USDA1, file = "USDA1_analysis.csv")



best.wells(Lalle1, "3.1")



