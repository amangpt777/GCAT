#Setup
# Load from WIP files

setwd( )

source("GLBRC Projects/10_HTS_1/package/test.package/R/load.R")
source("GLBRC Projects/10_HTS_1/package/test.package/R/misc_functions.R")
source("GLBRC Projects/10_HTS_1/package/test.package/R/curve.shapes.R")
source("GLBRC Projects/10_HTS_1/package/test.package/R/logistic.response.new.R")
source("GLBRC Projects/10_HTS_1/package/test.package/R/analyze.main.R")
source("GLBRC Projects/10_HTS_1/package/test.package/R/main.R")
source("GLBRC Projects/10_HTS_1/package/test.package/R/top.wells.R")
# OR:
# R CMD INSTALL "C:/My Documents/GLBRC Projects/10_HTS_1/package/test.package"
library(test.package)

?analyze.screen
?load.data
?test.package
?top.wells
?analyze.main


path = "C:/My Documents/GLBRC Projects/10_HTS_1/data"
file =  "072209a_USDA_screen.csv"
controls= c(paste(LETTERS[1:8],"01",sep=""),paste(LETTERS[1:8],"12",sep=""))

data1 = load.data(, path, file, column.names = c("Plate.ID", "Well", "OD", "Time"),
			time.format = "%Y-%m-%d %H:%M:%S")
output = analyze.screen(data1, scale = 0.5, controls = controls, save.jpegs = F, autopilot = T)

# Step by step:
analyze.screen(data1, scale = 0.5, controls = controls, save.jpegs = F, autopilot = F)

# equivalent (Save JPEGS):
analyze.screen(, path, file, controls = controls, save.jpegs = T, autopilot = F)


# Load another set
file =  "YeastScreenData_ValidationRun_rawdata_4-16-09.csv"
controls=c("A11", "H11", paste(LETTERS[1:8],"01",sep=""), paste(LETTERS[1:8],"12",sep=""))

data2 = load.data(, path, file, column.names = c("Plate.name", "Well", "OD600", "Timestamp"),
			time.format = "%m/%d/%Y %H:%M:%S")
output2 = analyze.screen(data2, scale = 0.5, remove.points = 1, controls = controls, save.jpegs = F, autopilot = T)

# Step by step:
analyze.screen(data2,  well.numbers = 584, scale = 0.5, controls = controls, remove.points = 1, save.jpegs = F, autopilot = F)

# equivalent (save JPEGS):
analyze.screen(, path, file, controls = controls, remove.points = 1:2, save.jpegs = T, autopilot = F)


output2[is.in.character("Error", output2[,4]),1] -> e
output2[is.in.character("above", output2[,4]),1] -> ab

best.wells(output3)




# Load a single plate set
file =  "YeastGrowthUSDAYPD30degrees3-19-09.csv"
controls=c(paste("A0",1:9,sep=""), paste("A",10:12,sep=""),
                 paste("H0",1:9,sep=""), paste("H",10:12,sep=""),
		     "E10", "E11", "F08", "F09", "G08", "G09", "P.7",
		     paste(LETTERS[2:7],"01",sep=""), paste(LETTERS[2:7],"12",sep=""))
data3 = load.data(, path, file, single.plate = T,  single.columns = c("Time", "A1", "H12"), time.format = "%S")

output3 = analyze.screen(data3, scale = 0.5, remove.points = 1:2, controls = controls, save.jpegs = F, autopilot = T)

# Step by step:
analyze.screen(data3, scale = 0.5, controls = controls, remove.points = 1:2, save.jpegs = F, autopilot = F)

# equivalent (save JPEGS):
analyze.screen(, path, file, controls = controls, remove.points = 1:2, save.jpegs = T, autopilot = F)














output = analyze.main(data1, scale = 0.5, controls = control.wells, remove.outliers = TRUE, autopilot = TRUE)
output2 = analyze.main(data1, controls = control.wells, remove.outliers = TRUE, autopilot = FALSE)

summary(output2)

output[is.finite(output$c) & output$c<0, ]$well.number

output[is.finite(output$growth.ratio),]$well.number

out.good.fit = output[output$curve.type == "" | output$curve.type == ": TANKING",]
summary(out.good.fit)
out.good.fit[,desired.columns]

out.lin.fit = output[(output$linear.fit) & (output$curve.type == "" | output$curve.type == ": TANKING"),]
summary(out.lin.fit)
out.lin.fit[,desired.columns]

out.lin.fit = output[(output$linear.fit & output$logistic.fit ) & (output$curve.type == "" | output$curve.type == ": TANKING"),]
out.lin.fit$well.number
out.lin.fit[,desired.columns]

out.log.fit = output[(output$action == "Logistic model only.") & (output$curve.type == "" | output$curve.type == ": TANKING"),]
summary(out.log.fit)
out.log.fit[,desired.columns]

out.no.s = output[output$curve.type == ": NOT S-SHAPE",]
summary(out.no.s[,desired.columns])

attention = output[grepl("(!)", output$remarks) & !output$curve.type == ": TANKING" ,]
summary(attention[,desired.columns])

out.no.fit = output[!(output$logistic.fit | output$linear.fit) & (output$curve.type == "" | output$curve.type == ": TANKING"),]
summary(out.no.fit[,desired.columns])

compare(output[output$total.growth <0,]$well.number)

analyze.main(data1, well.numbers = out.no.s$well.number, controls = control.wells, remove.outliers = T, autopilot = FALSE)

analyze.main(data1, well.numbers = attention$well.number, controls = control.wells, remove.outliers = T, autopilot = FALSE)

results[results$Plate.name == "041609a.GLBRC2000P.1" & results$Well == "G06",]

