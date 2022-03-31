GAZ <- read.csv("https://raw.githubusercontent.com/qwerty29544/RpracticeBook/master/2Data/01FlatTables/GAZ.csv", 
                header = T, 
                sep = "\t", 
                encoding = "UTF-8", 
                dec = ",")

GAZ = na.omit(GAZ)


GAZ$Temp_Kelvin = GAZ[,3] + 273
GAZ = GAZ[,c(1,2,10,4,5,6,7,8,9)]


GAZ[,7] = as.factor(GAZ[,7])
GAZ[,8] = as.factor(GAZ[,8])
GAZ[,9] = as.factor(GAZ[,9])


GAZ$gkk = GAZ$газ.м3.сут/GAZ$конд.т.м3.сут
GAZ$gkv = GAZ$газ.м3.сут/GAZ$вода.м3.сут
GAZ$vkk = GAZ$вода.м3.сут/GAZ$конд.т.м3.сут


dates_GAZ <- as.Date(GAZ$дата.замера, format = "%d/%m/%Y")
GAZ$дата.замера <- dates_GAZ
format(GAZ$дата.замера, "%Y")
GAZ2018 = GAZ[format(GAZ$дата.замера, "%Y") == "2018",]
GAZ[format(GAZ$дата.замера, "%Y") == "2018" & GAZ$ID == 111,]


unique(GAZ$ID)[!unique(GAZ$ID)%in%unique(GAZ[GAZ$вода.м3.сут > 2,]$ID)]


unique(GAZ$ID)[!unique(GAZ$ID)%in%unique(GAZ[GAZ$вода.м3.сут + GAZ$конд.т.м3.сут+ GAZ$газ.м3.сут < 1000,]$ID)]


spl <- split(GAZ2018, GAZ2018$Группа)
sums = c()
for(i in 1:length(spl))
  sums[i] <- sum(spl[[i]]$газ.м3.сут)
names(spl)[which.max(sums)]


sp2 <- split(GAZ2018, GAZ2018$Куст)
sums2 = c()
for(i in 1:length(sp2))
  sums2[i] <- sum(sp2[[i]]$газ.м3.сут)
names(sp2)[which.max(sums2)]

spl3 <- split(GAZ2018, GAZ2018$Куст)
means = c()
for(i in 1:length(spl3)){
  temp <- spl3[[i]]$gkv
  means[i] <- mean(as.numeric(temp[!is.infinite(temp) & !is.nan(temp)]))
}
means
names(spl3)[which.max(means)]

View(GAZ)
