# Memilih file CSV dari laptop
data <- read.csv(file.choose())

# Melihat data
View(data)

# Melihat nama kolom
names(data)

# Melihat jumlah baris dan kolom
dim(data)

# Melihat ringkasan data
summary(data)
# 1. Bersihkan 3 baris teratas (judul tabel BPS yang kosong)
data2 <- data[-(1:3), ]

# 2. Ganti nama kolomnya secara otomatis jadi Provinsi, X1, X2, X3, Y
names(data2) <- c("Provinsi", "X1", "X2", "X3", "Y")

# 3. Ubah tipe data kolom X1 sampai Y dari teks menjadi angka (numeric)
data2[, 2:5] <- lapply(data2[, 2:5], function(x) as.numeric(as.character(x)))

model <- lm(
  Y ~ X1 + X2 + X3,
  data = data2
)
summary(model)

summary(data)

# Instal paket, cukup sekali saja
install.packages(c("lmtest", "car", "sandwich"))

# Memanggil paket
library(lmtest)
library(car)
library(sandwich)

# 1. Normalitas residual
shapiro.test(residuals(model))

# 2. Multikolinearitas
vif(model)

# 3. Heteroskedastisitas
bptest(model)

# 4. Pemeriksaan spesifikasi / linearitas
resettest(model, power = 2:2, type = "fitted")

# 5. Grafik diagnostik
par(mfrow = c(2, 2))
plot(model)

# 6. Uji koefisien dengan standard error robust
coeftest(model, vcov. = vcovHC(model, type = "HC3"))


# Buang 3 baris awal yang bukan data provinsi
data2 <- data[-(1:3), ]

# Ganti nama kolom
names(data2) <- c("Provinsi", "APS_7_12",
                  "APS_13_15", "APS_16_18", "APS_19_23")

# Ubah kolom APS menjadi angka
data2[ , 2:5] <- lapply(data2[ , 2:5],
                         function(x) as.numeric(as.character(x)))

# Periksa hasil
str(data2)
head(data2)



# ==================================================
# ANALISIS REGRESI LINIER BERGANDA DATA APS BPS
# ==================================================

# 1. Membaca file CSV
data_awal <- read.csv(
  file.choose(),
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = TRUE
)

# 2. Memeriksa ukuran dan nama kolom data awal
dim(data_awal)
names(data_awal)
head(data_awal)
tail(data_awal)

# 3. Membuang 3 baris awal yang berisi judul/keterangan
data <- data_awal[-(1:3), ]

# 4. Memberi nama kolom
names(data) <- c(
  "Provinsi",
  "APS_7_12",
  "APS_13_15",
  "APS_16_18",
  "APS_19_23"
)

# 5. Mengubah kolom APS menjadi numerik
kolom_aps <- c(
  "APS_7_12",
  "APS_13_15",
  "APS_16_18",
  "APS_19_23"
)

data[kolom_aps] <- lapply(
  data[kolom_aps],
  function(x) as.numeric(as.character(x))
)

# 6. Menghapus baris yang tidak memiliki nama provinsi
data$Provinsi <- trimws(data$Provinsi)
data <- data[!is.na(data$Provinsi) &
             data$Provinsi != "", ]

# 7. Memeriksa data setelah dibersihkan
dim(data)
str(data)
View(data)

# 8. Memeriksa jumlah dan daftar provinsi
length(data$Provinsi)
data$Provinsi

# 9. Memeriksa nilai kosong
colSums(is.na(data[kolom_aps]))

# 10. Membuat model regresi linier berganda
model <- lm(
  APS_19_23 ~ APS_7_12 +
    APS_13_15 + APS_16_18,
  data = data
)

# 11. Menampilkan hasil regresi
summary(model)

# 12. Mengambil koefisien regresi
coef(model)

# 13. Menampilkan ANOVA model
anova(model)

# 14. Menghitung nilai prediksi
data$Prediksi <- predict(model)

# 15. Menghitung residual
data$Residual <- residuals(model)

# 16. Melihat hasil prediksi dan residual
View(data)

# ==================================================
# UJI ASUMSI REGRESI
# ==================================================

# 17. Memasang paket (cukup sekali jika belum terpasang)
# install.packages(c("lmtest", "car", "sandwich"))

# 18. Memanggil paket
library(lmtest)
library(car)
library(sandwich)

# 19. Uji normalitas residual
shapiro.test(residuals(model))

# 20. Uji multikolinearitas
vif(model)

# 21. Uji heteroskedastisitas
bptest(model)

# 22. Uji spesifikasi model RESET
resettest(model, power = 2:2, type = "fitted")

# 23. Grafik diagnostik model
par(mfrow = c(2, 2))
plot(model)
par(mfrow = c(1, 1))

# 24. Koefisien dengan standard error robust HC3
coeftest(
  model,
  vcov. = vcovHC(model, type = "HC3")
)