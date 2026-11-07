Nama = "Justisia Salmaa Husna"
NIM = "053973385"
Prodi = "Statistika"
UT_Daerah = "45-Yogyakarta"

# Memuat paket yang dibutuhkan
library(ggplot2)
library(dplyr)
library(broom)
install.packages("ggpubr")
library(ggpubr)


install.packages("readxl")
library(readxl)
data_tugas2 <- read_excel("C:/Users/A S U S/OneDrive/Dokumen/Tugas 2 - Semester 5/data tugas 2 AVD.xlsx", skip = 2)
# Cek apakah data sudah masuk
head(data_tugas2)
# Mengubah kolom ke-2 dan ke-3 menjadi format angka (numeric) secara aman
pendapatan_asli <- as.numeric(as.character(data_tugas2[[2]])) * 10000
kebahagiaan <- as.numeric(as.character(data_tugas2[[3]]))

head(pendapatan_asli)
head(kebahagiaan)

# a. Histogram Variabel Terikat (Kebahagiaan)
hist(kebahagiaan, 
     main = "Histogram Variabel Terikat (Kebahagiaan)", 
     xlab = "Kebahagiaan", ylab = "Frekuensi", 
     col = "skyblue", border = "black")

# b. Scatter Plot Hubungan Pendapatan dan Kebahagiaan
plot(pendapatan_asli, kebahagiaan, 
     main = "Scatter Plot Pendapatan vs Kebahagiaan", 
     xlab = "Pendapatan (Nilai Asli)", ylab = "Kebahagiaan", 
     pch = 19, col = "blue")

# c. Regresi Linear Sederhana
model_regresi <- lm(kebahagiaan ~ pendapatan_asli)
summary(model_regresi)

# d. Cek Homoskedastisitas (Residuals vs Fitted)
plot(model_regresi, which = 1)

# e. Visualisasi Hasil Regresi Lengkap (ggplot2)
data_df <- data.frame(pendapatan_asli, kebahagiaan)

ggplot(data_df, aes(x = pendapatan_asli, y = kebahagiaan)) +
  geom_point(color = "blue", alpha = 0.6) +                                    # 1. Plot data points
  geom_smooth(method = "lm", formula = y ~ x, color = "red", se = FALSE) +     # 2. Tambah garis regresi linear
  stat_regline_equation(label.x = 15000, label.y = 6, formula = y ~ x) +       # 3. Tambah persamaan garis linear
  labs(title = "Grafik Regresi Linear Sederhana", 
       x = "Pendapatan", 
       y = "Kebahagiaan") +                                                    # 4. Judul dan label sumbu x & y
  theme_minimal()