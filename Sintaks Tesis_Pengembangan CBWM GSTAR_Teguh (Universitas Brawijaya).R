# =====================================================================
# ANALISIS TESIS KEMISKINAN PROVINSI DI SULAWESI
# ST-DBSCAN + CLUSTER-BASED WEIGHT MATRIX (CBWM) + GSTAR
# =====================================================================
rm(list=ls()); graphics.off()
packages <- c("sf","dplyr","tidyr","readxl","dbscan","cluster","clusterSim","ggplot2","stringr","openxlsx","reshape2","MASS","forecast","tseries","purrr")
installed <- rownames(installed.packages())
for(pkg in setdiff(packages,installed)) install.packages(pkg,dependencies=TRUE)
invisible(lapply(packages,library,character.only=TRUE))
options(stringsAsFactors=FALSE,scipen=999); sf::sf_use_s2(FALSE)
# =====================================================================
# PERSIAPAN DATA DAN DIREKTORI
# =====================================================================
shp_path <- "C:/Users/ASUS/Downloads/BATAS PROVINSI DESEMBER 2019 DUKCAPIL/BATAS_PROVINSI_DESEMBER_2019_DUKCAPIL.shp"
file_data <- "C:/Users/ASUS/Downloads/Data_Panel_Kemiskinan_Indonesia.xlsx"
folder_hasil <- "hasil_Tesis_Kemiskinan_Sulawesi"
folder_visual <- file.path(folder_hasil,"visualisasi_parameter_STDBSCAN")
dir.create(folder_hasil,showWarnings=FALSE,recursive=TRUE); dir.create(folder_visual,showWarnings=FALSE,recursive=TRUE)
if(!file.exists(file_data)) stop(paste0("File data tidak ditemukan: ",file_data))
if(!file.exists(shp_path)) stop(paste0("Shapefile tidak ditemukan: ",shp_path))
prov_sulawesi <- c("SULAWESI UTARA","GORONTALO","SULAWESI TENGAH","SULAWESI BARAT","SULAWESI SELATAN","SULAWESI TENGGARA")
prov_sf <- sf::st_read(shp_path,quiet=TRUE) %>% sf::st_make_valid() %>% sf::st_collection_extract("POLYGON") %>% sf::st_transform(3857) %>%
  dplyr::mutate(provinsi=toupper(trimws(PROVINSI))) %>% dplyr::filter(provinsi %in% prov_sulawesi)
point_sf <- sf::st_point_on_surface(prov_sf); coord <- sf::st_coordinates(point_sf)
centroid_prov <- point_sf %>% dplyr::mutate(x=coord[,1],y=coord[,2]) %>% sf::st_drop_geometry() %>% dplyr::select(provinsi,x,y)
raw <- readxl::read_excel(file_data,sheet="Data",col_names=FALSE)
periode <- as.character(unlist(raw[3,-1])); data <- raw[-c(1:3),]; colnames(data) <- c("Provinsi",periode)
data$Provinsi <- stringr::str_squish(as.character(data$Provinsi)); data <- data %>% dplyr::filter(!is.na(Provinsi),Provinsi!="")
data_long <- data %>% tidyr::pivot_longer(cols=-Provinsi,names_to="Periode",values_to="kemiskinan") %>% dplyr::mutate(kemiskinan=as.numeric(kemiskinan))
parse_periode <- function(x) tibble::tibble(Periode=x,tahun=as.integer(stringr::str_extract(x,"^[0-9]{4}")),semester=dplyr::case_when(grepl("Semester 1",x)~1,grepl("Semester 2",x)~2,TRUE~NA_real_))
data_long <- data_long %>% dplyr::left_join(parse_periode(unique(data_long$Periode)),by="Periode") %>% dplyr::arrange(Provinsi,tahun,semester) %>%
  dplyr::mutate(waktu=tahun+(semester-1)/2,Provinsi=toupper(trimws(Provinsi))) %>% dplyr::filter(Provinsi %in% prov_sulawesi)
df <- data_long %>% dplyr::left_join(centroid_prov,by=c("Provinsi"="provinsi"))
cat("\nDATA PANEL SULAWESI\nJumlah observasi:",nrow(df),"\nJumlah provinsi:",dplyr::n_distinct(df$Provinsi),"\nPeriode:",min(df$tahun),"-",max(df$tahun),"\n")
# =====================================================================
# IV.2 DESKRIPSI DATA PENELITIAN
# =====================================================================
# 4.2.1 Karakteristik Tingkat Kemiskinan Provinsi di Sulawesi
ringkasan_kemiskinan <- df %>% dplyr::group_by(Provinsi) %>% dplyr::summarise(
  Jumlah_Observasi=sum(!is.na(kemiskinan)),Rata_rata=mean(kemiskinan,na.rm=TRUE),Simpangan_Baku=sd(kemiskinan,na.rm=TRUE),
  Minimum=min(kemiskinan,na.rm=TRUE),Maksimum=max(kemiskinan,na.rm=TRUE),.groups="drop")
cat("\n4.2.1 KARAKTERISTIK TINGKAT KEMISKINAN\n"); print(ringkasan_kemiskinan)
openxlsx::write.xlsx(ringkasan_kemiskinan,file.path(folder_hasil,"4.2.1_Karakteristik_Kemiskinan.xlsx"),overwrite=TRUE)
# 4.2.2 Pola Perkembangan Tingkat Kemiskinan Provinsi di Sulawesi
df_tren <- df %>% dplyr::arrange(Provinsi,tahun,semester)
ringkasan_tren <- df_tren %>% dplyr::group_by(Provinsi) %>% dplyr::summarise(
  Periode_Awal=first(Periode),Kemiskinan_Awal=first(kemiskinan),Periode_Akhir=last(Periode),Kemiskinan_Akhir=last(kemiskinan),
  Perubahan=Kemiskinan_Akhir-Kemiskinan_Awal,.groups="drop")
cat("\n4.2.2 POLA PERKEMBANGAN TINGKAT KEMISKINAN\n"); print(ringkasan_tren)
openxlsx::write.xlsx(list(Data_Perkembangan=df_tren %>% dplyr::select(Provinsi,Periode,tahun,semester,kemiskinan),Ringkasan=ringkasan_tren),
                     file.path(folder_hasil,"4.2.2_Pola_Perkembangan_Kemiskinan.xlsx"),overwrite=TRUE)
# =====================================================================
# IV.3 PEMBENTUKAN KLASTER SPASIO-TEMPORAL MENGGUNAKAN ST-DBSCAN
# REVISI: ST-DBSCAN DAN PEMILIHAN PARAMETER HANYA MENGGUNAKAN TRAINING
# =====================================================================
# 4.3.0 Pemisahan data training dan testing
# Tiga semester terakhir disimpan sebagai testing dan tidak digunakan
# dalam standardisasi, diagnostik, grid search, clustering, atau pemilihan
# cluster dominan.
n_test_semesters <- 3L
waktu_unik <- sort(unique(df$waktu[!is.na(df$waktu)]))
if (length(waktu_unik) <= n_test_semesters) {
  stop("Jumlah semester tidak cukup untuk memisahkan training dan testing.")
}
waktu_train <- waktu_unik[seq_len(length(waktu_unik) - n_test_semesters)]
waktu_test <- waktu_unik[
  (length(waktu_unik) - n_test_semesters + 1L):length(waktu_unik)
]
df_train <- df %>% dplyr::filter(waktu %in% waktu_train)
df_test <- df %>% dplyr::filter(waktu %in% waktu_test)
cat("\nPEMISAHAN DATA TRAINING DAN TESTING\n")
cat("Jumlah semester training:", length(waktu_train), "\n")
cat("Jumlah semester testing :", length(waktu_test), "\n")
cat("Periode training        :", paste(waktu_train, collapse = ", "), "\n")
cat("Periode testing         :", paste(waktu_test, collapse = ", "), "\n")
cek_train <- df_train %>% dplyr::count(Provinsi, name = "jumlah_semester")
print(cek_train)
if (any(cek_train$jumlah_semester != length(waktu_train))) {
  stop("Panel training tidak seimbang: jumlah semester berbeda antarprovinsi.")
}
if (anyDuplicated(df_train[, c("Provinsi", "waktu")]) > 0) {
  stop("Terdapat kombinasi Provinsi-waktu duplikat pada data training.")
}
# Data testing hanya disimpan untuk tahap evaluasi peramalan selanjutnya.
# Jangan gunakan df_test untuk memilih parameter atau membentuk cluster.
# ---------------------------------------------------------------------
# 4.3.1 Standardisasi data tingkat kemiskinan
# Rata-rata dan simpangan baku dihitung dari data training saja.
# ---------------------------------------------------------------------
df_train_clean <- df_train %>%
  dplyr::filter(
    !is.na(kemiskinan),
    !is.na(waktu),
    !is.na(x),
    !is.na(y)
  )
mean_train <- mean(df_train_clean$kemiskinan, na.rm = TRUE)
sd_train <- stats::sd(df_train_clean$kemiskinan, na.rm = TRUE)
if (!is.finite(sd_train) || sd_train == 0) {
  stop("Simpangan baku kemiskinan pada data training tidak valid.")
}
df_train_std <- df_train_clean %>%
  dplyr::mutate(z_kemiskinan = (kemiskinan - mean_train) / sd_train)
hasil_std <- df_train_std %>%
  dplyr::select(Provinsi, tahun, semester, waktu, kemiskinan, z_kemiskinan)
cat("\n4.3.1 HASIL STANDARDISASI DATA TRAINING\n")
print(hasil_std, n = Inf, width = Inf)
data_cluster <- df_train_std %>%
  dplyr::select(
    provinsi = Provinsi,
    tahun,
    waktu,
    x,
    y,
    z_kemiskinan
  )
openxlsx::write.xlsx(
  hasil_std,
  file.path(folder_hasil, "4.3.1_Standardisasi_Kemiskinan_TRAINING.xlsx"),
  overwrite = TRUE
)
# Simpan parameter standardisasi training untuk dokumentasi dan replikasi.
parameter_standardisasi <- data.frame(
  parameter = c("mean_kemiskinan_training", "sd_kemiskinan_training"),
  nilai = c(mean_train, sd_train)
)
openxlsx::write.xlsx(
  parameter_standardisasi,
  file.path(folder_hasil, "4.3.1_Parameter_Standardisasi_TRAINING.xlsx"),
  overwrite = TRUE
)
# ---------------------------------------------------------------------
# 4.3.2 Optimasi parameter ST-DBSCAN
# Seluruh diagnostik berbasis deret kemiskinan menggunakan data training.
# ---------------------------------------------------------------------
# 4.3.2.1 Penentuan parameter epsilon spasial
# Koordinat berasal dari shapefile dan bersifat tetap secara geografis.
coords_df <- data_cluster %>%
  dplyr::distinct(provinsi, x, y) %>%
  dplyr::arrange(provinsi)
coords <- as.matrix(coords_df[, c("x", "y")])
if (nrow(coords) <= 3) {
  stop("Jumlah provinsi harus lebih besar dari k untuk menghitung kNN distance.")
}
spatial_dist <- stats::dist(coords) / 1000
k <- 3
kNN_spatial <- dbscan::kNNdist(coords, k = k) / 1000
q_spatial <- stats::quantile(
  kNN_spatial,
  probs = c(.50, .75, .90),
  na.rm = TRUE
)
cat("\n4.3.2.1 SPATIAL THRESHOLD\n")
print(summary(as.vector(spatial_dist)))
print(round(q_spatial, 2))
kdist_spatial <- data.frame(
  Observation = seq_along(sort(kNN_spatial)),
  Distance = sort(kNN_spatial)
)
q_spatial_df <- data.frame(
  Kuantil = factor(names(q_spatial), levels = names(q_spatial)),
  Distance = as.numeric(q_spatial)
)
p_spatial <- ggplot2::ggplot(kdist_spatial, ggplot2::aes(Observation, Distance)) +
  ggplot2::geom_line(linewidth = .8) +
  ggplot2::geom_hline(
    data = q_spatial_df,
    ggplot2::aes(yintercept = Distance, linetype = Kuantil),
    inherit.aes = FALSE, linewidth = .45, alpha = .8, color = "grey35"
  ) +
  ggplot2::scale_linetype_manual(
    name = "Kuantil",
    values = c("50%" = "dashed", "75%" = "dashed", "90%" = "dashed")
  ) +
  ggplot2::labs(
    title = "Spatial K-Distance Plot",
    subtitle = "Kuantil 50%, 75%, dan 90% untuk jarak spasial training",
    x = "Observasi", y = "k-Distance (km)"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(legend.position = "bottom")
print(p_spatial)
ggplot2::ggsave(
  file.path(folder_visual, "4.3.2.1_Spatial_Threshold_TRAINING.png"),
  p_spatial,
  width = 8,
  height = 6,
  dpi = 300,
  bg = "white"
)
# 4.3.2.2 Penentuan parameter epsilon temporal
# Catatan: waktu dibentuk sebagai tahun + (semester - 1)/2,
# sehingga satu unit waktu berarti satu tahun.
df_temp <- data_cluster %>%
  dplyr::arrange(provinsi, waktu)
acf_list <- list()
prov_list <- unique(df_temp$provinsi)
for (p in prov_list) {
  ts_data <- df_temp %>%
    dplyr::filter(provinsi == p) %>%
    dplyr::arrange(waktu) %>%
    dplyr::pull(z_kemiskinan)
  if (length(ts_data) < 2 || stats::sd(ts_data, na.rm = TRUE) == 0) {
    acf_list[[p]] <- rep(NA_real_, 6)
  } else {
    acf_list[[p]] <- tryCatch(
      as.numeric(stats::acf(ts_data, lag.max = 5, plot = FALSE)$acf),
      error = function(e) rep(NA_real_, 6)
    )
  }
}
acf_matrix <- do.call(cbind, acf_list)
acf_mean <- rowMeans(acf_matrix, na.rm = TRUE)
lags <- 0:(length(acf_mean) - 1)
n_obs <- min(table(df_temp$provinsi))
acf_threshold <- 1.96 / sqrt(n_obs)
acf_df <- data.frame(
  Lag = lags,
  ACF = acf_mean,
  Batas_Signifikan = acf_threshold
) %>%
  dplyr::mutate(
    Signifikan = ifelse(
      Lag == 0,
      "-",
      ifelse(
        abs(ACF) > Batas_Signifikan,
        "Signifikan",
        "Tidak Signifikan"
      )
    )
  )
cat("\n4.3.2.2 TEMPORAL THRESHOLD (TRAINING ONLY)\n")
print(acf_df)
cat("\nLag signifikan:\n")
print(
  acf_df %>%
    dplyr::filter(
      Lag > 0,
      !is.na(ACF),
      abs(ACF) > Batas_Signifikan
    )
)
p_temporal <- ggplot2::ggplot(acf_df, ggplot2::aes(Lag, ACF)) +
  ggplot2::geom_segment(
    ggplot2::aes(xend = Lag, yend = 0),
    linewidth = 1.2
  ) +
  ggplot2::geom_hline(
    yintercept = c(-acf_threshold, acf_threshold),
    linetype = "dashed",
    linewidth = .7
  ) +
  ggplot2::geom_hline(yintercept = 0) +
  ggplot2::labs(
    title = "Average Autocorrelation Function",
    subtitle = "Determination of Temporal Threshold Using Training Data",
    x = "Lag",
    y = "Average ACF"
  ) +
  ggplot2::theme_minimal(base_size = 11)
print(p_temporal)
ggplot2::ggsave(
  file.path(folder_visual, "4.3.2.2_Temporal_Threshold_TRAINING.png"),
  p_temporal,
  width = 8,
  height = 6,
  dpi = 300,
  bg = "white"
)
# 4.3.2.3 Penentuan parameter epsilon atribut
# Distribusi jarak atribut dan k-distance dihitung hanya dari data training.
attr_data <- data_cluster %>% dplyr::select(z_kemiskinan)
Da_attr <- as.vector(stats::dist(attr_data))
q_attr <- stats::quantile(Da_attr, probs = c(.50, .75, .90), na.rm = TRUE)
cat("\n4.3.2.3 ATTRIBUTE THRESHOLD (TRAINING ONLY)\n")
print(summary(Da_attr)); print(round(q_attr, 3))
# Plot distribusi seluruh jarak berpasangan atribut.
df_attr_dist <- data.frame(Distance = Da_attr)
q_attr_df <- data.frame(Kuantil = factor(names(q_attr), levels = names(q_attr)),
                        Distance = as.numeric(q_attr))
p_attr <- ggplot2::ggplot(df_attr_dist, ggplot2::aes(Distance)) +
  ggplot2::geom_histogram(bins = 30, fill = "grey80", color = "grey35") +
  ggplot2::geom_vline(
    data = q_attr_df,
    ggplot2::aes(xintercept = Distance, linetype = Kuantil),
    inherit.aes = FALSE, linewidth = .45, alpha = .8, color = "grey35"
  ) +
  ggplot2::scale_linetype_manual(
    name = "Kuantil",
    values = c("50%" = "dashed", "75%" = "dashed", "90%" = "dashed")
  ) +
  ggplot2::labs(
    title = "Distribusi Jarak Atribut",
    subtitle = "Jarak berpasangan z_kemiskinan pada data training",
    x = "Jarak Atribut", y = "Frekuensi"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(legend.position = "bottom")
print(p_attr)
ggplot2::ggsave(file.path(folder_visual, "4.3.2.3a_Distribusi_Jarak_Atribut_TRAINING.png"),
                p_attr, width = 8, height = 6, dpi = 300, bg = "white")
# Plot k-distance atribut berdasarkan jarak ke tetangga ke-k.
if (nrow(attr_data) <= k) stop("Jumlah observasi training harus lebih besar dari k untuk k-distance atribut.")
kNN_attribute <- dbscan::kNNdist(as.matrix(attr_data), k = k)
q_kdist_attribute <- stats::quantile(kNN_attribute, probs = c(.50, .75, .90), na.rm = TRUE)
kdist_attribute <- data.frame(Observation = seq_along(kNN_attribute), Distance = sort(kNN_attribute))
q_kdist_attribute_df <- data.frame(
  Kuantil = factor(names(q_kdist_attribute), levels = names(q_kdist_attribute)),
  Distance = as.numeric(q_kdist_attribute)
)
cat("\nK-DISTANCE ATRIBUT (TRAINING ONLY)\n"); print(round(q_kdist_attribute, 4))
p_kdist_attribute <- ggplot2::ggplot(kdist_attribute, ggplot2::aes(Observation, Distance)) +
  ggplot2::geom_line(linewidth = .8) +
  ggplot2::geom_hline(
    data = q_kdist_attribute_df,
    ggplot2::aes(yintercept = Distance, linetype = Kuantil),
    inherit.aes = FALSE, linewidth = .45, alpha = .8, color = "grey35"
  ) +
  ggplot2::scale_linetype_manual(
    name = "Kuantil",
    values = c("50%" = "dashed", "75%" = "dashed", "90%" = "dashed")
  ) +
  ggplot2::labs(
    title = "Attribute K-Distance Plot",
    subtitle = "Kuantil 50%, 75%, dan 90% untuk jarak atribut training",
    x = "Observasi", y = "k-Distance Atribut"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(legend.position = "bottom")
print(p_kdist_attribute)
ggplot2::ggsave(file.path(folder_visual, "4.3.2.3b_KDistance_Atribut_TRAINING.png"),
                p_kdist_attribute, width = 8, height = 6, dpi = 300, bg = "white")
# 4.3.2.4 Kandidat parameter MinPts dan grid search
# Grid kandidat terdiri atas 81 kombinasi (3 x 3 x 3 x 3).
# Nilai kandidat harus dianggap ditetapkan sebelum melihat data testing.
# eps_1 dalam km; eps_2 dalam satuan waktu "tahun"; eps_3 dalam satuan z.
eps_1_final <- c(390, 450, 536)
eps_2_final <- c(1, 2, 3)
eps_3_final <- c(1, 1.6, 2.3)
MinPts_final <- c(3L, 4L, 5L)
cat("\n4.3.2.4 KANDIDAT PARAMETER\n")
print(list(
  eps_1_km = eps_1_final,
  eps_2_tahun = eps_2_final,
  eps_3_z = eps_3_final,
  MinPts = MinPts_final
))
# ---------------------------------------------------------------------
# Matriks jarak dibangun hanya dari observasi training.
# ---------------------------------------------------------------------
coords_all <- as.matrix(data_cluster[, c("x", "y")])
Ds <- as.matrix(stats::dist(coords_all)) / 1000
Dt <- as.matrix(stats::dist(data_cluster$waktu))
Da <- as.matrix(stats::dist(data_cluster[, "z_kemiskinan", drop = FALSE]))
max_Ds <- max(Ds, na.rm = TRUE)
max_Dt <- max(Dt, na.rm = TRUE)
max_Da <- max(Da, na.rm = TRUE)
if (any(!is.finite(c(max_Ds, max_Dt, max_Da))) ||
    any(c(max_Ds, max_Dt, max_Da) <= 0)) {
  stop("Matriks jarak tidak dapat dinormalisasi; periksa koordinat, waktu, dan data training.")
}
Ds_norm <- Ds / max_Ds
Dt_norm <- Dt / max_Dt
Da_norm <- Da / max_Da
# Fungsi ST-DBSCAN
st_dbscan <- function(Ds, Dt, Da, eps_1, eps_2, eps_3, MinPts) {
  n <- nrow(Ds)
  cluster <- rep(0L, n)
  visited <- rep(FALSE, n)
  cluster_id <- 0L
  for (i in seq_len(n)) {
    if (visited[i]) next
    visited[i] <- TRUE
    neighbors <- which(
      Ds[i, ] <= eps_1 &
        Dt[i, ] <= eps_2 &
        Da[i, ] <= eps_3
    )
    if (length(neighbors) < MinPts) {
      cluster[i] <- -1L
    } else {
      cluster_id <- cluster_id + 1L
      cluster[i] <- cluster_id
      seeds <- neighbors
      while (length(seeds) > 0) {
        j <- seeds[1]
        seeds <- seeds[-1]
        if (!visited[j]) {
          visited[j] <- TRUE
          neighbors_j <- which(
            Ds[j, ] <= eps_1 &
              Dt[j, ] <= eps_2 &
              Da[j, ] <= eps_3
          )
          if (length(neighbors_j) >= MinPts) {
            seeds <- union(seeds, neighbors_j)
          }
        }
        if (cluster[j] <= 0L) {
          cluster[j] <- cluster_id
        }
      }
    }
  }
  cluster
}
# ---------------------------------------------------------------------
# 4.3.3 Evaluasi kualitas cluster dengan grid search
# Kombinasi yang tidak valid diberi NA dan tidak boleh memenangkan ranking.
# ---------------------------------------------------------------------
# Matriks dissimilarity komposit untuk evaluasi SC dan DBCV.
# Menggabungkan jarak spasial, temporal, dan atribut yang telah dinormalisasi.
D_combined_all <- Ds_norm + Dt_norm + Da_norm
diag(D_combined_all) <- 0
# DBCV menggunakan label 0 untuk noise. ST-DBSCAN pada skrip ini
# menggunakan -1 untuk noise, sehingga label dikonversi sebelum evaluasi.
# d_dimension = 4 merepresentasikan x, y, waktu, dan kemiskinan.
calculate_dbcv <- function(cluster_vector, distance_matrix, d_dimension = 4L) {
  cluster_vector <- as.integer(cluster_vector)
  cluster_for_dbcv <- ifelse(cluster_vector > 0L, cluster_vector, 0L)
  ukuran_cluster <- table(cluster_for_dbcv[cluster_for_dbcv > 0L])
  cluster_valid_dbcv <- sum(ukuran_cluster >= 3L)
  # DBCV memerlukan minimal dua cluster non-noise dengan setidaknya 3 anggota.
  if (cluster_valid_dbcv < 2L) return(NA_real_)
  hasil_dbcv <- tryCatch(
    suppressWarnings(
      dbscan::dbcv(
        x = stats::as.dist(distance_matrix),
        cl = cluster_for_dbcv,
        d = d_dimension
      )
    ),
    error = function(e) NULL
  )
  if (is.null(hasil_dbcv) || is.null(hasil_dbcv$score)) {
    return(NA_real_)
  }
  skor <- as.numeric(hasil_dbcv$score)[1]
  if (!is.finite(skor)) return(NA_real_)
  skor
}
results <- data.frame()
counter <- 0L
total_comb <- length(eps_1_final) *
  length(eps_2_final) *
  length(eps_3_final) *
  length(MinPts_final)
cat(
  "\n4.3.3 GRID SEARCH ST-DBSCAN (TRAINING ONLY) | Total kombinasi:",
  total_comb,
  "\n"
)
for (eps_1 in eps_1_final) {
  for (eps_2 in eps_2_final) {
    for (eps_3 in eps_3_final) {
      for (MinPts in MinPts_final) {
        counter <- counter + 1L
        if (counter %% 50 == 0) {
          cat("Proses:", counter, "/", total_comb, "\n")
        }
        cl <- st_dbscan(
          Ds_norm,
          Dt_norm,
          Da_norm,
          eps_1 / max_Ds,
          eps_2 / max_Dt,
          eps_3 / max_Da,
          MinPts
        )
        n_cluster <- length(unique(cl[cl > 0]))
        n_noise <- sum(cl == -1)
        pct_noise <- 100 * n_noise / length(cl)
        idx <- which(cl > 0)
        # Kandidat tidak valid jika cluster terlalu sedikit, observasi
        # non-noise terlalu sedikit, atau proporsi noise terlalu besar.
        if (length(idx) <= 10 || n_cluster < 2 || pct_noise > 50) {
          results <- rbind(
            results,
            data.frame(
              eps_1 = eps_1,
              eps_2 = eps_2,
              eps_3 = eps_3,
              MinPts = MinPts,
              cluster = n_cluster,
              noise = n_noise,
              pct_noise = pct_noise,
              SC = NA_real_,
              DBI = NA_real_,
              DBCV = NA_real_
            )
          )
          next
        }
        D_combined <- Ds_norm[idx, idx, drop = FALSE] +
          Dt_norm[idx, idx, drop = FALSE] +
          Da_norm[idx, idx, drop = FALSE]
        SC <- tryCatch(
          mean(
            cluster::silhouette(
              cl[idx],
              stats::as.dist(D_combined)
            )[, 3],
            na.rm = TRUE
          ),
          error = function(e) NA_real_
        )
        cluster_eval <- data.frame(
          x = as.numeric(scale(data_cluster$x[idx])),
          y = as.numeric(scale(data_cluster$y[idx])),
          t = as.numeric(scale(data_cluster$waktu[idx])),
          poverty = as.numeric(scale(data_cluster$z_kemiskinan[idx]))
        )
        DBI <- tryCatch(
          clusterSim::index.DB(cluster_eval, cl[idx])$DB,
          error = function(e) NA_real_
        )
        # DBCV dihitung pada matriks dissimilarity komposit.
        # Label noise ST-DBSCAN (-1) dikonversi menjadi 0 di fungsi calculate_dbcv().
        DBCV <- calculate_dbcv(
          cluster_vector = cl,
          distance_matrix = D_combined_all,
          d_dimension = 4L
        )
        results <- rbind(
          results,
          data.frame(
            eps_1 = eps_1,
            eps_2 = eps_2,
            eps_3 = eps_3,
            MinPts = MinPts,
            cluster = n_cluster,
            noise = n_noise,
            pct_noise = pct_noise,
            SC = SC,
            DBI = DBI,
            DBCV = DBCV
          )
        )
      }
    }
  }
}
# Normalisasi indeks hanya pada nilai yang valid.
# SC dan DBCV: semakin besar semakin baik; DBI: semakin kecil semakin baik.
normalize_metric <- function(x, higher_better = TRUE) {
  out <- rep(NA_real_, length(x))
  valid <- is.finite(x)
  if (!any(valid)) return(out)
  rng <- range(x[valid])
  if (diff(rng) == 0) {
    out[valid] <- 1
  } else if (higher_better) {
    out[valid] <- (x[valid] - rng[1]) / diff(rng)
  } else {
    out[valid] <- (rng[2] - x[valid]) / diff(rng)
  }
  out
}
results$SC_norm <- normalize_metric(results$SC, higher_better = TRUE)
results$DBI_norm <- normalize_metric(results$DBI, higher_better = FALSE)
results$DBCV_norm <- normalize_metric(results$DBCV, higher_better = TRUE)
results$total_score <- results$SC_norm +
  results$DBI_norm +
  results$DBCV_norm
results_valid <- results %>%
  dplyr::filter(
    is.finite(SC),
    is.finite(DBI),
    is.finite(DBCV),
    is.finite(total_score)
  )
if (nrow(results_valid) == 0) {
  openxlsx::write.xlsx(
    results,
    file.path(folder_hasil, "4.3.3_Hasil_Grid_Search_STDBSCAN_SC_DBI_DBCV_TRAINING.xlsx"),
    overwrite = TRUE
  )
  stop(
    "Tidak ada kombinasi parameter ST-DBSCAN yang valid. ",
    "Periksa rentang epsilon, MinPts, dan struktur data training."
  )
}
best_result <- results_valid %>%
  dplyr::arrange(dplyr::desc(total_score)) %>%
  dplyr::slice(1)
cat("\nPARAMETER TERBAIK ST-DBSCAN (TRAINING ONLY)\n")
print(best_result)
openxlsx::write.xlsx(
  results,
  file.path(folder_hasil, "4.3.3_Hasil_Grid_Search_STDBSCAN_SC_DBI_DBCV_TRAINING.xlsx"),
  overwrite = TRUE
)
# ---------------------------------------------------------------------
# 4.3.4 Hasil clustering spasio-temporal pada data training
# ---------------------------------------------------------------------
final_cluster <- st_dbscan(
  Ds_norm,
  Dt_norm,
  Da_norm,
  best_result$eps_1 / max_Ds,
  best_result$eps_2 / max_Dt,
  best_result$eps_3 / max_Da,
  best_result$MinPts
)
data_cluster$cluster <- final_cluster
data_cluster$cluster_label <- factor(
  ifelse(
    final_cluster == -1,
    "Noise",
    paste0("Cluster ", final_cluster)
  )
)
jumlah_cluster <- length(unique(final_cluster[final_cluster > 0]))
jumlah_noise <- sum(final_cluster == -1)
persen_noise <- round(100 * jumlah_noise / length(final_cluster), 2)
idx_final <- which(final_cluster > 0)
sil_score <- NA_real_
if (length(idx_final) > 1 &&
    length(unique(final_cluster[idx_final])) > 1) {
  D_final <- Ds_norm[idx_final, idx_final, drop = FALSE] +
    Dt_norm[idx_final, idx_final, drop = FALSE] +
    Da_norm[idx_final, idx_final, drop = FALSE]
  sil_score <- tryCatch(
    mean(
      cluster::silhouette(
        final_cluster[idx_final],
        stats::as.dist(D_final)
      )[, 3],
      na.rm = TRUE
    ),
    error = function(e) NA_real_
  )
}
# Hitung ketiga indeks validasi untuk hasil clustering final.
dbi_score_final <- NA_real_
if (length(idx_final) > 1 &&
    length(unique(final_cluster[idx_final])) > 1) {
  cluster_eval_final <- data.frame(
    x = as.numeric(scale(data_cluster$x[idx_final])),
    y = as.numeric(scale(data_cluster$y[idx_final])),
    t = as.numeric(scale(data_cluster$waktu[idx_final])),
    poverty = as.numeric(scale(data_cluster$z_kemiskinan[idx_final]))
  )
  dbi_score_final <- tryCatch(
    clusterSim::index.DB(cluster_eval_final, final_cluster[idx_final])$DB,
    error = function(e) NA_real_
  )
}
dbcv_score_final <- calculate_dbcv(
  cluster_vector = final_cluster,
  distance_matrix = D_combined_all,
  d_dimension = 4L
)
validasi_final <- data.frame(
  Indeks = c("SC", "DBI", "DBCV"),
  Nilai = c(sil_score, dbi_score_final, dbcv_score_final)
)
# Join hanya dengan data training, bukan seluruh df.
data_cluster_final <- data_cluster %>%
  dplyr::left_join(
    df_train_clean %>%
      dplyr::select(Provinsi, waktu, kemiskinan),
    by = c("provinsi" = "Provinsi", "waktu" = "waktu")
  )
cluster_char <- data_cluster_final %>%
  dplyr::group_by(cluster_label) %>%
  dplyr::summarise(
    jumlah_observasi = dplyr::n(),
    jumlah_provinsi = dplyr::n_distinct(provinsi),
    rata_kemiskinan = mean(kemiskinan, na.rm = TRUE),
    .groups = "drop"
  )
cluster_provinsi <- data_cluster_final %>%
  dplyr::group_by(cluster_label, provinsi) %>%
  dplyr::summarise(
    frekuensi = dplyr::n(),
    rata_kemiskinan = mean(kemiskinan, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(cluster_label, dplyr::desc(rata_kemiskinan))
cat("\n4.3.4 HASIL CLUSTERING ST-DBSCAN (TRAINING ONLY)\n")
cat("Jumlah cluster:", jumlah_cluster, "\n")
cat("Jumlah noise:", jumlah_noise, "\n")
cat("Persentase noise:", persen_noise, "%\n")
cat("Silhouette final (SC):", round(sil_score, 4), "\n")
cat("DBI final:", round(dbi_score_final, 4), "\n")
cat("DBCV final:", round(dbcv_score_final, 4), "\n")
print(validasi_final)
print(cluster_char)
print(cluster_provinsi)
openxlsx::write.xlsx(
  list(
    Hasil_Klaster_Training = data_cluster_final,
    Karakteristik_Klaster = cluster_char,
    Provinsi_per_Klaster = cluster_provinsi,
    Validasi_Indeks = validasi_final
  ),
  file.path(folder_hasil, "4.3.4_Hasil_Klasterisasi_STDBSCAN_TRAINING.xlsx"),
  overwrite = TRUE
)
# ---------------------------------------------------------------------
# 4.3.5 Penentuan cluster dominan tingkat provinsi
# Noise (-1) tidak diperlakukan sebagai cluster. Jika suatu provinsi tidak
# memiliki observasi pada cluster valid, cluster dominannya ditandai NA dan
# perlu ditangani sebelum pembentukan matriks CBWM.
# ---------------------------------------------------------------------
dominant_non_noise <- function(x) {
  x_valid <- x[x > 0]
  if (length(x_valid) == 0) return(NA_real_)
  tab <- table(x_valid)
  as.numeric(names(tab)[which.max(tab)])
}
frequency_non_noise <- function(x) {
  x_valid <- x[x > 0]
  if (length(x_valid) == 0) return(0L)
  as.integer(max(table(x_valid)))
}
cluster_dominan <- data_cluster_final %>%
  dplyr::group_by(provinsi) %>%
  dplyr::summarise(
    cluster_dominan = dominant_non_noise(cluster),
    frekuensi_cluster = frequency_non_noise(cluster),
    jumlah_periode = dplyr::n(),
    jumlah_noise = sum(cluster == -1),
    .groups = "drop"
  )
cat("\n4.3.5 CLUSTER DOMINAN TINGKAT PROVINSI (TRAINING ONLY)\n")
print(cluster_dominan)
if (any(is.na(cluster_dominan$cluster_dominan))) {
  warning(
    "Ada provinsi yang tidak memiliki observasi pada cluster valid. ",
    "Jangan membentuk CBWM sebelum kasus tersebut ditangani."
  )
}
openxlsx::write.xlsx(
  cluster_dominan,
  file.path(folder_hasil, "4.3.5_Klaster_Dominan_Provinsi_TRAINING.xlsx"),
  overwrite = TRUE
)
# ---------------------------------------------------------------------
# CATATAN UNTUK TAHAP CBWM-GSTAR BERIKUTNYA
# 1. Gunakan cluster_dominan dari 28 semester training untuk membentuk CBWM.
# 2. Hitung NCCF hanya dari data training.
# 3. Estimasi GSTAR hanya menggunakan data training.
# 4. df_test hanya digunakan setelah model selesai untuk evaluasi ramalan.
# 5. Jangan memakai data testing untuk memilih parameter, cluster, bobot,
#    orde model, atau transformasi.
# 6. eps_2_final di atas dinyatakan dalam satuan tahun karena variabel waktu
#    didefinisikan sebagai tahun + (semester - 1)/2. Pastikan satuan ini
#    konsisten dengan definisi epsilon temporal pada naskah artikel.
# =====================================================================
# =====================================================================
# PERSIAPAN INTERNAL DATA GSTAR
# Perhitungan berikut mendukung subbab 4.4.2.2 dan 4.4.2.3.
# =====================================================================
data_long <- df %>% dplyr::transmute(Provinsi,Tahun=tahun,Semester=semester,Periode,Waktu=waktu,Kemiskinan=kemiskinan)
lambda_bawah <- .75; lambda_atas <- 1.75; maks_transformasi <- 2; alpha_uji <- .05
buat_positif <- function(x){ shift <- ifelse(min(x,na.rm=TRUE)<=0,abs(min(x,na.rm=TRUE))+1e-6,0); list(data=x+shift,shift=shift) }
estimasi_lambda <- function(x){
  x <- as.numeric(x); x <- x[is.finite(x)]; if(length(x)<8||sd(x)==0) return(1)
  x <- buat_positif(x)$data; m <- lm(nilai~1,data=data.frame(nilai=x),x=TRUE,y=TRUE,model=TRUE); bc <- MASS::boxcox(m,lambda=seq(-3,3,.01),plotit=FALSE); bc$x[which.max(bc$y)]
}
pilih_lambda_transformasi <- function(lambda_est){ if(!is.finite(lambda_est)) return(1); if(lambda_est<=-.75) return(-1); if(lambda_est<=-.25) return(-.5); if(lambda_est<=.25) return(0); if(lambda_est<=.75) return(.5); if(lambda_est<=lambda_atas) return(1); max(2,min(3,round(lambda_est))) }
nama_transformasi <- function(lambda){ if(lambda==-1) return("1/X"); if(lambda==-.5) return("1/sqrt(X)"); if(lambda==0) return("ln(X)"); if(lambda==.5) return("sqrt(X)"); if(lambda==1) return("X (tanpa transformasi)"); if(lambda>1) return(paste0("X^",lambda)); paste0("BoxCox lambda=",round(lambda,4)) }
transformasi_manual <- function(x,lambda){ if(lambda==-1) return(1/x); if(lambda==-.5) return(1/sqrt(x)); if(lambda==0) return(log(x)); if(lambda==.5) return(sqrt(x)); if(lambda==1) return(x); if(lambda>1) return(x^lambda); forecast::BoxCox(x,lambda) }
proses_boxcox_maks_dua <- function(x,provinsi,variabel){
  x_asli <- as.numeric(x); posisi_valid <- which(is.finite(x_asli)); z <- x_asli[posisi_valid]
  if(length(z)<8||sd(z)==0) return(list(data=x_asli,riwayat=tibble::tibble(Provinsi=provinsi,Variabel=variabel,Tahap=0,Lambda_sebelum=NA,Transformasi="Data tidak cukup",Lambda_dipakai=NA,Lambda_setelah=NA,Lolos=NA),lambda_akhir=NA,shift=0))
  positif <- buat_positif(z); z <- positif$data; shift_awal <- positif$shift; lambda_awal <- estimasi_lambda(z); lambda_sekarang <- lambda_awal; z_hasil <- z; riwayat <- list(); status_lolos <- lambda_awal>=lambda_bawah&&lambda_awal<=lambda_atas
  if(status_lolos) riwayat[[1]] <- tibble::tibble(Provinsi=provinsi,Variabel=variabel,Tahap=0,Lambda_sebelum=lambda_awal,Transformasi="Tidak perlu transformasi",Lambda_dipakai=1,Lambda_setelah=lambda_awal,Lolos=TRUE)
  if(!status_lolos) for(i in seq_len(maks_transformasi)){
    lambda_pakai <- pilih_lambda_transformasi(lambda_sekarang); z_hasil <- transformasi_manual(z_hasil,lambda_pakai); z_hasil <- buat_positif(z_hasil)$data; lambda_baru <- estimasi_lambda(z_hasil)
    riwayat[[i]] <- tibble::tibble(Provinsi=provinsi,Variabel=variabel,Tahap=i,Lambda_sebelum=lambda_sekarang,Transformasi=nama_transformasi(lambda_pakai),Lambda_dipakai=lambda_pakai,Lambda_setelah=lambda_baru,Lolos=lambda_baru>=lambda_bawah&&lambda_baru<=lambda_atas)
    lambda_sekarang <- lambda_baru; if(lambda_baru>=lambda_bawah&&lambda_baru<=lambda_atas){ status_lolos <- TRUE; break }
  }
  if(!status_lolos) z_hasil <- z; hasil_akhir <- rep(NA_real_,length(x_asli)); hasil_akhir[posisi_valid] <- z_hasil
  list(data=hasil_akhir,riwayat=dplyr::bind_rows(riwayat),lambda_akhir=lambda_sekarang,shift=shift_awal)
}
proses_variabel <- function(df,nama_variabel){ split(df,df$Provinsi) %>% purrr::imap(function(d,prov){ d <- d %>% dplyr::arrange(Tahun,Semester); h <- proses_boxcox_maks_dua(d[[nama_variabel]],prov,nama_variabel); d[[paste0(nama_variabel,"_Ragam")]] <- h$data; list(data=d,riwayat=h$riwayat,lambda=h$lambda_akhir,shift=h$shift) }) }
hasil_kem <- proses_variabel(data_long %>% dplyr::select(Provinsi,Tahun,Semester,Periode,Waktu,Kemiskinan),"Kemiskinan")
data_kem_ragam <- dplyr::bind_rows(purrr::map(hasil_kem,"data")); riwayat_boxcox <- hasil_kem %>% purrr::map("riwayat") %>% dplyr::bind_rows()
ringkasan_boxcox <- riwayat_boxcox %>% dplyr::group_by(Provinsi,Variabel) %>% dplyr::summarise(
  Lambda_awal=dplyr::first(Lambda_sebelum),Transformasi_I=ifelse(any(Tahap==1),Transformasi[Tahap==1][1],"Tidak perlu transformasi"),
  Lambda_setelah_I=ifelse(any(Tahap==1),Lambda_setelah[Tahap==1][1],dplyr::first(Lambda_setelah)),Transformasi_II=ifelse(any(Tahap==2),Transformasi[Tahap==2][1],"-"),
  Lambda_setelah_II=ifelse(any(Tahap==2),Lambda_setelah[Tahap==2][1],NA_real_),Jumlah_transformasi=max(Tahap,na.rm=TRUE),Lambda_akhir=dplyr::last(Lambda_setelah),Status_stasioner_ragam=dplyr::last(Lolos),.groups="drop")
data_ragam <- data_kem_ragam %>% dplyr::arrange(Provinsi,Tahun,Semester)
uji_adf <- function(x,d){
  if(d>0) x <- diff(x,differences=d); x <- x[is.finite(x)]
  if(length(x)<8) return(data.frame(d=d,Statistik=NA_real_,p_value=NA_real_,Status="Data tidak cukup")); if(sd(x)==0) return(data.frame(d=d,Statistik=NA_real_,p_value=NA_real_,Status="Variansi nol"))
  h <- tryCatch(suppressWarnings(tseries::adf.test(x,alternative="stationary")),error=function(e) NULL)
  if(is.null(h)) return(data.frame(d=d,Statistik=NA_real_,p_value=NA_real_,Status="Gagal uji"))
  data.frame(d=d,Statistik=as.numeric(h$statistic),p_value=h$p.value,Status=ifelse(h$p.value<.05,"Stasioner","Tidak stasioner"))
}
hasil_adf <- data_ragam %>% dplyr::group_split(Provinsi) %>% purrr::map_dfr(function(z) purrr::map_dfr(0:2,function(d) uji_adf(z$Kemiskinan_Ragam,d) %>% dplyr::mutate(Provinsi=z$Provinsi[1],Jumlah_Data=length(z$Kemiskinan_Ragam)))) %>% dplyr::select(Provinsi,d,Statistik,p_value,Status,Jumlah_Data) %>% dplyr::arrange(Provinsi,d)
orde_d <- hasil_adf %>% dplyr::filter(Status=="Stasioner") %>% dplyr::group_by(Provinsi) %>% dplyr::summarise(d_terpilih=min(d),.groups="drop")
d_global <- max(orde_d$d_terpilih,na.rm=TRUE)
ringkasan_adf <- hasil_adf %>% dplyr::group_by(Provinsi) %>% dplyr::summarise(p_value_d0=p_value[d==0],p_value_d1=p_value[d==1],p_value_d2=p_value[d==2],d_terpilih=ifelse(Provinsi %in% orde_d$Provinsi,orde_d$d_terpilih[match(Provinsi,orde_d$Provinsi)],NA),.groups="drop")
data_stasioner <- data_ragam %>% dplyr::arrange(Provinsi,Tahun,Semester) %>% dplyr::group_by(Provinsi) %>% dplyr::mutate(Kemiskinan_dglobal=dplyr::case_when(
  d_global==0~Kemiskinan_Ragam,d_global==1~Kemiskinan_Ragam-dplyr::lag(Kemiskinan_Ragam,1),d_global==2~Kemiskinan_Ragam-2*dplyr::lag(Kemiskinan_Ragam,1)+dplyr::lag(Kemiskinan_Ragam,2))) %>% dplyr::ungroup() %>% dplyr::filter(!is.na(Kemiskinan_dglobal))
data_mts <- data_stasioner %>% dplyr::mutate(Waktu_Model=paste0(Tahun,"_S",Semester)) %>% dplyr::select(Waktu_Model,Provinsi,Kemiskinan_dglobal) %>% tidyr::pivot_wider(names_from=Provinsi,values_from=Kemiskinan_dglobal)
matriks_ts <- as.matrix(data_mts[,-1]); rownames(matriks_ts) <- data_mts$Waktu_Model
openxlsx::write.xlsx(list(Ringkasan_BoxCox=ringkasan_boxcox,Hasil_ADF=hasil_adf,Ringkasan_ADF=ringkasan_adf),file.path(folder_hasil,"4.4.2_Hasil_Pengujian_Asumsi.xlsx"),overwrite=TRUE)
# =====================================================================
# IV.4 PEMBENTUKAN MODEL GSTAR TINGKAT KEMISKINAN PROVINSI DI SULAWESI
# =====================================================================
# 4.4.1 Pembagian Data Training dan Testing
Y_mts <- as.matrix(matriks_ts); Z <- Y_mts; Tn <- nrow(Z); N <- ncol(Z); nama_lokasi <- colnames(Z)
n_train <- floor(.90*Tn); Z_train <- Z[1:n_train,,drop=FALSE]; Z_test <- Z[(n_train+1):Tn,,drop=FALSE]; periode_train <- rownames(Z_train); periode_test <- rownames(Z_test)
pembagian_data <- data.frame(Kelompok=c("Training","Testing"),Jumlah_Observasi=c(nrow(Z_train),nrow(Z_test)),Persentase=c(100*nrow(Z_train)/Tn,100*nrow(Z_test)/Tn),Periode_Awal=c(rownames(Z_train)[1],rownames(Z_test)[1]),Periode_Akhir=c(tail(rownames(Z_train),1),tail(rownames(Z_test),1)))
cat("\n4.4.1 PEMBAGIAN DATA TRAINING DAN TESTING\n"); print(pembagian_data)
# 4.4.2 Pengujian Asumsi Data
# 4.4.2.1 Analisis Korelasi Pearson
data_korelasi <- data_stasioner %>% dplyr::mutate(Periode_Model=paste0(Tahun,"_S",Semester)) %>% dplyr::select(Periode_Model,Provinsi,Kemiskinan_dglobal) %>% tidyr::pivot_wider(names_from=Provinsi,values_from=Kemiskinan_dglobal)
matriks_korelasi_pearson <- cor(data_korelasi[,-1],method="pearson",use="pairwise.complete.obs")
cat("\n4.4.2.1 ANALISIS KORELASI PEARSON\n"); print(round(matriks_korelasi_pearson,4))
# 4.4.2.2 Stasioneritas Ragam
cat("\n4.4.2.2 STASIONERITAS RAGAM\n"); print(ringkasan_boxcox)
# 4.4.2.3 Stasioneritas Rata-rata
cat("\n4.4.2.3 STASIONERITAS RATA-RATA\n"); print(ringkasan_adf); cat("d_global =",d_global,"\n")
koordinat <- data.frame(Provinsi=c("GORONTALO","SULAWESI BARAT","SULAWESI SELATAN","SULAWESI TENGAH","SULAWESI TENGGARA","SULAWESI UTARA"),Latitude=c(.6999,-2.8441,-3.6688,-1.4300,-4.1449,.6247),Longitude=c(122.4467,119.2321,119.9741,121.4456,122.1746,123.9750))
koordinat <- koordinat[match(nama_lokasi,koordinat$Provinsi),,drop=FALSE]; if(anyNA(koordinat$Provinsi)) stop("Nama provinsi pada koordinat tidak sesuai dengan matriks time series")
spatial_lag <- function(x,W) as.numeric(W%*%x)
# =====================================================================
# IV.5 PEMBENTUKAN MATRIKS BOBOT PADA MODEL GSTAR
# =====================================================================
# 4.5.1 Pembentukan Bobot Inverse Distance Weighting (IDW)
W_IDW <- matrix(0,N,N)
for(i in seq_len(N)) for(j in seq_len(N)) if(i!=j){ jarak <- sqrt((koordinat$Latitude[i]-koordinat$Latitude[j])^2+(koordinat$Longitude[i]-koordinat$Longitude[j])^2); W_IDW[i,j] <- 1/jarak }
W_IDW <- W_IDW/rowSums(W_IDW); dimnames(W_IDW) <- list(nama_lokasi,nama_lokasi); cat("\n4.5.1 BOBOT IDW\n"); print(round(W_IDW,6))
# 4.5.2 Pembentukan Bobot Normalized Cross-Correlation Function (NCCF)
W_NCCF <- matrix(0,N,N)
for(i in seq_len(N)) for(j in seq_len(N)) if(i!=j) W_NCCF[i,j] <- abs(cor(Z_train[,i],Z_train[,j]))
W_NCCF[is.na(W_NCCF)] <- 0; diag(W_NCCF) <- 0; rs <- rowSums(W_NCCF); W_NCCF[rs>0,] <- W_NCCF[rs>0,,drop=FALSE]/rs[rs>0]; W_NCCF[!is.finite(W_NCCF)] <- 0; dimnames(W_NCCF) <- list(nama_lokasi,nama_lokasi)
cat("\n4.5.2 BOBOT NCCF\n"); print(round(W_NCCF,6))
# 4.5.3 Penetapan klaster yang digunakan untuk membentuk CBWM
# Keanggotaan CBWM ditetapkan sesuai hasil klaster yang diberikan:
# Cluster 1: Sulawesi Tengah, Sulawesi Barat, Sulawesi Selatan, Sulawesi Tenggara
# Cluster 2: Gorontalo, Sulawesi Utara
# Catatan: label ini dipakai untuk pembentukan CBWM; hasil ST-DBSCAN otomatis
# tetap tersimpan terpisah pada objek cluster_dominan.
cluster_manual_cbwm <- c(
  "SULAWESI TENGAH" = 1L,
  "SULAWESI BARAT" = 1L,
  "SULAWESI SELATAN" = 1L,
  "SULAWESI TENGGARA" = 1L,
  "GORONTALO" = 1L,
  "SULAWESI UTARA" = 2L
)
cluster_sulawesi <- stats::setNames(as.integer(cluster_manual_cbwm[nama_lokasi]), nama_lokasi)
if(anyNA(cluster_sulawesi)) {
  stop("Penetapan klaster CBWM tidak lengkap. Periksa nama provinsi pada matriks GSTAR.")
}
cluster_cbwm_ditetapkan <- data.frame(
  Provinsi = names(cluster_sulawesi),
  Cluster = as.integer(cluster_sulawesi),
  Nama_Cluster = paste0("Cluster ", as.integer(cluster_sulawesi)),
  stringsAsFactors = FALSE
)
cat("\nKLASTER YANG DIGUNAKAN UNTUK CBWM\n")
print(cluster_cbwm_ditetapkan)
bangun_CBWM <- function(cluster_label, W) {
  cluster_label <- cluster_label[colnames(W)]
  if(anyNA(cluster_label)) stop("Label klaster CBWM tidak cocok dengan nama kolom matriks bobot.")
  n <- length(cluster_label)
  out <- matrix(0, n, n, dimnames = list(names(cluster_label), names(cluster_label)))
  for(i in seq_len(n)) {
    for(j in seq_len(n)) {
      if(i != j && cluster_label[i] == cluster_label[j]) out[i, j] <- W[i, j]
    }
  }
  rs <- rowSums(out)
  nz <- rs > 0
  out[nz, ] <- out[nz, , drop = FALSE] / rs[nz]
  out[!is.finite(out)] <- 0
  out
}
W_CBWM_IDW <- bangun_CBWM(cluster_sulawesi, W_IDW)
cat("\n4.5.3 BOBOT CBWM-IDW\n")
print(round(W_CBWM_IDW, 6))
# 4.5.4 Pembentukan Bobot CBWM-NCCF
W_CBWM_NCCF <- bangun_CBWM(cluster_sulawesi, W_NCCF)
cat("\n4.5.4 BOBOT CBWM-NCCF\n")
print(round(W_CBWM_NCCF, 6))
daftar_W <- list(
  IDW = W_IDW,
  NCCF = W_NCCF,
  CBWM_IDW = W_CBWM_IDW,
  CBWM_NCCF = W_CBWM_NCCF
)
# =====================================================================
# IV.6 IDENTIFIKASI ORDE MODEL GSTAR
# =====================================================================
Y <- Y_mts; n <- nrow(Y); k <- ncol(Y); nama_prov <- colnames(Y)
# 4.6.1 Matrix Autocorrelation Function (MACF)
hitung_MACF <- function(data,maxlag=5){ data <- as.matrix(data); n <- nrow(data); k <- ncol(data); nm <- colnames(data); hasil <- list(); for(h in 0:maxlag){ if(h==0) Rlag <- diag(k) else Rlag <- cor(data[(h+1):n,,drop=FALSE],data[1:(n-h),,drop=FALSE],use="pairwise.complete.obs"); rownames(Rlag) <- nm; colnames(Rlag) <- nm; hasil[[paste0("Lag_",h)]] <- Rlag }; hasil }
MACF <- hitung_MACF(Y,5); cat("\n4.6.1 HASIL MACF\n"); print(MACF)
# 4.6.2 Matrix Partial Autocorrelation Function (MPACF)
residualisasi_matrix <- function(target,kontrol=NULL){ target <- as.matrix(target); if(is.null(kontrol)||ncol(as.matrix(kontrol))==0) return(as.matrix(scale(target,center=TRUE,scale=FALSE))); X <- cbind(Intercept=1,as.matrix(kontrol)); hasil <- matrix(NA_real_,nrow(target),ncol(target)); for(j in seq_len(ncol(target))) hasil[,j] <- lm.fit(x=X,y=target[,j])$residuals; colnames(hasil) <- colnames(target); hasil }
hitung_MPACF <- function(data,maxlag=5){
  data <- as.matrix(data); n <- nrow(data); k <- ncol(data); nm <- colnames(data); hm <- list(); hp <- list(); info <- data.frame(); MP0 <- diag(k); rownames(MP0) <- nm; colnames(MP0) <- nm; P0 <- matrix(NA_real_,k,k,dimnames=list(nm,nm)); hm[["Lag_0"]] <- MP0; hp[["Lag_0"]] <- P0
  for(h in 1:maxlag){
    Y_now <- data[(h+1):n,,drop=FALSE]; Y_lag <- data[1:(n-h),,drop=FALSE]; kontrol <- NULL
    if(h>1) for(r in 1:(h-1)){ blok <- data[(h+1-r):(n-r),,drop=FALSE]; colnames(blok) <- paste0(nm,"_lag",r); kontrol <- cbind(kontrol,blok) }
    R_now <- residualisasi_matrix(Y_now,kontrol); R_lag <- residualisasi_matrix(Y_lag,kontrol); MP <- cor(R_now,R_lag,use="pairwise.complete.obs"); rownames(MP) <- nm; colnames(MP) <- nm
    n_eff <- nrow(Y_now); if(is.null(kontrol)){ rank_kontrol <- 1; jumlah_kontrol <- 0 } else { Xk <- cbind(Intercept=1,kontrol); rank_kontrol <- qr(Xk)$rank; jumlah_kontrol <- ncol(kontrol) }
    df_partial <- n_eff-rank_kontrol-2; Pval <- matrix(NA_real_,k,k,dimnames=list(nm,nm))
    if(df_partial>0) for(i in seq_len(k)) for(j in seq_len(k)){ r_ij <- MP[i,j]; if(is.finite(r_ij)&&abs(r_ij)<1){ t_stat <- r_ij*sqrt(df_partial/(1-r_ij^2)); Pval[i,j] <- 2*pt(-abs(t_stat),df=df_partial) } }
    hm[[paste0("Lag_",h)]] <- MP; hp[[paste0("Lag_",h)]] <- Pval; info <- rbind(info,data.frame(Lag=h,N_Efektif=n_eff,Jumlah_Kontrol=jumlah_kontrol,Rank_Kontrol=rank_kontrol,DF=df_partial))
  }
  list(MPACF=hm,PValue=hp,Informasi=info)
}
hasil_MPACF <- hitung_MPACF(Y,5); MPACF <- hasil_MPACF$MPACF; PValue_MPACF <- hasil_MPACF$PValue; Info_MPACF <- hasil_MPACF$Informasi
cat("\n4.6.2 INFORMASI MPACF\n"); print(Info_MPACF); for(h in 0:5){ cat("\nMPACF LAG",h,"\n"); print(MPACF[[paste0("Lag_",h)]]) }
data_MACF <- data.frame(); data_MPACF <- data.frame()
for(h in 0:5){
  temp <- reshape2::melt(MACF[[paste0("Lag_",h)]])
  colnames(temp) <- c("Provinsi_t","Provinsi_lag","Nilai")
  temp$Lag <- paste0("Lag ",h); data_MACF <- rbind(data_MACF,temp)
  nilai <- reshape2::melt(MPACF[[paste0("Lag_",h)]])
  pval <- reshape2::melt(PValue_MPACF[[paste0("Lag_",h)]])
  colnames(nilai) <- c("Provinsi_t","Provinsi_lag","Nilai")
  nilai$p_value <- pval$value; nilai$Lag <- paste0("Lag ",h)
  data_MPACF <- rbind(data_MPACF,nilai)
}
data_MACF$Lag <- factor(data_MACF$Lag,levels=paste0("Lag ",0:5))
data_MPACF$Lag <- factor(data_MPACF$Lag,levels=paste0("Lag ",0:5))
data_MACF <- data_MACF %>% dplyr::mutate(
  Lag_Angka=as.integer(gsub("Lag ","",as.character(Lag))),
  N_Efektif=n-Lag_Angka,
  Batas_Signifikan=1.96/sqrt(N_Efektif),
  Signifikan_MACF=ifelse(Lag_Angka==0,NA,abs(Nilai)>Batas_Signifikan),
  Tanda_MACF=dplyr::case_when(
    Lag_Angka==0 ~ "—",
    Signifikan_MACF & Nilai>0 ~ "+",
    Signifikan_MACF & Nilai<0 ~ "-",
    TRUE ~ "...."
  )
)
data_MPACF <- data_MPACF %>% dplyr::mutate(
  Lag_Angka=as.integer(gsub("Lag ","",as.character(Lag))),
  Signifikan_MPACF=!is.na(p_value) & p_value<.05,
  Tanda_MPACF=dplyr::case_when(
    Lag_Angka==0 ~ "—",
    Signifikan_MPACF & Nilai>0 ~ "+",
    Signifikan_MPACF & Nilai<0 ~ "-",
    TRUE ~ "...."
  )
)
ringkasan_MACF <- data_MACF %>% dplyr::filter(Lag_Angka>0) %>% dplyr::group_by(Lag) %>% dplyr::summarise(
  N_Efektif=dplyr::first(N_Efektif),Batas_Signifikan=dplyr::first(Batas_Signifikan),
  Jumlah_Hubungan=dplyr::n(),Signifikan=sum(Signifikan_MACF,na.rm=TRUE),
  Positif_Signifikan=sum(Signifikan_MACF&Nilai>0,na.rm=TRUE),
  Negatif_Signifikan=sum(Signifikan_MACF&Nilai<0,na.rm=TRUE),
  Persentase_Signifikan=100*Signifikan/Jumlah_Hubungan,.groups="drop"
)
ringkasan_MPACF <- data_MPACF %>% dplyr::filter(Lag_Angka>0) %>% dplyr::group_by(Lag) %>% dplyr::summarise(
  Jumlah_Hubungan=sum(!is.na(p_value)),Signifikan=sum(p_value<.05,na.rm=TRUE),
  Positif_Signifikan=sum(p_value<.05&Nilai>0,na.rm=TRUE),
  Negatif_Signifikan=sum(p_value<.05&Nilai<0,na.rm=TRUE),.groups="drop"
)
# Plot signifikansi setiap lag: + positif signifikan, - negatif signifikan, .... tidak signifikan.
data_MACF_plot <- data_MACF %>% dplyr::filter(Lag_Angka>0) %>% dplyr::mutate(
  Lag=factor(as.character(Lag),levels=paste0("Lag ",1:5)),
  Provinsi_t=factor(as.character(Provinsi_t),levels=rev(nama_lokasi)),
  Provinsi_lag=factor(as.character(Provinsi_lag),levels=nama_lokasi)
)
p_macf <- ggplot2::ggplot(data_MACF_plot,ggplot2::aes(x=Provinsi_lag,y=Provinsi_t)) +
  ggplot2::geom_tile(fill="white",color="grey80",linewidth=.35) +
  ggplot2::geom_text(ggplot2::aes(label=Tanda_MACF),size=3.6,fontface="bold") +
  ggplot2::facet_wrap(~Lag,nrow=1) +
  ggplot2::labs(title="Signifikansi MACF pada Setiap Lag",
    subtitle="+ = positif signifikan; - = negatif signifikan; .... = tidak signifikan",
    x="Provinsi pada lag",y="Provinsi saat ini") +
  ggplot2::theme_minimal(base_size=9) +
  ggplot2::theme(axis.text.x=ggplot2::element_text(angle=45,hjust=1),
    panel.grid=ggplot2::element_blank(),strip.text=ggplot2::element_text(face="bold"))
print(p_macf)
ggplot2::ggsave(file.path(folder_visual,"4.6.1_MACF_Signifikansi_per_Lag.png"),
  p_macf,width=14,height=5.5,dpi=300,bg="white")
data_MPACF_plot <- data_MPACF %>% dplyr::filter(Lag_Angka>0) %>% dplyr::mutate(
  Lag=factor(as.character(Lag),levels=paste0("Lag ",1:5)),
  Provinsi_t=factor(as.character(Provinsi_t),levels=rev(nama_lokasi)),
  Provinsi_lag=factor(as.character(Provinsi_lag),levels=nama_lokasi)
)
p_mpacf <- ggplot2::ggplot(data_MPACF_plot,ggplot2::aes(x=Provinsi_lag,y=Provinsi_t)) +
  ggplot2::geom_tile(fill="white",color="grey80",linewidth=.35) +
  ggplot2::geom_text(ggplot2::aes(label=Tanda_MPACF),size=3.6,fontface="bold") +
  ggplot2::facet_wrap(~Lag,nrow=1) +
  ggplot2::labs(title="Signifikansi MPACF pada Setiap Lag",
    subtitle="+ = positif signifikan; - = negatif signifikan; .... = tidak signifikan",
    x="Provinsi pada lag",y="Provinsi saat ini") +
  ggplot2::theme_minimal(base_size=9) +
  ggplot2::theme(axis.text.x=ggplot2::element_text(angle=45,hjust=1),
    panel.grid=ggplot2::element_blank(),strip.text=ggplot2::element_text(face="bold"))
print(p_mpacf)
ggplot2::ggsave(file.path(folder_visual,"4.6.2_MPACF_Signifikansi_per_Lag.png"),
  p_mpacf,width=14,height=5.5,dpi=300,bg="white")
cat("\nRINGKASAN SIGNIFIKANSI MACF\n"); print(ringkasan_MACF)
cat("\nRINGKASAN SIGNIFIKANSI MPACF\n"); print(ringkasan_MPACF)
wb_macf <- list()
for(h in 0:5) wb_macf[[paste0("MACF_Lag",h)]] <- MACF[[paste0("Lag_",h)]]
for(h in 0:5) wb_macf[[paste0("MPACF_Lag",h)]] <- MPACF[[paste0("Lag_",h)]]
for(h in 0:5) wb_macf[[paste0("PValue_MPACF_Lag",h)]] <- PValue_MPACF[[paste0("Lag_",h)]]
wb_macf[["Ringkasan_MACF"]] <- ringkasan_MACF
wb_macf[["Info_MPACF"]] <- Info_MPACF
wb_macf[["Ringkasan_MPACF"]] <- ringkasan_MPACF
wb_macf[["Tanda_MACF_per_Lag"]] <- data_MACF
wb_macf[["Tanda_MPACF_per_Lag"]] <- data_MPACF
openxlsx::write.xlsx(wb_macf,file.path(folder_hasil,"4.6.1-4.6.2_MACF_MPACF.xlsx"),overwrite=TRUE)
# 4.6.3 Pemilihan Orde Model Terbaik Berdasarkan AIC
Zt <- Y_mts; Tn <- nrow(Zt); N <- ncol(Zt); W_matriks <- list(IDW=as.matrix(W_IDW),NCCF=as.matrix(W_NCCF))
for(nm in names(W_matriks)) if(!all(dim(W_matriks[[nm]])==c(N,N))) stop(paste("Dimensi W",nm,"tidak sesuai"))
estimasi_GSTAR_OLS <- function(Zt,W,p){
  Tn <- nrow(Zt); N <- ncol(Zt); if(p==0) return(list(beta=NULL,residual=Zt)); X <- NULL; Y0 <- NULL
  for(t in (p+1):Tn){ baris <- NULL; for(k0 in 1:p) baris <- c(baris,Zt[t-k0,],spatial_lag(Zt[t-k0,],W)); X <- rbind(X,baris); Y0 <- rbind(Y0,Zt[t,]) }
  beta <- MASS::ginv(t(X)%*%X)%*%t(X)%*%Y0; list(beta=beta,residual=Y0-X%*%beta)
}
estimasi_MA_residual <- function(residual,q){ if(q==0) return(list(theta=NULL,residual=residual)); X <- NULL; Y0 <- NULL; for(t in (q+1):nrow(residual)){ X <- rbind(X,residual[t-q,]); Y0 <- rbind(Y0,residual[t,]) }; theta <- MASS::ginv(t(X)%*%X)%*%t(X)%*%Y0; list(theta=theta,residual=Y0-X%*%theta) }
hitung_AIC_GSTARMA <- function(residual,jumlah_parameter){ Sigma <- cov(residual); det_sigma <- det(Sigma); if(is.na(det_sigma)||!is.finite(det_sigma)||det_sigma<=0) return(Inf); n0 <- nrow(residual); m <- ncol(residual); n0*(m*log(2*pi)+log(det_sigma))+2*jumlah_parameter }
hasil_gstarma <- data.frame(Bobot=character(),AR=integer(),MA=integer(),AIC=numeric()); model_simpan <- list()
for(nama_bobot in names(W_matriks)) for(p in 0:2) for(q in 0:2){ W <- W_matriks[[nama_bobot]]; hasil_AR <- estimasi_GSTAR_OLS(Zt,W,p); hasil_MA <- estimasi_MA_residual(hasil_AR$residual,q); residual_final <- hasil_MA$residual; jumlah_parameter <- 2*N*p+N*q+N; AIC_model <- hitung_AIC_GSTARMA(residual_final,jumlah_parameter); hasil_gstarma <- rbind(hasil_gstarma,data.frame(Bobot=nama_bobot,AR=p,MA=q,AIC=AIC_model)); nm <- paste0("GSTARMA_",nama_bobot,"_",p,"_",q); model_simpan[[nm]] <- list(Bobot=nama_bobot,W=W,AR=hasil_AR,MA=hasil_MA,residual=residual_final,AIC=AIC_model) }
hasil_gstarma <- dplyr::arrange(hasil_gstarma,AIC); model_terbaik_per_bobot <- hasil_gstarma %>% dplyr::group_by(Bobot) %>% dplyr::slice_min(AIC,n=1,with_ties=FALSE) %>% dplyr::ungroup(); model_terbaik_global <- hasil_gstarma[1,,drop=FALSE]
cat("\n4.6.3 HASIL PEMILIHAN ORDE BERDASARKAN AIC\n"); print(hasil_gstarma); cat("\nMODEL TERBAIK SETIAP BOBOT\n"); print(model_terbaik_per_bobot); cat("\nMODEL TERBAIK GLOBAL\n"); print(model_terbaik_global)
openxlsx::write.xlsx(list(Seluruh_Kandidat=hasil_gstarma,Model_Terbaik_Per_Bobot=model_terbaik_per_bobot,Model_Terbaik_Global=model_terbaik_global),file.path(folder_hasil,"4.6.3_Hasil_Pemilihan_Orde_AIC.xlsx"),overwrite=TRUE)
# =====================================================================
# IV.7 ESTIMASI PARAMETER MODEL GSTAR (2,0)
# =====================================================================
# 4.7.1 Model GSTAR (2,0) dengan Bobot IDW
# 4.7.2 Model GSTAR (2,0) dengan Bobot NCCF
# 4.7.3 Model GSTAR (2,0) dengan Bobot CBWM-IDW
# 4.7.4 Model GSTAR (2,0) dengan Bobot CBWM-NCCF
estimasi_GSTAR_OLS_final <- function(Z_train,W){
  Z_train <- as.matrix(Z_train); W <- as.matrix(W); Tn <- nrow(Z_train); N <- ncol(Z_train); model <- vector("list",N)
  beta <- matrix(NA_real_,N,4,dimnames=list(colnames(Z_train),c("phi1","lambda1","phi2","lambda2"))); residual <- matrix(NA_real_,Tn-2,N,dimnames=list(rownames(Z_train)[3:Tn],colnames(Z_train))); fitted_value <- residual
  for(i in seq_len(N)){
    X <- matrix(NA_real_,Tn-2,4,dimnames=list(NULL,c("Z_lag1","WZ_lag1","Z_lag2","WZ_lag2"))); Y0 <- numeric(Tn-2); rr <- 1
    for(t in 3:Tn){ X[rr,] <- c(Z_train[t-1,i],spatial_lag(Z_train[t-1,],W)[i],Z_train[t-2,i],spatial_lag(Z_train[t-2,],W)[i]); Y0[rr] <- Z_train[t,i]; rr <- rr+1 }
    X <- as.data.frame(X)
    if(all(abs(X$WZ_lag1)<1e-12)&&all(abs(X$WZ_lag2)<1e-12)){ fit <- lm(Y0~0+Z_lag1+Z_lag2,data=X); cf <- coef(fit); beta[i,] <- c(cf["Z_lag1"],0,cf["Z_lag2"],0) }
    else { fit <- lm(Y0~0+Z_lag1+WZ_lag1+Z_lag2+WZ_lag2,data=X); cf <- coef(fit); beta[i,] <- c(cf["Z_lag1"],cf["WZ_lag1"],cf["Z_lag2"],cf["WZ_lag2"]) }
    model[[i]] <- fit; residual[,i] <- residuals(fit); fitted_value[,i] <- fitted(fit)
  }
  names(model) <- colnames(Z_train); list(model=model,beta=beta,residual=residual,fitted=fitted_value)
}
model_bobot <- list(); hasil_residual <- list(); hasil_fitted_diff <- list(); judul_model <- c(IDW="4.7.1 Model GSTAR (2,0) dengan Bobot IDW",NCCF="4.7.2 Model GSTAR (2,0) dengan Bobot NCCF",CBWM_IDW="4.7.3 Model GSTAR (2,0) dengan Bobot CBWM-IDW",CBWM_NCCF="4.7.4 Model GSTAR (2,0) dengan Bobot CBWM-NCCF")
for(nama_W in names(daftar_W)){ model_bobot[[nama_W]] <- estimasi_GSTAR_OLS_final(Z_train,daftar_W[[nama_W]]); hasil_residual[[nama_W]] <- model_bobot[[nama_W]]$residual; hasil_fitted_diff[[nama_W]] <- model_bobot[[nama_W]]$fitted; cat("\n",judul_model[[nama_W]],"\n",sep=""); print(model_bobot[[nama_W]]$beta) }
parameter_lengkap <- data.frame(); map_coef <- c(phi1="Z_lag1",lambda1="WZ_lag1",phi2="Z_lag2",lambda2="WZ_lag2")
for(nama_W in names(model_bobot)) for(i in seq_along(nama_lokasi)){ fit <- model_bobot[[nama_W]]$model[[i]]; sm <- summary(fit)$coefficients; for(par in names(map_coef)){ nm <- map_coef[[par]]; est <- model_bobot[[nama_W]]$beta[i,par]; if(nm %in% rownames(sm)){ se <- sm[nm,"Std. Error"]; ts <- sm[nm,"t value"]; pv <- sm[nm,"Pr(>|t|)"] } else { se <- NA_real_; ts <- NA_real_; pv <- NA_real_ }; parameter_lengkap <- rbind(parameter_lengkap,data.frame(Bobot=nama_W,Provinsi=nama_lokasi[i],Parameter=par,Estimate=as.numeric(est),Std_Error=as.numeric(se),t_value=as.numeric(ts),p_value=as.numeric(pv),Signifikan=ifelse(is.na(pv),"Struktural",ifelse(pv<.05,"Ya","Tidak")))) } }
label_model <- c(IDW="IDW",NCCF="NCCF",CBWM_IDW="CBWM-IDW",CBWM_NCCF="CBWM-NCCF")
cat("\nTABEL PARAMETER LENGKAP\n"); print(parameter_lengkap)
openxlsx::write.xlsx(parameter_lengkap,file.path(folder_hasil,"4.7_Estimasi_Parameter_GSTAR.xlsx"),overwrite=TRUE)
# =====================================================================
# IV.8 PENGUJIAN DIAGNOSTIK MODEL GSTAR
# =====================================================================
alpha_diag <- .05; lag_ljung <- 5; lag_port <- 5
# 4.8.1 Uji White Noise Residual
hasil_diagnostik_univariat <- data.frame()
for(nama_W in names(model_bobot)){ E <- as.matrix(model_bobot[[nama_W]]$residual); for(i in seq_len(ncol(E))){ e <- E[,i]; e <- e[is.finite(e)]; lb <- Box.test(e,lag=lag_ljung,type="Ljung-Box"); jb <- tseries::jarque.bera.test(e); hasil_diagnostik_univariat <- rbind(hasil_diagnostik_univariat,data.frame(Bobot=nama_W,Provinsi=colnames(E)[i],LB_Statistic=as.numeric(lb$statistic),LB_df=as.numeric(lb$parameter),LB_p_value=lb$p.value,Keputusan_LB=ifelse(lb$p.value>alpha_diag,"White Noise","Tidak White Noise"),JB_Statistic=as.numeric(jb$statistic),JB_p_value=jb$p.value,Keputusan_JB=ifelse(jb$p.value>alpha_diag,"Normal","Tidak Normal"))) } }
cat("\n4.8.1 UJI WHITE NOISE RESIDUAL - LJUNG-BOX\n"); print(hasil_diagnostik_univariat %>% dplyr::select(Bobot,Provinsi,LB_Statistic,LB_df,LB_p_value,Keputusan_LB))
portmanteau_multivariat <- function(E,lag=5){ E <- as.matrix(E); E <- E[apply(E,1,function(x) all(is.finite(x))),,drop=FALSE]; n0 <- nrow(E); m <- ncol(E); E <- scale(E,center=TRUE,scale=FALSE); C0 <- crossprod(E)/n0; C0_inv <- tryCatch(solve(C0),error=function(e) MASS::ginv(C0)); Q <- 0; for(h in 1:lag){ Ch <- crossprod(E[(h+1):n0,,drop=FALSE],E[1:(n0-h),,drop=FALSE])/n0; Q <- Q+(n0^2/(n0-h))*sum(diag(t(Ch)%*%C0_inv%*%Ch%*%C0_inv)) }; df <- m^2*lag; p <- 1-pchisq(Q,df=df); data.frame(Statistic=Q,df=df,p_value=p,Keputusan=ifelse(p>alpha_diag,"White Noise Multivariat","Tidak White Noise Multivariat")) }
hasil_portmanteau <- dplyr::bind_rows(lapply(names(model_bobot),function(nm){ z <- portmanteau_multivariat(model_bobot[[nm]]$residual,lag_port); data.frame(Bobot=nm,Lag=lag_port,z,check.names=FALSE) }))
cat("\nPORTMANTEAU MULTIVARIAT\n"); print(hasil_portmanteau)
# 4.8.2 Uji Normalitas Residual
cat("\n4.8.2 UJI NORMALITAS RESIDUAL - JARQUE-BERA\n"); print(hasil_diagnostik_univariat %>% dplyr::select(Bobot,Provinsi,JB_Statistic,JB_p_value,Keputusan_JB))
mardia_test <- function(E){ E <- as.matrix(E); E <- E[apply(E,1,function(x) all(is.finite(x))),,drop=FALSE]; n0 <- nrow(E); p0 <- ncol(E); X <- sweep(E,2,colMeans(E),"-"); S_inv <- tryCatch(solve(cov(E)),error=function(e) MASS::ginv(cov(E))); A <- X%*%S_inv%*%t(X); b1p <- sum(A^3)/(n0^2); skew_stat <- n0*b1p/6; skew_df <- p0*(p0+1)*(p0+2)/6; skew_p <- 1-pchisq(skew_stat,df=skew_df); mahal_sq <- rowSums((X%*%S_inv)*X); b2p <- mean(mahal_sq^2); kurt_z <- (b2p-p0*(p0+2))/sqrt(8*p0*(p0+2)/n0); kurt_p <- 2*(1-pnorm(abs(kurt_z))); data.frame(Mardia_Skewness=b1p,Skew_Statistic=skew_stat,Skew_df=skew_df,Skew_p_value=skew_p,Mardia_Kurtosis=b2p,Kurtosis_Z=kurt_z,Kurtosis_p_value=kurt_p,Keputusan=ifelse(skew_p>alpha_diag&&kurt_p>alpha_diag,"Normal Multivariat","Tidak Normal Multivariat")) }
hasil_mardia <- dplyr::bind_rows(lapply(names(model_bobot),function(nm) data.frame(Bobot=nm,mardia_test(model_bobot[[nm]]$residual),check.names=FALSE)))
cat("\nMARDIA MULTIVARIAT\n"); print(hasil_mardia)
# =====================================================================
# IV.9 HASIL PERAMALAN TINGKAT KEMISKINAN PROVINSI DI SULAWESI
# =====================================================================
# 4.9.1 Hasil Forecast Model GSTAR dengan Berbagai Matriks Bobot
forecast_GSTAR_OLS_final <- function(model,Z_train,h,W,periode_forecast=NULL){ Z_hat <- rbind(as.matrix(Z_train),matrix(NA_real_,h,ncol(Z_train))); colnames(Z_hat) <- colnames(Z_train); mulai <- nrow(Z_train)+1; for(t in mulai:nrow(Z_hat)){ WZ1 <- spatial_lag(Z_hat[t-1,],W); WZ2 <- spatial_lag(Z_hat[t-2,],W); for(i in seq_len(ncol(Z_train))){ b <- model$beta[i,]; Z_hat[t,i] <- b["phi1"]*Z_hat[t-1,i]+b["lambda1"]*WZ1[i]+b["phi2"]*Z_hat[t-2,i]+b["lambda2"]*WZ2[i] } }; out <- Z_hat[mulai:nrow(Z_hat),,drop=FALSE]; colnames(out) <- colnames(Z_train); if(!is.null(periode_forecast)) rownames(out) <- periode_forecast; out }
data_level_wide <- data_ragam %>% dplyr::mutate(Periode_Model=paste0(Tahun,"_S",Semester)) %>% dplyr::select(Periode_Model,Provinsi,Kemiskinan_Ragam) %>% tidyr::pivot_wider(names_from=Provinsi,values_from=Kemiskinan_Ragam)
level_matrix <- as.matrix(data_level_wide[,nama_lokasi]); rownames(level_matrix) <- data_level_wide$Periode_Model; colnames(level_matrix) <- nama_lokasi; periode_fitted_train <- periode_train[3:length(periode_train)]
inverse_fitted_training <- function(fitted_diff,periode_fit,level_matrix,d_global){ fitted_diff <- as.matrix(fitted_diff); hasil <- matrix(NA_real_,nrow(fitted_diff),ncol(fitted_diff),dimnames=dimnames(fitted_diff)); idx <- match(periode_fit,rownames(level_matrix)); if(any(is.na(idx))) stop("Periode fitted training tidak ditemukan"); for(r in seq_along(idx)) for(i in seq_len(ncol(fitted_diff))){ if(d_global==0) hasil[r,i] <- fitted_diff[r,i]; if(d_global==1) hasil[r,i] <- fitted_diff[r,i]+level_matrix[idx[r]-1,i]; if(d_global==2) hasil[r,i] <- fitted_diff[r,i]+2*level_matrix[idx[r]-1,i]-level_matrix[idx[r]-2,i] }; hasil }
inverse_forecast_testing_C <- function(forecast_diff,level_matrix,d_global){ forecast_diff <- as.matrix(forecast_diff); hasil <- matrix(NA_real_,nrow(forecast_diff),ncol(forecast_diff),dimnames=dimnames(forecast_diff)); for(i in seq_len(ncol(forecast_diff))){ if(d_global==0) hasil[,i] <- forecast_diff[,i]; if(d_global==1){ x1 <- tail(level_matrix[,i],1); for(h in seq_len(nrow(forecast_diff))){ x1 <- x1+forecast_diff[h,i]; hasil[h,i] <- x1 } }; if(d_global==2){ x1 <- tail(level_matrix[,i],1); x2 <- tail(level_matrix[,i],2)[1]; for(h in seq_len(nrow(forecast_diff))){ baru <- forecast_diff[h,i]+2*x1-x2; hasil[h,i] <- baru; x2 <- x1; x1 <- baru } } }; hasil }
inverse_transformasi_manual <- function(x,ringkasan_boxcox){ hasil <- as.matrix(x); info <- ringkasan_boxcox %>% dplyr::filter(Variabel=="Kemiskinan") %>% dplyr::arrange(match(Provinsi,colnames(hasil))); inverse_satu <- function(z,tr){ if(is.na(tr)||tr=="-"||tr=="Tidak perlu transformasi"||tr=="X (tanpa transformasi)") return(z); if(tr=="1/X") return(1/z); if(tr=="1/sqrt(X)") return(1/(z^2)); if(tr=="ln(X)") return(exp(z)); if(tr=="sqrt(X)") return(z^2); if(grepl("^X\\^[0-9.]+$",tr)) return(z^(1/as.numeric(sub("X\\^","",tr)))); stop(paste("Inverse transformasi belum tersedia:",tr)) }; for(i in seq_len(ncol(hasil))){ if("Transformasi_II" %in% names(info)) hasil[,i] <- inverse_satu(hasil[,i],info$Transformasi_II[i]); if("Transformasi_I" %in% names(info)) hasil[,i] <- inverse_satu(hasil[,i],info$Transformasi_I[i]) }; dimnames(hasil) <- dimnames(x); hasil }
data_aktual_wide <- data_long %>% dplyr::mutate(Periode_Model=paste0(Tahun,"_S",Semester)) %>% dplyr::select(Periode_Model,Provinsi,Kemiskinan) %>% tidyr::pivot_wider(names_from=Provinsi,values_from=Kemiskinan)
aktual_matrix <- as.matrix(data_aktual_wide[,nama_lokasi]); rownames(aktual_matrix) <- data_aktual_wide$Periode_Model; colnames(aktual_matrix) <- nama_lokasi; aktual_train <- aktual_matrix[periode_fitted_train,,drop=FALSE]; aktual_test <- aktual_matrix[periode_test,,drop=FALSE]
hasil_fitted_train <- list(); hasil_forecast_test <- list(); hasil_forecast_diff <- list()
for(nama_W in names(daftar_W)){ fitted_level <- inverse_fitted_training(model_bobot[[nama_W]]$fitted,periode_fitted_train,level_matrix,d_global); hasil_fitted_train[[nama_W]] <- inverse_transformasi_manual(fitted_level,ringkasan_boxcox); forecast_diff <- forecast_GSTAR_OLS_final(model_bobot[[nama_W]],Z_train,nrow(Z_test),daftar_W[[nama_W]],periode_test); hasil_forecast_diff[[nama_W]] <- forecast_diff; forecast_level <- inverse_forecast_testing_C(forecast_diff,level_matrix,d_global); hasil_forecast_test[[nama_W]] <- inverse_transformasi_manual(forecast_level,ringkasan_boxcox) }
matriks_ke_long <- function(M,nama_nilai="Nilai") as.data.frame(M,check.names=FALSE) %>% dplyr::mutate(Periode=rownames(M)) %>% tidyr::pivot_longer(-Periode,names_to="Provinsi",values_to=nama_nilai)
cat("\n4.9.1 HASIL FORECAST SELURUH MATRIKS BOBOT\n")
for(nm in names(hasil_forecast_test)){ cat("\n",label_model[[nm]],"\n",sep=""); print(data.frame(Periode=rownames(hasil_forecast_test[[nm]]),hasil_forecast_test[[nm]],check.names=FALSE),row.names=FALSE) }
# =====================================================================
# IV.10 EVALUASI DAN PERBANDINGAN KINERJA MODEL
# =====================================================================
# 4.10.1 Evaluasi Akurasi Model Berdasarkan RMSE, MAE, dan MAPE
hitung_akurasi <- function(prediksi,aktual){ prediksi <- as.matrix(prediksi); aktual <- as.matrix(aktual); valid <- is.finite(prediksi)&is.finite(aktual); e <- prediksi[valid]-aktual[valid]; a <- aktual[valid]; data.frame(RMSE=sqrt(mean(e^2)),MAE=mean(abs(e)),MAPE=mean(abs(e[a!=0]/a[a!=0]))*100) }
evaluasi_training <- data.frame(); evaluasi_testing <- data.frame(); evaluasi_per_provinsi <- data.frame()
for(nama_W in names(daftar_W)){ at <- hitung_akurasi(hasil_fitted_train[[nama_W]],aktual_train); ae <- hitung_akurasi(hasil_forecast_test[[nama_W]],aktual_test); evaluasi_training <- rbind(evaluasi_training,data.frame(Bobot=nama_W,at)); evaluasi_testing <- rbind(evaluasi_testing,data.frame(Bobot=nama_W,ae)); for(i in seq_along(nama_lokasi)){ x1 <- hitung_akurasi(hasil_fitted_train[[nama_W]][,i,drop=FALSE],aktual_train[,i,drop=FALSE]); x2 <- hitung_akurasi(hasil_forecast_test[[nama_W]][,i,drop=FALSE],aktual_test[,i,drop=FALSE]); evaluasi_per_provinsi <- rbind(evaluasi_per_provinsi,data.frame(Bobot=nama_W,Provinsi=nama_lokasi[i],Sample="Training",x1),data.frame(Bobot=nama_W,Provinsi=nama_lokasi[i],Sample="Testing",x2)) } }
evaluasi_training <- evaluasi_training %>% dplyr::arrange(RMSE); evaluasi_testing <- evaluasi_testing %>% dplyr::arrange(RMSE); bobot_terbaik <- evaluasi_testing$Bobot[which.min(evaluasi_testing$RMSE)]; forecast_final <- hasil_forecast_test[[bobot_terbaik]]
cat("\nEVALUASI AKURASI TESTING\n"); print(evaluasi_testing); cat("\nMODEL DENGAN RMSE TESTING TERENDAH:",bobot_terbaik,"\n")
forecast_all_wide <- Reduce(function(x,y) merge(x,y,by=c("Periode","Provinsi"),all=TRUE),c(list(matriks_ke_long(aktual_test,"Aktual")),lapply(names(hasil_forecast_test),function(nm) matriks_ke_long(hasil_forecast_test[[nm]],unname(label_model[nm])))))
# =====================================================================
# 4.10.2 ROLLING-ORIGIN EVALUATION DENGAN EXPANDING WINDOW
# =====================================================================
# Evaluasi testing tetap dipertahankan seperti pada bagian sebelumnya.
# Rolling-origin ditambahkan sebagai evaluasi tambahan dan hanya berjalan
# sampai periode terakhir data training pada evaluasi testing tetap.
# Dengan demikian, periode testing tetap menjadi holdout dan tidak dipakai
# sebagai target pada rolling-origin ini.
#
# Skenario memakai indeks semester asli:
# training semester 1-23 -> ramal semester 24;
# training semester 1-24 -> ramal semester 25; dan seterusnya.
#
# Karena model GSTAR memakai data yang sudah didiferensiasi, baris model
# yang tersedia akan lebih sedikit daripada indeks semester asli.
# Label cluster CBWM mengikuti pembagian manual yang ditetapkan pada Bagian IV.5.
# Bobot NCCF dan CBWM-NCCF dihitung ulang pada setiap jendela training.
buat_W_NCCF_origin <- function(Z_ref) {
  Z_ref <- as.matrix(Z_ref)
  n_lokasi <- ncol(Z_ref)
  W <- matrix(0, n_lokasi, n_lokasi,
              dimnames = list(colnames(Z_ref), colnames(Z_ref)))
  if (nrow(Z_ref) < 3) stop("Data training terlalu pendek untuk menghitung NCCF.")
  for (i in seq_len(n_lokasi)) {
    for (j in seq_len(n_lokasi)) {
      if (i != j) {
        nilai_korelasi <- suppressWarnings(
          stats::cor(Z_ref[, i], Z_ref[, j], use = "pairwise.complete.obs")
        )
        if (is.finite(nilai_korelasi)) W[i, j] <- abs(nilai_korelasi)
      }
    }
  }
  diag(W) <- 0
  jumlah_baris <- rowSums(W)
  baris_valid <- jumlah_baris > 0
  W[baris_valid, ] <- W[baris_valid, , drop = FALSE] / jumlah_baris[baris_valid]
  W[!is.finite(W)] <- 0
  W
}
# Estimator OLS dengan generalized inverse untuk menjaga kestabilan
# ketika sebagian prediktor berkorelasi kuat atau tidak teridentifikasi.
# Fungsi ini khusus untuk rolling-origin; estimasi utama/testing tidak diubah.
estimasi_GSTAR_OLS_rolling <- function(Z_train_origin, W) {
  Z_train_origin <- as.matrix(Z_train_origin)
  W <- as.matrix(W)
  T_origin <- nrow(Z_train_origin)
  N_origin <- ncol(Z_train_origin)
  if (T_origin < 4) stop("Observasi training tidak cukup untuk GSTAR(2,0).")
  if (!all(dim(W) == c(N_origin, N_origin))) {
    stop("Dimensi matriks bobot tidak sesuai dengan jumlah provinsi.")
  }
  nama_parameter <- c("phi1", "lambda1", "phi2", "lambda2")
  beta <- matrix(
    0, nrow = N_origin, ncol = 4,
    dimnames = list(colnames(Z_train_origin), nama_parameter)
  )
  fitted_value <- matrix(
    NA_real_, nrow = T_origin - 2, ncol = N_origin,
    dimnames = list(rownames(Z_train_origin)[3:T_origin], colnames(Z_train_origin))
  )
  residual <- fitted_value
  for (i in seq_len(N_origin)) {
    X <- matrix(NA_real_, nrow = T_origin - 2, ncol = 4)
    Y0 <- as.numeric(Z_train_origin[3:T_origin, i])
    for (t in 3:T_origin) {
      lag1 <- as.numeric(Z_train_origin[t - 1, ])
      lag2 <- as.numeric(Z_train_origin[t - 2, ])
      X[t - 2, ] <- c(
        Z_train_origin[t - 1, i],
        spatial_lag(lag1, W)[i],
        Z_train_origin[t - 2, i],
        spatial_lag(lag2, W)[i]
      )
    }
    koefisien <- as.numeric(MASS::ginv(X) %*% Y0)
    beta[i, ] <- koefisien
    fitted_value[, i] <- as.numeric(X %*% koefisien)
    residual[, i] <- Y0 - fitted_value[, i]
  }
  list(beta = beta, fitted = fitted_value, residual = residual)
}
# Inversi diferensiasi untuk satu langkah ramalan.
# level_history hanya memuat periode yang sudah tersedia sampai origin.
inverse_forecast_origin <- function(forecast_diff, level_history, d_global) {
  forecast_diff <- as.matrix(forecast_diff)
  level_history <- as.matrix(level_history)
  if (d_global == 1 && nrow(level_history) < 1) {
    stop("Riwayat level tidak cukup untuk inversi diferensiasi pertama.")
  }
  if (d_global == 2 && nrow(level_history) < 2) {
    stop("Riwayat level tidak cukup untuk inversi diferensiasi kedua.")
  }
  hasil <- forecast_diff
  for (j in seq_len(ncol(forecast_diff))) {
    if (d_global == 0) {
      hasil[, j] <- forecast_diff[, j]
    } else if (d_global == 1) {
      nilai_sebelumnya <- tail(level_history[, j], 1)
      for (h in seq_len(nrow(forecast_diff))) {
        nilai_sebelumnya <- nilai_sebelumnya + forecast_diff[h, j]
        hasil[h, j] <- nilai_sebelumnya
      }
    } else if (d_global == 2) {
      nilai_t1 <- tail(level_history[, j], 1)
      nilai_t2 <- tail(level_history[, j], 2)[1]
      for (h in seq_len(nrow(forecast_diff))) {
        nilai_baru <- forecast_diff[h, j] + 2 * nilai_t1 - nilai_t2
        hasil[h, j] <- nilai_baru
        nilai_t2 <- nilai_t1
        nilai_t1 <- nilai_baru
      }
    } else {
      stop("Fungsi inversi hanya mendukung d_global = 0, 1, atau 2.")
    }
  }
  dimnames(hasil) <- dimnames(forecast_diff)
  hasil
}
# Susun indeks seluruh semester pada skala data asli.
periode_asli_urut <- data_long %>%
  dplyr::mutate(Periode_Model = paste0(Tahun, "_S", Semester)) %>%
  dplyr::distinct(Periode_Model, Waktu) %>%
  dplyr::arrange(Waktu) %>%
  dplyr::pull(Periode_Model)
if (anyDuplicated(periode_asli_urut)) {
  stop("Ditemukan periode semester duplikat pada data asli.")
}
if (!all(periode_asli_urut %in% rownames(level_matrix))) {
  stop("Ada periode asli yang tidak ditemukan dalam level_matrix.")
}
if (!all(periode_asli_urut %in% rownames(aktual_matrix))) {
  stop("Ada periode asli yang tidak ditemukan dalam aktual_matrix.")
}
level_matrix_rolling <- level_matrix[
  match(periode_asli_urut, rownames(level_matrix)), , drop = FALSE
]
aktual_matrix_rolling <- aktual_matrix[
  match(periode_asli_urut, rownames(aktual_matrix)), , drop = FALSE
]
rownames(level_matrix_rolling) <- periode_asli_urut
rownames(aktual_matrix_rolling) <- periode_asli_urut
periode_model_urut <- periode_asli_urut[periode_asli_urut %in% rownames(Z)]
Z_rolling_all <- Z[
  match(periode_model_urut, rownames(Z)), , drop = FALSE
]
rownames(Z_rolling_all) <- periode_model_urut
# Batasi target rolling-origin sampai akhir training pada evaluasi testing tetap,
# sehingga observasi testing tidak digunakan sebagai target rolling-origin.
indeks_akhir_training_tetap <- match(tail(periode_train, 1), periode_asli_urut)
indeks_awal_origin <- 23L
indeks_origin_terakhir <- indeks_akhir_training_tetap - 1L
if (is.na(indeks_akhir_training_tetap) ||
    indeks_origin_terakhir < indeks_awal_origin) {
  stop("Data tidak cukup untuk rolling-origin mulai training semester 1-23.")
}
indeks_origin <- seq.int(indeks_awal_origin, indeks_origin_terakhir)
rolling_skenario <- data.frame(
  Skenario = seq_along(indeks_origin),
  Indeks_Training_Awal = 1L,
  Indeks_Training_Akhir = indeks_origin,
  Periode_Training_Awal = periode_asli_urut[1],
  Periode_Training_Akhir = periode_asli_urut[indeks_origin],
  Indeks_Peramalan = indeks_origin + 1L,
  Periode_Diramalkan = periode_asli_urut[indeks_origin + 1L],
  Jumlah_Observasi_Model_Training = vapply(
    indeks_origin,
    function(i) as.integer(sum(periode_model_urut %in% periode_asli_urut[seq_len(i)])),
    integer(1)
  ),
  stringsAsFactors = FALSE
)
cat("\n4.10.2 ROLLING-ORIGIN: DAFTAR SKENARIO\n")
print(rolling_skenario)
hasil_rolling_list <- list()
status_rolling_list <- list()
counter_rolling <- 0L
for (ii in seq_along(indeks_origin)) {
  origin_idx <- indeks_origin[ii]
  periode_training_raw <- periode_asli_urut[seq_len(origin_idx)]
  periode_target <- periode_asli_urut[origin_idx + 1L]
  baris_training_model <- which(rownames(Z_rolling_all) %in% periode_training_raw)
  Z_train_origin <- Z_rolling_all[baris_training_model, , drop = FALSE]
  level_history_origin <- level_matrix_rolling[periode_training_raw, , drop = FALSE]
  # Bobot NCCF diestimasi ulang hanya dari data model yang tersedia pada origin ini.
  W_NCCF_origin <- buat_W_NCCF_origin(Z_train_origin)
  W_CBWM_NCCF_origin <- bangun_CBWM(cluster_sulawesi, W_NCCF_origin)
  daftar_W_origin <- list(
    IDW = W_IDW,
    NCCF = W_NCCF_origin,
    CBWM_IDW = W_CBWM_IDW,
    CBWM_NCCF = W_CBWM_NCCF_origin
  )
  for (nama_W_origin in names(daftar_W_origin)) {
    counter_rolling <- counter_rolling + 1L
    W_origin <- daftar_W_origin[[nama_W_origin]]
    hasil_satu <- tryCatch({
      model_origin <- estimasi_GSTAR_OLS_rolling(Z_train_origin, W_origin)
      forecast_diff_origin <- forecast_GSTAR_OLS_final(
        model_origin,
        Z_train_origin,
        h = 1,
        W = W_origin,
        periode_forecast = periode_target
      )
      forecast_level_transformasi <- inverse_forecast_origin(
        forecast_diff_origin,
        level_history_origin,
        d_global
      )
      forecast_asli <- inverse_transformasi_manual(
        forecast_level_transformasi,
        ringkasan_boxcox
      )
      aktual_target <- aktual_matrix_rolling[periode_target, , drop = FALSE]
      prediksi_df <- data.frame(
        Skenario = ii,
        Indeks_Training_Awal = 1L,
        Indeks_Training_Akhir = origin_idx,
        Periode_Training_Awal = periode_asli_urut[1],
        Periode_Training_Akhir = periode_asli_urut[origin_idx],
        Indeks_Peramalan = origin_idx + 1L,
        Periode_Diramalkan = periode_target,
        Jumlah_Observasi_Model_Training = nrow(Z_train_origin),
        Model = nama_W_origin,
        Provinsi = colnames(forecast_asli),
        Aktual = as.numeric(aktual_target[1, ]),
        Prediksi = as.numeric(forecast_asli[1, ]),
        stringsAsFactors = FALSE
      )
      prediksi_df$Error <- prediksi_df$Prediksi - prediksi_df$Aktual
      prediksi_df$APE <- ifelse(
        is.finite(prediksi_df$Aktual) & prediksi_df$Aktual != 0,
        abs(prediksi_df$Error / prediksi_df$Aktual) * 100,
        NA_real_
      )
      list(ok = TRUE, prediksi = prediksi_df, pesan = "Berhasil")
    }, error = function(e) {
      list(ok = FALSE, prediksi = NULL, pesan = conditionMessage(e))
    })
    if (isTRUE(hasil_satu$ok)) {
      hasil_rolling_list[[length(hasil_rolling_list) + 1L]] <- hasil_satu$prediksi
    }
    status_rolling_list[[length(status_rolling_list) + 1L]] <- data.frame(
      Skenario = ii,
      Indeks_Training_Akhir = origin_idx,
      Periode_Training_Akhir = periode_asli_urut[origin_idx],
      Indeks_Peramalan = origin_idx + 1L,
      Periode_Diramalkan = periode_target,
      Model = nama_W_origin,
      Status = ifelse(isTRUE(hasil_satu$ok), "Berhasil", "Gagal"),
      Pesan = hasil_satu$pesan,
      stringsAsFactors = FALSE
    )
  }
  cat("Rolling-origin skenario", ii, "dari", length(indeks_origin),
      "| training sampai", periode_asli_urut[origin_idx],
      "| target", periode_target, "\n")
}
rolling_origin_prediksi <- dplyr::bind_rows(hasil_rolling_list)
rolling_origin_status <- dplyr::bind_rows(status_rolling_list)
if (nrow(rolling_origin_prediksi) > 0) {
  rolling_origin_akurasi_skenario <- rolling_origin_prediksi %>%
    dplyr::group_by(
      Skenario, Indeks_Training_Akhir, Periode_Training_Akhir,
      Indeks_Peramalan, Periode_Diramalkan, Model
    ) %>%
    dplyr::summarise(
      RMSE = if (sum(is.finite(Error)) == 0) NA_real_
      else sqrt(mean(Error[is.finite(Error)]^2)),
      MAE = if (sum(is.finite(Error)) == 0) NA_real_
      else mean(abs(Error[is.finite(Error)])),
      MAPE = if (sum(is.finite(APE)) == 0) NA_real_
      else mean(APE[is.finite(APE)]),
      Jumlah_Provinsi_Dievaluasi = sum(is.finite(Error)),
      .groups = "drop"
    )
  rolling_origin_akurasi_global <- rolling_origin_prediksi %>%
    dplyr::group_by(Model) %>%
    dplyr::summarise(
      RMSE = if (sum(is.finite(Error)) == 0) NA_real_
      else sqrt(mean(Error[is.finite(Error)]^2)),
      MAE = if (sum(is.finite(Error)) == 0) NA_real_
      else mean(abs(Error[is.finite(Error)])),
      MAPE = if (sum(is.finite(APE)) == 0) NA_real_
      else mean(APE[is.finite(APE)]),
      Jumlah_Forecast_Valid = sum(is.finite(Error)),
      .groups = "drop"
    ) %>%
    dplyr::arrange(RMSE)
  rolling_origin_akurasi_provinsi <- rolling_origin_prediksi %>%
    dplyr::group_by(Model, Provinsi) %>%
    dplyr::summarise(
      RMSE = if (sum(is.finite(Error)) == 0) NA_real_
      else sqrt(mean(Error[is.finite(Error)]^2)),
      MAE = if (sum(is.finite(Error)) == 0) NA_real_
      else mean(abs(Error[is.finite(Error)])),
      MAPE = if (sum(is.finite(APE)) == 0) NA_real_
      else mean(APE[is.finite(APE)]),
      Jumlah_Forecast_Valid = sum(is.finite(Error)),
      .groups = "drop"
    ) %>%
    dplyr::arrange(Model, RMSE)
} else {
  rolling_origin_akurasi_skenario <- data.frame()
  rolling_origin_akurasi_global <- data.frame()
  rolling_origin_akurasi_provinsi <- data.frame()
}
cat("\n4.10.2 AKURASI ROLLING-ORIGIN PER SKENARIO\n")
print(rolling_origin_akurasi_skenario)
cat("\n4.10.2 RINGKASAN AKURASI ROLLING-ORIGIN\n")
print(rolling_origin_akurasi_global)
cat("\n4.10.2 AKURASI ROLLING-ORIGIN PER PROVINSI\n")
print(rolling_origin_akurasi_provinsi)
openxlsx::write.xlsx(
  list(
    Daftar_Skenario = rolling_skenario,
    Prediksi_per_Origin = rolling_origin_prediksi,
    Akurasi_per_Skenario = rolling_origin_akurasi_skenario,
    Akurasi_Global = rolling_origin_akurasi_global,
    Akurasi_per_Provinsi = rolling_origin_akurasi_provinsi,
    Status_Eksekusi = rolling_origin_status
  ),
  file.path(folder_hasil, "4.10.2_Rolling_Origin_Evaluation.xlsx"),
  overwrite = TRUE
)
# Workbook utama: hasil evaluasi testing tetap disertakan, ditambah hasil rolling-origin.
openxlsx::write.xlsx(
  list(
    Pembagian_Data = pembagian_data,
    Korelasi_Pearson = matriks_korelasi_pearson,
    Ringkasan_BoxCox = ringkasan_boxcox,
    Ringkasan_ADF = ringkasan_adf,
    Klaster_Dominan_STDBSCAN = cluster_dominan,
    Klaster_CBWM_Ditetapkan = cluster_cbwm_ditetapkan,
    Bobot_IDW = W_IDW,
    Bobot_NCCF = W_NCCF,
    Bobot_CBWM_IDW = W_CBWM_IDW,
    Bobot_CBWM_NCCF = W_CBWM_NCCF,
    Ringkasan_MACF = ringkasan_MACF,
    Ringkasan_MPACF = ringkasan_MPACF,
    AIC = hasil_gstarma,
    Parameter_GSTAR = parameter_lengkap,
    Diagnostik_Univariat = hasil_diagnostik_univariat,
    Portmanteau = hasil_portmanteau,
    Mardia = hasil_mardia,
    Forecast_Semua_Metode = forecast_all_wide,
    Evaluasi_Training = evaluasi_training,
    Evaluasi_Testing = evaluasi_testing,
    Evaluasi_Per_Provinsi = evaluasi_per_provinsi,
    Rolling_Origin_Skenario = rolling_skenario,
    Rolling_Origin_Prediksi = rolling_origin_prediksi,
    Rolling_Origin_Akurasi_Skenario = rolling_origin_akurasi_skenario,
    Rolling_Origin_Akurasi_Global = rolling_origin_akurasi_global,
    Rolling_Origin_Akurasi_Provinsi = rolling_origin_akurasi_provinsi,
    Rolling_Origin_Status = rolling_origin_status
  ),
  file.path(folder_hasil, "Hasil_Analisis_Tesis_STDBSCAN_CBWM_GSTAR.xlsx"),
  overwrite = TRUE
)
# =====================================================================
# IV.11 PEMBAHASAN PENGEMBANGAN CBWM MENGGUNAKAN ST-DBSCAN PADA MODEL GSTAR
# =====================================================================
cat("\nIV.11 merupakan bagian pembahasan/narasi tesis dan tidak menambah metode komputasi baru.\n")
cat("\nSELURUH ANALISIS SESUAI STRUKTUR BAB IV SELESAI.\nOutput hasil:",normalizePath(folder_hasil),"\n")
