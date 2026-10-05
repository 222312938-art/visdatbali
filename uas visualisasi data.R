# Pariwisata Bali: dari Mana Wisatawan Datang dan Apa Dampaknya bagi Ekonomi Pulau
# UAS Visualisasi Data dan Informasi 2026 | R Shiny  (versi desain diperbarui)
# Jalankan: shiny::runApp()  (data ada di folder data/)

suppressPackageStartupMessages({
  library(shiny); library(bslib); library(readxl); library(dplyr); library(tidyr)
  library(plotly); library(leaflet); library(sf); library(classInt)
})
sf_use_s2(FALSE)

# ---------------------------------------------------------------- 1. Konfigurasi
XLSX      <- "sumberdata/vidsat_uas.xlsx"
GEOJSON   <- "sumberdata/dataKabupaten-Kota (Provinsi Bali).geojson"  # batas wilayah GADM v4 (non-BPS)
TGL_AKSES <- "4 Oktober 2026"
SATUAN    <- "miliar Rp"   # TODO: pastikan harga berlaku/konstan sesuai tabel sumber PDRB
SRC <- list(
  wisman = list(judul = "Banyaknya Wisatawan Mancanegara yang Datang Langsung ke Bali menurut Kebangsaan",
                tahun = "2019-2025", url = "https://bali.bps.go.id/en/statistics-table?subject=561"),
  pintu  = list(judul = "Banyaknya Wisatawan Mancanegara Bulanan ke Bali menurut Pintu Masuk",
                tahun = "2019-2025",
                url = "https://bali.bps.go.id/en/statistics-table/2/MTA2IzI=/banyaknya-wisatawan-mancanegara-bulanan-ke-bali-menurut-pintu-masuk.html"),
  pdrb   = list(judul = "PDRB Kabupaten/Kota menurut Lapangan Usaha (Tinjauan Regional PDRB Kab/Kota, Buku 2 Jawa-Bali)",
                tahun = "2021-2025",
                url = "https://www.bps.go.id/id/publication/2025/09/15/a1daf2f268d53d17506677ac"))
# Palet Okabe-Ito (ramah buta warna)
COL_KAW <- c("Asia Pasifik" = "#0072B2", "ASEAN" = "#E69F00", "Eropa" = "#009E73",
             "Amerika" = "#CC79A7", "Afrika" = "#D55E00")
BALI <- c(lat = -8.65, lon = 115.22)

# Token desain
C_INK <- "#12343B"; C_SEA <- "#0B6E7F"; C_SAND <- "#F6F1E7"; C_SUN <- "#E07A3F"; C_MUTED <- "#5B6B70"
FONT  <- "Plus Jakarta Sans, system-ui, sans-serif"

fmt  <- function(x, d = 0) formatC(x, format = "f", digits = d, big.mark = ".", decimal.mark = ",")
rgba <- function(hex, a) { m <- col2rgb(hex); sprintf("rgba(%d,%d,%d,%.2f)", m[1, ], m[2, ], m[3, ], a) }
sumber <- function(s, extra = NULL) {
  tags$p(class = "sumber", tags$b("Sumber: BPS"), " | ", s$judul, " (tahun data ", s$tahun, "). ",
         tags$a(href = s$url, target = "_blank", s$url), ". Diakses ", TGL_AKSES, ". ", extra)
}
# Gaya plotly seragam (font, latar transparan, grid halus)
tema <- function(p) p %>% layout(font = list(family = FONT, color = C_INK),
                                 paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
                                 hoverlabel = list(font = list(family = FONT))) %>% config(displaylogo = FALSE)

CSS <- sprintf("
  body{background:%s;color:%s}
  h3,h4,.hero h1{font-family:'Fraunces',Georgia,serif;font-weight:700;color:%s;letter-spacing:-.01em}
  .navbar{background:linear-gradient(90deg,%s,#0A5563)!important;box-shadow:0 2px 12px rgba(0,0,0,.15)}
  .navbar .navbar-brand{font-family:'Fraunces',serif;font-weight:700;font-size:1.3rem;color:#fff!important}
  .navbar .nav-link{color:rgba(255,255,255,.78)!important;font-weight:500;border-bottom:3px solid transparent;margin:0 .15rem}
  .navbar .nav-link:hover{color:#fff!important}
  .navbar .nav-link.active{color:#fff!important;border-bottom-color:%s}
  .hero{background:linear-gradient(135deg,%s 0%%,#1C93A5 60%%,#F2B26B 140%%);color:#fff;border-radius:18px;
        padding:2.2rem 2.4rem;margin:1.2rem 0 1.4rem;box-shadow:0 10px 30px rgba(11,110,127,.25)}
  .hero h1{color:#fff;font-size:2rem;margin-bottom:.6rem}
  .hero p{max-width:760px;font-size:1.02rem;opacity:.95;margin:0}
  .intro{color:%s;max-width:820px}
  .card{border:1px solid rgba(18,52,59,.08)!important;border-radius:16px!important;
        box-shadow:0 4px 18px rgba(18,52,59,.07);margin-bottom:1.1rem;overflow:hidden}
  .card-header{background:#fff!important;font-weight:600;color:%s;border-bottom:3px solid %s!important}
  .bslib-value-box{border-radius:16px!important;box-shadow:0 4px 18px rgba(18,52,59,.1);margin-bottom:1rem}
  .bslib-sidebar-layout>.sidebar{background:#FFFDF8;border-right:1px solid rgba(18,52,59,.08)}
  .sidebar-title{font-family:'Fraunces',serif;color:%s}
  .nav-tabs .nav-link.active{color:%s;font-weight:600;border-bottom:3px solid %s}
  .insight{background:#fff;border-left:5px solid %s;border-radius:10px;padding:.8rem 1.1rem;
           box-shadow:0 2px 10px rgba(18,52,59,.06);margin:.4rem 0 1rem}
  .sumber{font-size:.78rem;color:%s;margin-top:.6rem}
  .sumber a{color:%s;word-break:break-all}
  .container-fluid{max-width:1400px}
", C_SAND, C_INK, C_INK, C_SEA, C_SUN, C_SEA, C_MUTED, C_INK, C_SUN, C_SEA, C_SEA, C_SUN, C_SUN, C_MUTED, C_SEA)

CSS <- paste0(CSS, "
  .k-card{background:#fff;border-radius:16px;padding:1.1rem 1.3rem;box-shadow:0 4px 18px rgba(18,52,59,.07);
          border-top:6px solid var(--k);height:100%;margin-bottom:1rem}
  .k-card h5{font-family:'Fraunces',serif;font-weight:700;margin:.1rem 0 .15rem}
  .k-card .vis{font-size:.78rem;color:#5B6B70;margin-bottom:.6rem}
  .k-card ul{padding-left:1.1rem;margin:0;font-size:.92rem}
  .k-card li{margin-bottom:.45rem}
  .k-bar{background:linear-gradient(90deg,#12343B,#0B6E7F);color:#fff;border-radius:16px;padding:1.3rem 1.6rem;
         box-shadow:0 8px 24px rgba(11,110,127,.25);margin:.4rem 0 1.2rem}
  .k-bar h5{font-family:'Fraunces',serif;color:#fff;font-weight:700}
  .k-bar p{margin:.3rem 0 0;opacity:.96}
")

# ---------------------------------------------------------------- 2. Persiapan data
angka      <- function(x) if (is.numeric(x)) x else as.numeric(gsub("[^0-9.-]", "", x))  # "37, 043" -> 37043
drop_empty <- function(d) d[, colSums(!is.na(d)) > 0, drop = FALSE]

negara <- read_excel(XLSX, "negara_asal") %>% drop_empty() %>%
  rename(kawasan = Kawasan, negara = `Negara (kebangsaan)`) %>%
  mutate(across(-c(kawasan, negara), angka)) %>%
  pivot_longer(-c(kawasan, negara), names_to = "tahun", values_to = "wisman") %>%
  mutate(tahun = as.integer(tahun))

tren <- read_excel(XLSX, "Pintu_masuk") %>% drop_empty() %>% rename(jenis = `Pintu Masuk`) %>%
  mutate(across(-jenis, angka)) %>%
  pivot_longer(-jenis, names_to = "tahun", values_to = "wisman") %>%
  mutate(tahun = as.integer(tahun))
total_thn <- tren %>% filter(jenis == "Total")

# koordinat ibukota (perkiraan, data pendukung NON-BPS)
koord <- tribble(
  ~negara, ~lat, ~lon,
  "Australia", -35.28, 149.13, "India", 28.61, 77.21, "China", 39.90, 116.41,
  "Korea Selatan", 37.57, 126.98, "Jepang", 35.68, 139.69, "Taiwan", 25.03, 121.57,
  "Malaysia", 3.14, 101.69, "Singapore", 1.35, 103.82, "Philippines", 14.60, 120.98,
  "Thailand", 13.75, 100.50, "Vietnam", 21.03, 105.85, "Myanmar", 19.76, 96.08,
  "Cambodia", 11.56, 104.92, "Laos", 17.97, 102.60, "Brunei Darussalam", 4.89, 114.94,
  "United Kingdom", 51.51, -0.13, "France", 48.86, 2.35, "Germany", 52.52, 13.40,
  "Russia", 55.76, 37.62, "Netherlands", 52.37, 4.90, "Italy", 41.90, 12.50,
  "Spain", 40.42, -3.70, "United States", 38.91, -77.04, "Canada", 45.42, -75.70,
  "Afrika Selatan", -25.75, 28.19)

# PDRB agregat per kab/kota (Sheet1): koreksi satuan Klungkung (nilai x1000 -> bagi 1000)
s1 <- read_excel(XLSX, "Sheet1") %>% drop_empty()
names(s1) <- c("kota", "tahun", "total", "akom", "share_file")
s1 <- s1 %>% mutate(f = ifelse(total > 1e6, 1000, 1), total = total / f, akom = akom / f) %>%
  select(kota, tahun, total, akom)

# PDRB per sektor (hirarki_pdrb): koreksi satuan, isi nilai akomodasi yang kosong dari Sheet1
pdrb <- read_excel(XLSX, "hirarki_pdrb") %>% drop_empty() %>%
  rename(kota = Kota, tahun = Tahun, kelompok = `Kelompok Sektor`, sektor = `Kategori/Sektor`, nilai = `Nilai PDRB`)
faktor <- pdrb %>% group_by(kota) %>% summarise(f = ifelse(max(nilai, na.rm = TRUE) > 1e6, 1000, 1), .groups = "drop")
pdrb <- pdrb %>% left_join(faktor, by = "kota") %>% mutate(nilai = nilai / f) %>% select(-f) %>%
  left_join(s1 %>% select(kota, tahun, akom), by = c("kota", "tahun")) %>%
  mutate(nilai = ifelse(is.na(nilai) & grepl("Akomodasi", sektor), akom, nilai)) %>% select(-akom) %>%
  arrange(kota, sektor, tahun) %>% group_by(kota, sektor) %>% mutate(prev = lag(nilai)) %>% ungroup()

# Geospasial: total PDRB dihitung dari rincian sektor bila ada (cek konsistensi dengan Sheet1)
tot_h <- pdrb %>% filter(tahun == max(tahun)) %>% group_by(kota) %>% summarise(total_h = sum(nilai, na.rm = TRUE), .groups = "drop")
geo_tab <- s1 %>% filter(tahun == max(tahun)) %>% left_join(tot_h, by = "kota") %>%
  mutate(total_pakai = coalesce(total_h, total), share = 100 * akom / total_pakai)
cari_geojson <- function() {
  cand <- c(GEOJSON, file.path("data", basename(GEOJSON)), basename(GEOJSON),
            list.files(".", "Kabupaten.*\\.geojson$", recursive = TRUE, full.names = TRUE))
  cand[file.exists(cand)][1]
}
muat_peta <- function() {
  f <- cari_geojson()
  if (is.na(f)) stop("File GeoJSON batas kab/kota tidak ditemukan. Letakkan 'Kabupaten-Kota__Provinsi_Bali_.geojson' di folder data/ (sejajar app.R).")
  g  <- st_read(f, quiet = TRUE) %>% st_make_valid()
  nm <- intersect(c("NAME_2", "WADMKK", "NAMOBJ", "KABKOT", "kabkot"), names(g))[1]
  if (is.na(nm)) stop("Kolom nama kabupaten/kota tidak ditemukan di GeoJSON. Kolom tersedia: ", paste(names(g), collapse = ", "))
  g <- g %>% mutate(kota = .data[[nm]]) %>% select(kota) %>% left_join(geo_tab, by = "kota")
  if (anyNA(g$share)) stop("Nama wilayah GeoJSON tidak cocok dengan data: ", paste(g$kota[is.na(g$share)], collapse = ", "))
  list(sf = g, xy = st_coordinates(st_centroid(g)))
}

# ---------------------------------------------------------------- 3. Fungsi bantu visual
# Kurva busur dari asal ke Bali (Bezier kuadratik)
arc <- function(lat1, lon1, lat2, lon2, n = 30, bend = 0.15) {
  t <- seq(0, 1, length.out = n)
  cx <- (lon1 + lon2) / 2 - (lat2 - lat1) * bend
  cy <- (lat1 + lat2) / 2 + (lon2 - lon1) * bend
  data.frame(lng = (1 - t)^2 * lon1 + 2 * (1 - t) * t * cx + t^2 * lon2,
             lat = (1 - t)^2 * lat1 + 2 * (1 - t) * t * cy + t^2 * lat2)
}

# Warna sunburst: cincin dalam = kelompok sektor, cincin luar = sektor (palet mirip Tableau)
KEL_COL <- c(Tersier = "#D9534F", Sekunder = "#E8963A", Primer = "#4E79A7")
T20 <- c("#4E79A7","#A0CBE8","#F28E2B","#FFBE7D","#59A14F","#8CD17D","#B6992D","#F1CE63","#499894","#86BCB6",
         "#E15759","#FF9D9A","#79706E","#BAB0AC","#D37295","#FABFD2","#B07AA1","#D4A6C8","#9D7660","#D7B5A6")
SEK_COL <- setNames(rep_len(T20, n_distinct(pdrb$sektor)), sort(unique(pdrb$sektor)))

plot_sun <- function(d) {
  d   <- d %>% mutate(v = round(nilai * 100))
  sek <- d %>% group_by(kelompok, sektor) %>% summarise(v = sum(v), .groups = "drop")
  kel <- sek %>% group_by(kelompok) %>% summarise(v = sum(v), .groups = "drop")
  tot <- sum(sek$v)
  n <- bind_rows(
    tibble(id = "Bali", label = "Bali", parent = "", v = tot, col = "#FFFFFF",
           txt = sprintf("<b>%s</b><br>Total PDRB", fmt(tot / 100, 0))),
    kel %>% transmute(id = kelompok, label = kelompok, parent = "Bali", v, col = unname(KEL_COL[kelompok]),
                      txt = sprintf("<b>%s</b><br>%s%%", kelompok, fmt(100 * v / tot, 0))),
    sek %>% transmute(id = paste(kelompok, sektor, sep = "|"), label = sektor, parent = kelompok, v,
                      col = unname(SEK_COL[sektor]), txt = ifelse(100 * v / tot >= 4, paste0(fmt(100 * v / tot, 0), "%"), "")))
  n$col[is.na(n$col)] <- "#999999"
  hov <- sprintf("<b>%s</b><br>PDRB: %s %s<br>Pangsa: %s%%", n$label, fmt(n$v / 100, 1), SATUAN, fmt(100 * n$v / tot, 1))
  plot_ly(type = "sunburst", ids = n$id, labels = n$label, parents = n$parent, values = n$v,
          branchvalues = "total", text = n$txt, textinfo = "text", hovertext = hov, hoverinfo = "text",
          insidetextorientation = "radial", textfont = list(size = 10),
          marker = list(colors = n$col, line = list(width = 2, color = "#FFFFFF"))) %>%
    layout(margin = list(l = 30, r = 30, t = 20, b = 20)) %>% tema()
}

# Pohon hirarki 3 level + akar "Bali" (nilai dalam 0,01 agar jumlah induk = jumlah anak persis)
pohon <- function(d, lv) {
  d <- d %>% mutate(l1 = .data[[lv[1]]], l2 = .data[[lv[2]]], l3 = .data[[lv[3]]],
                    v = round(nilai * 100), p = round(prev * 100))
  n3 <- d %>% transmute(id = paste(l1, l2, l3, sep = "|"), label = l3, parent = paste(l1, l2, sep = "|"), v, p)
  n2 <- d %>% group_by(l1, l2) %>% summarise(v = sum(v), p = sum(p), .groups = "drop") %>%
    transmute(id = paste(l1, l2, sep = "|"), label = l2, parent = l1, v, p)
  n1 <- d %>% group_by(l1) %>% summarise(v = sum(v), p = sum(p), .groups = "drop") %>%
    transmute(id = l1, label = l1, parent = "Bali", v, p)
  n0 <- tibble(id = "Bali", label = "Bali", parent = "", v = sum(d$v), p = sum(d$p))
  bind_rows(n0, n1, n2, n3) %>% mutate(v = v / 100, p = p / 100, growth = 100 * (v / p - 1))
}

plot_hier <- function(n, tipe) {
  mid <- n$growth[n$id == "Bali"]
  rng <- max(abs(n$growth - mid), na.rm = TRUE); if (!is.finite(rng) || rng == 0) rng <- 1
  g   <- ifelse(is.na(n$growth), mid, n$growth)
  hov <- sprintf("<b>%s</b><br>PDRB: %s %s<br>Pertumbuhan: %s%%<br>Pangsa thd total: %s%%",
                 n$label, fmt(n$v, 1), SATUAN, fmt(n$growth, 1), fmt(100 * n$v / n$v[n$id == "Bali"], 1))
  plot_ly(type = tipe, ids = n$id, labels = n$label, parents = n$parent, values = n$v,
          branchvalues = "total", hovertext = hov, hoverinfo = "text", textinfo = "label",
          maxdepth = 3,
          marker = list(colors = g, cmin = mid - rng, cmax = mid + rng, showscale = TRUE,
                        colorscale = list(c(0, "#D55E00"), c(0.5, "#F7F7F7"), c(1, "#0072B2")),
                        colorbar = list(title = list(text = "Pertumbuhan<br>(% y/y)"), len = 0.7, thickness = 14,
                                        outlinewidth = 0),
                        line = list(width = 2, color = "#FFFFFF"))) %>%
    layout(margin = list(l = 0, r = 0, t = 5, b = 0)) %>% tema() %>%
    { if (tipe == "sunburst") . else style(., pathbar = list(visible = TRUE)) }
}

# ---------------------------------------------------------------- 4. Antarmuka
ui <- page_navbar(
  title = "Pariwisata Bali", fillable = FALSE,
  theme = bs_theme(version = 5, primary = C_SEA, secondary = C_SUN, bg = C_SAND, fg = C_INK,
                   base_font = font_google("Plus Jakarta Sans"), heading_font = font_google("Fraunces")),
  header = tags$head(tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
                     tags$style(HTML(CSS))),
  
  nav_panel("1. Ringkasan",
            div(class = "hero",
                tags$h1("Bali Pulih: dari 50 Kunjungan ke Jutaan, Siapa yang Menikmati Hasilnya?"),
                tags$p("Wisatawan mancanegara (wisman) yang datang langsung ke Bali nyaris hilang saat pandemi lalu pulih melewati level sebelum pandemi. ",
                       "Dashboard ini menelusuri tiga pertanyaan: dari mana mereka datang (Aliran), ke sektor mana uangnya mengalir (Hierarki), ",
                       "dan di mana aktivitas ekonomi pariwisata terkonsentrasi (Geospasial).")),
            fluidRow(column(4, uiOutput("vb_total")), column(4, uiOutput("vb_pulih")), column(4, uiOutput("vb_pasar"))),
            card(card_header("Tren kedatangan wisman langsung ke Bali, 2019-2025 (kunjungan)"),
                 plotlyOutput("p_tren", height = 360), uiOutput("tren_cap"), sumber(SRC$pintu))),
  
  nav_panel("2. Aliran",
            layout_sidebar(
              sidebar = sidebar(title = "Filter", width = 280,
                                selectInput("a_tahun", "Periode (tahun)", choices = sort(unique(negara$tahun)), selected = max(negara$tahun)),
                                checkboxGroupInput("a_kaw", "Asal (kawasan)", choices = names(COL_KAW), selected = names(COL_KAW)),
                                sliderInput("a_n", "Jumlah negara terbesar", min = 3, max = 25, value = 25, step = 1)),
              tags$h4("Dari mana wisatawan datang?"),
              tags$p(class = "intro", "Sankey menunjukkan aliran kawasan -> negara -> Bali (lebar = jumlah kunjungan). Peta aliran menunjukkan jarak: ",
                     "garis tebal dan pendek dari Australia, garis tipis dan panjang dari Eropa."),
              card(card_header("Aliran wisman menurut kawasan dan negara asal menuju Bali (kunjungan)"),
                   plotlyOutput("p_sankey", height = 520)),
              card(card_header("Peta aliran: negara asal -> Bali (kunjungan)"),
                   leafletOutput("peta_alir", height = 460),
                   tags$p(class = "sumber",
                          "Ketebalan garis dan warna (kawasan) mengodekan volume; arah dari titik berwarna menuju lingkaran Bali.")),
              uiOutput("a_cap"), sumber(SRC$wisman))),
  
  nav_panel("3. Hierarki",
            layout_sidebar(
              sidebar = sidebar(title = "Filter", width = 280,
                                sliderInput("h_tahun", "Tahun", min = 2022, max = 2025, value = 2025, step = 1, sep = ""),
                                selectInput("h_wil", "Wilayah (Sunburst)", choices = c("Seluruh Bali", sort(unique(pdrb$kota)))),
                                radioButtons("h_struktur", "Struktur hirarki (Icicle)",
                                             choices = c("Wilayah > Kelompok sektor > Sektor" = "w", "Kelompok sektor > Sektor > Wilayah" = "s"))),
              tags$h4("Uang dari pariwisata mengalir ke sektor mana?"),
              tags$p(class = "intro", "Sunburst: cincin dalam = kelompok sektor, cincin luar = sektor, ukuran irisan = nilai PDRB, angka di tengah = total PDRB. Icicle: ukuran kotak = nilai PDRB, warna = pertumbuhan y/y dibanding rata-rata Bali (biru = di atas, oranye = di bawah). ",
                     "Klik irisan (Sunburst) atau kotak (Icicle) untuk drill-down; klik bagian tengah Sunburst atau jalur posisi (breadcrumb) Icicle untuk kembali."),
              navset_card_tab(
                nav_panel("Sunburst", layout_columns(col_widths = c(8, 4), plotlyOutput("p_sunburst", height = 420, width = "100%"), uiOutput("leg_sun"))),
                nav_panel("Icicle", plotlyOutput("p_icicle", height = 560))),
              uiOutput("h_insight"),
              sumber(SRC$pdrb, paste0("Satuan: ", SATUAN, ". Pertumbuhan dihitung dari nilai PDRB pada data."))) ),
  
  nav_panel("4. Geospasial",
            layout_sidebar(
              sidebar = sidebar(title = "Klasifikasi choropleth", width = 280,
                                selectInput("g_metode", "Metode klasifikasi",
                                            choices = c("Quantile" = "quantile", "Natural breaks (Jenks)" = "jenks", "Equal interval" = "equal")),
                                sliderInput("g_kelas", "Jumlah kelas", min = 3, max = 5, value = 4, step = 1)),
              tags$h4("Di mana aktivitas ekonomi pariwisata terkonsentrasi?"),
              tags$p(class = "intro", "Choropleth = share PDRB akomodasi dan makan minum terhadap PDRB total (%); simbol proporsional = nilai PDRB akomodasi dan makan minum. ",
                     "Cerita bergeser dari 'dari mana wisatawan datang' menjadi 'di mana ekonominya terkonsentrasi'."),
              card(card_header("Share dan nilai PDRB akomodasi & makan minum menurut kabupaten/kota, 2025"),
                   leafletOutput("peta", height = 520)),
              uiOutput("g_insight"), uiOutput("g_just"),
              sumber(SRC$pdrb, "Batas wilayah: GADM v4 (data pendukung non-BPS); kunci gabung: nama kabupaten/kota."))),
  
  nav_panel("5. Kesimpulan",
            tags$h4("Apa yang dikatakan ketiga visualisasi?"),
            tags$p(class = "intro", "Ringkasan temuan dari Sankey dan peta aliran (Aliran), Sunburst dan Icicle (Hierarki), serta choropleth (Geospasial). ",
                   "Angka dihitung otomatis dari data tahun terbaru."),
            layout_columns(col_widths = c(4, 4, 4), uiOutput("k_aliran"), uiOutput("k_hirarki"), uiOutput("k_geo")),
            uiOutput("k_sintesis"),
            sumber(SRC$wisman), sumber(SRC$pdrb)))

# ---------------------------------------------------------------- 5. Server
server <- function(input, output, session) {
  
  geo <- reactive(muat_peta())   # error (jika ada) tampil jelas di kotak peta
  
  # ---- Ringkasan
  tmax <- max(total_thn$tahun)
  output$vb_total <- renderUI(value_box("Wisman langsung ke Bali (kunjungan)", fmt(total_thn$wisman[total_thn$tahun == tmax]),
                                        paste("Tahun", tmax), theme = value_box_theme(bg = C_SEA, fg = "#FFFFFF")))
  output$vb_pulih <- renderUI({
    r <- 100 * total_thn$wisman[total_thn$tahun == tmax] / total_thn$wisman[total_thn$tahun == 2019]
    value_box("Pemulihan terhadap 2019", paste0(fmt(r, 1), "%"), paste("Tahun", tmax, "dibanding 2019"),
              theme = value_box_theme(bg = C_SUN, fg = "#FFFFFF"))
  })
  output$vb_pasar <- renderUI({
    d <- negara %>% filter(tahun == tmax) %>% arrange(desc(wisman)) %>% slice(1)
    value_box("Pasar terbesar", d$negara, paste0(fmt(100 * d$wisman / total_thn$wisman[total_thn$tahun == tmax], 1), "% dari total"),
              theme = value_box_theme(bg = C_INK, fg = "#FFFFFF"))
  })
  output$p_tren <- renderPlotly({
    low <- total_thn %>% slice_min(wisman, n = 1)
    plot_ly(total_thn, x = ~tahun, y = ~wisman, type = "scatter", mode = "lines+markers",
            fill = "tozeroy", fillcolor = "rgba(11,110,127,0.12)",
            line = list(color = C_SEA, width = 3.5, shape = "spline"),
            marker = list(color = "#FFFFFF", size = 10, line = list(color = C_SEA, width = 3)),
            hovertemplate = "%{x}: %{y:,.0f} kunjungan<extra></extra>") %>%
      layout(xaxis = list(title = "Tahun", dtick = 1, showgrid = FALSE),
             yaxis = list(title = "Kunjungan wisman (kunjungan)", tickformat = ",d", gridcolor = "rgba(18,52,59,.08)", zeroline = FALSE),
             annotations = list(list(x = low$tahun, y = low$wisman, text = paste0("Terendah: ", fmt(low$wisman), " kunjungan"),
                                     showarrow = TRUE, ay = -50, arrowcolor = C_SUN, font = list(color = C_SUN, size = 12)))) %>% tema()
  })
  output$tren_cap <- renderUI({
    laut <- tren %>% filter(grepl("Laut", jenis), tahun == tmax)
    div(class = "insight", sprintf("Moda masuk %d: laut %s kunjungan (%s%% dari total), sisanya udara.", tmax, fmt(laut$wisman),
                                   fmt(100 * laut$wisman / total_thn$wisman[total_thn$tahun == tmax], 1)))
  })
  
  # ---- Aliran
  fd <- reactive({
    req(input$a_kaw)
    negara %>% filter(tahun == as.integer(input$a_tahun), kawasan %in% input$a_kaw, !is.na(wisman), wisman > 0) %>%
      arrange(desc(wisman)) %>% slice_head(n = input$a_n)
  })
  output$p_sankey <- renderPlotly({
    d <- fd(); validate(need(nrow(d) > 0, "Tidak ada data untuk filter ini."))
    kaw <- unique(d$kawasan); lab <- c(kaw, d$negara, "Bali"); id <- setNames(seq_along(lab) - 1, lab)
    src <- unname(c(id[d$kawasan], id[d$negara])); tgt <- unname(c(id[d$negara], rep(id["Bali"], nrow(d))))
    cl  <- unname(COL_KAW[d$kawasan])
    plot_ly(type = "sankey", orientation = "h",
            node = list(label = lab, color = c(unname(COL_KAW[kaw]), cl, C_INK), pad = 14, thickness = 18,
                        line = list(color = "white", width = 0.5)),
            link = list(source = src, target = tgt, value = c(d$wisman, d$wisman), color = rgba(c(cl, cl), 0.4))) %>%
      layout(font = list(size = 11), margin = list(l = 5, r = 5, t = 10, b = 10)) %>% tema()
  })
  output$peta_alir <- renderLeaflet({
    d <- fd() %>% left_join(koord, by = "negara"); validate(need(nrow(d) > 0, "Tidak ada data untuk filter ini."))
    w <- 1 + 13 * sqrt(d$wisman / max(d$wisman)); cl <- unname(COL_KAW[d$kawasan])
    lab <- lapply(sprintf("<b>%s</b> -> Bali<br>%s kunjungan (%s)", d$negara, fmt(d$wisman), d$kawasan), HTML)
    m <- leaflet(options = leafletOptions(minZoom = 1, worldCopyJump = TRUE)) %>%
      addProviderTiles("CartoDB.DarkMatter", group = "Peta gelap") %>%
      addProviderTiles("Esri.WorldGrayCanvas", group = "Peta abu-abu") %>%
      addTiles(group = "OpenStreetMap")
    for (i in seq_len(nrow(d))) {
      a <- arc(d$lat[i], d$lon[i], BALI["lat"], BALI["lon"])
      m <- m %>% addPolylines(lng = a$lng, lat = a$lat, weight = w[i], color = cl[i], opacity = 0.8, label = lab[[i]])
    }
    m %>% addCircleMarkers(lng = d$lon, lat = d$lat, radius = 4, color = "#FFFFFF", weight = 1, fillColor = cl,
                           fillOpacity = 1, label = lab) %>%
      addCircleMarkers(lng = BALI["lon"], lat = BALI["lat"], radius = 9, color = C_SUN, weight = 3,
                       fillColor = "#FFFFFF", fillOpacity = 1, label = "Bali (tujuan)") %>%
      addLegend("bottomleft", colors = unname(COL_KAW[unique(d$kawasan)]), labels = unique(d$kawasan), title = "Kawasan asal") %>%
      addControl("<small>Ketebalan garis ~ akar jumlah kunjungan</small>", position = "bottomright") %>%
      addLayersControl(baseGroups = c("Peta gelap", "Peta abu-abu", "OpenStreetMap"), options = layersControlOptions(collapsed = TRUE)) %>%
      fitBounds(lng1 = min(c(d$lon, BALI[["lon"]])) - 5, lat1 = min(c(d$lat, BALI[["lat"]])) - 5,
                lng2 = max(c(d$lon, BALI[["lon"]])) + 5, lat2 = max(c(d$lat, BALI[["lat"]])) + 5)
  })
  output$a_cap <- renderUI({
    d <- fd(); tot <- total_thn$wisman[total_thn$tahun == as.integer(input$a_tahun)]
    div(class = "insight", sprintf("Menampilkan %d negara = %s kunjungan (%s%% dari total %s kunjungan wisman langsung ke Bali tahun %s). Satuan: kunjungan.",
                                   nrow(d), fmt(sum(d$wisman)), fmt(100 * sum(d$wisman) / tot, 1), fmt(tot), input$a_tahun))
  })
  
  # ---- Hierarki
  hdata <- reactive({
    lv <- if (input$h_struktur == "w") c("kota", "kelompok", "sektor") else c("kelompok", "sektor", "kota")
    pohon(pdrb %>% filter(tahun == input$h_tahun, !is.na(nilai)), lv)
  })
  sun_d <- reactive({
    d <- pdrb %>% filter(tahun == as.integer(input$h_tahun), !is.na(nilai))
    if (input$h_wil != "Seluruh Bali") d <- d %>% filter(kota == input$h_wil)
    validate(need(nrow(d) > 0, "Tidak ada data untuk filter ini."))
    d
  })
  output$p_sunburst <- renderPlotly(plot_sun(sun_d()))
  output$leg_sun <- renderUI({
    d <- sun_d()
    item <- function(col, lab) div(style = "display:flex;align-items:center;gap:.5rem;margin:.05rem 0;font-size:.68rem;line-height:1.15",
                                   span(style = sprintf("width:9px;height:9px;border-radius:2px;flex:none;background:%s", col)), lab)
    kel <- d %>% group_by(kelompok) %>% summarise(v = sum(nilai), .groups = "drop") %>% arrange(desc(v))
    sek <- d %>% group_by(sektor) %>% summarise(v = sum(nilai), .groups = "drop") %>% arrange(desc(v))
    div(class = "insight", style = "padding:.5rem .7rem;font-size:.72rem",
        tags$b("Kelompok Sektor"),
        lapply(seq_len(nrow(kel)), function(i) item(unname(KEL_COL[kel$kelompok[i]]), kel$kelompok[i])),
        tags$hr(style = "margin:.4rem 0"), tags$b("Kategori/Sektor"),
        lapply(seq_len(nrow(sek)), function(i) item(unname(SEK_COL[sek$sektor[i]]), sek$sektor[i])))
  })
  output$p_icicle <- renderPlotly(plot_hier(hdata(), "icicle"))
  output$h_insight <- renderUI({
    th <- as.integer(input$h_tahun)
    d  <- pdrb %>% filter(tahun == th, !is.na(nilai))
    validate(need(nrow(d) > 0, "Tidak ada data untuk tahun ini."))
    tot <- sum(d$nilai); tot_prev <- sum(d$prev, na.rm = TRUE)
    g_bali <- if (tot_prev > 0) 100 * (sum(d$nilai[!is.na(d$prev)]) / tot_prev - 1) else NA_real_
    ins <- function(judul, teks) div(class = "insight", tags$b(judul), tags$br(), teks)
    
    a    <- d %>% filter(grepl("Akomodasi", sektor)) %>% arrange(desc(nilai))
    sek  <- d %>% group_by(sektor) %>% summarise(v = sum(nilai), p = sum(prev, na.rm = TRUE), .groups = "drop") %>%
      mutate(g = ifelse(p > 0, 100 * (v / p - 1), NA_real_)) %>% arrange(desc(v))
    kel  <- d %>% group_by(kelompok) %>% summarise(v = sum(nilai), .groups = "drop") %>% arrange(desc(v))
    wil  <- d %>% group_by(kota) %>% summarise(v = sum(nilai), .groups = "drop") %>% arrange(desc(v))
    sa   <- sek %>% filter(grepl("Akomodasi", sektor))
    gs   <- sek %>% filter(!is.na(g)) %>% arrange(desc(g))
    top3 <- a %>% slice_head(n = 3)
    ra   <- d %>% group_by(kota) %>% summarise(sh = 100 * sum(nilai[grepl("Akomodasi", sektor)]) / sum(nilai), .groups = "drop") %>%
      arrange(desc(sh))
    
    kartu <- list(
      ins("Peran akomodasi dan makan minum",
          sprintf("Tahun %s: sektor ini = %s%% dari PDRB %d kabupaten/kota yang tersedia; terbesar di %s (%s %s).",
                  th, fmt(100 * sum(a$nilai) / tot, 1), n_distinct(d$kota), a$kota[1], fmt(a$nilai[1], 0), SATUAN)),
      ins("Sektor terbesar di Bali",
          sprintf("%s adalah sektor terbesar (%s%% dari total), disusul %s (%s%%) dan %s (%s%%).",
                  sek$sektor[1], fmt(100 * sek$v[1] / tot, 1), sek$sektor[2], fmt(100 * sek$v[2] / tot, 1),
                  sek$sektor[3], fmt(100 * sek$v[3] / tot, 1))),
      ins("Kelompok sektor dominan",
          sprintf("Kelompok %s menyumbang %s%% PDRB, sedangkan kelompok terkecil, %s, hanya %s%%.",
                  kel$kelompok[1], fmt(100 * kel$v[1] / tot, 1), kel$kelompok[nrow(kel)], fmt(100 * kel$v[nrow(kel)] / tot, 1))),
      ins("Konsentrasi wilayah",
          sprintf("%s adalah penyumbang PDRB terbesar (%s%% dari total yang tersedia). Untuk akomodasi dan makan minum, tiga wilayah teratas (%s) menguasai %s%% nilai sektor ini.",
                  wil$kota[1], fmt(100 * wil$v[1] / tot, 1), paste(top3$kota, collapse = ", "), fmt(100 * sum(top3$nilai) / sum(a$nilai), 1))),
      ins("Wilayah paling bergantung pada akomodasi",
          sprintf("Terhadap PDRB wilayahnya sendiri, akomodasi dan makan minum paling besar di %s (%s%%) dan paling kecil di %s (%s%%).",
                  ra$kota[1], fmt(ra$sh[1], 1), ra$kota[nrow(ra)], fmt(ra$sh[nrow(ra)], 1))))
    if (nrow(gs) > 0 && nrow(sa) > 0 && is.finite(g_bali) && !is.na(sa$g)) {
      kartu <- c(kartu, list(
        ins("Pertumbuhan dibanding rata-rata Bali",
            sprintf("PDRB total tumbuh %s%% (y/y). Akomodasi dan makan minum tumbuh %s%%, yaitu %s rata-rata Bali (selisih %s poin persentase).",
                    fmt(g_bali, 1), fmt(sa$g, 1), ifelse(sa$g >= g_bali, "di atas", "di bawah"), fmt(abs(sa$g - g_bali), 1))),
        ins("Sektor tumbuh tercepat dan terlambat",
            sprintf("Tercepat: %s (%s%%). Terlambat: %s (%s%%). Warna biru pada grafik menandai sektor yang tumbuh di atas rata-rata Bali.",
                    gs$sektor[1], fmt(gs$g[1], 1), gs$sektor[nrow(gs)], fmt(gs$g[nrow(gs)], 1)))))
    }
    do.call(layout_columns, c(list(col_widths = c(6, 6)), kartu))
  })
  
  # ---- Geospasial
  brks <- reactive({
    b <- tryCatch(unique(classIntervals(geo()$sf$share, n = input$g_kelas, style = input$g_metode)$brks), error = function(e) NULL)
    if (is.null(b) || length(b) < 3) b <- pretty(range(geo()$sf$share), n = input$g_kelas)
    b
  })
  output$peta <- renderLeaflet({
    pal <- colorBin("YlGnBu", domain = geo()$sf$share, bins = brks(), pretty = FALSE)
    r   <- 8 + 30 * sqrt(geo()$sf$akom / max(geo()$sf$akom))
    lab <- lapply(sprintf("<b>%s</b><br>Share akomodasi & makan minum: %s%%<br>PDRB akomodasi & makan minum: %s %s<br>PDRB total: %s %s",
                          geo()$sf$kota, fmt(geo()$sf$share, 1), fmt(geo()$sf$akom, 0), SATUAN, fmt(geo()$sf$total_pakai, 0), SATUAN), HTML)
    G1 <- "Choropleth: share PDRB akomodasi & makan minum (%)"; G2 <- "Simbol: nilai PDRB akomodasi & makan minum"
    v <- c(min(geo()$sf$akom), median(geo()$sf$akom), max(geo()$sf$akom)); rr <- 8 + 30 * sqrt(v / max(geo()$sf$akom))
    leg <- paste0("<div style='background:#fff;padding:8px 10px;border-radius:10px;box-shadow:0 2px 8px rgba(0,0,0,.15)'><b>PDRB akomodasi & makan minum (", SATUAN, ")</b><br>",
                  paste0("<span style='display:inline-block;width:", 2 * rr, "px;height:", 2 * rr,
                         "px;border-radius:50%;background:#D55E00;opacity:.75;vertical-align:middle'></span> ", fmt(v, 0), collapse = "<br>"), "</div>")
    leaflet(geo()$sf) %>% addProviderTiles("Esri.WorldGrayCanvas", group = "Peta dasar terang") %>% addTiles(group = "OpenStreetMap") %>%
      addPolygons(fillColor = ~pal(share), fillOpacity = 0.85, color = "#FFFFFF", weight = 1.5, label = lab, group = G1,
                  highlightOptions = highlightOptions(weight = 3, color = C_INK, bringToFront = FALSE)) %>%
      addCircleMarkers(lng = geo()$xy[, 1], lat = geo()$xy[, 2], radius = r, fillColor = "#D55E00", fillOpacity = 0.75,
                       color = "#FFFFFF", weight = 1.5, label = lab, group = G2) %>%
      addLegend("bottomright", pal = pal, values = ~share, title = "Share PDRB akomodasi<br>& makan minum (%)",
                labFormat = labelFormat(suffix = "%", digits = 1)) %>%
      addControl(leg, position = "bottomleft") %>%
      addLayersControl(baseGroups = c("Peta dasar terang", "OpenStreetMap"), overlayGroups = c(G1, G2),
                       options = layersControlOptions(collapsed = FALSE))
  })
  output$g_insight <- renderUI({
    o <- geo()$sf %>% st_drop_geometry() %>% arrange(desc(share)) %>% slice_head(n = 3)
    div(class = "insight", sprintf("Tiga kabupaten/kota dengan share tertinggi: %s. Pariwisata paling terkonsentrasi di %s.",
                                   paste0(o$kota, " (", fmt(o$share, 1), "%)", collapse = ", "), o$kota[1]))
  })
  output$g_just <- renderUI({
    m <- switch(input$g_metode,
                quantile = "Quantile: tiap kelas berisi jumlah kabupaten/kota yang hampir sama; cocok untuk hanya 9 unit dan sebaran tidak simetris, tetapi batas kelas bisa memisahkan nilai yang berdekatan.",
                jenks    = "Natural breaks (Jenks): batas kelas dicari agar variasi dalam kelas minimal, sehingga kelompok alami (mis. wilayah pariwisata inti) terlihat; batas bergantung pada data.",
                equal    = "Equal interval: lebar kelas sama sehingga mudah dibaca, tetapi bisa menyisakan kelas hampir kosong bila data mengelompok.")
    div(class = "insight small",
        tags$p(tags$b("Mengapa share (%)? "), "Angka absolut condong besar di wilayah yang ekonominya memang besar. Rasio terhadap PDRB total menunjukkan seberapa bergantung ekonomi wilayah pada akomodasi dan makan minum."),
        tags$p(class = "mb-0", tags$b("Metode terpilih. "), m))
  })
  
  # ---- Kesimpulan
  kartu <- function(judul, vis, warna, poin)
    div(class = "k-card", style = paste0("--k:", warna), tags$h5(judul), div(class = "vis", vis),
        tags$ul(lapply(poin, function(p) tags$li(p))))
  tp <- max(pdrb$tahun)
  k_alir <- reactive({
    d   <- negara %>% filter(tahun == tmax, !is.na(wisman), wisman > 0) %>% arrange(desc(wisman))
    tot <- total_thn$wisman[total_thn$tahun == tmax]
    kw  <- d %>% group_by(kawasan) %>% summarise(v = sum(wisman), .groups = "drop") %>% arrange(desc(v))
    t3  <- slice_head(d, n = 3)
    list(d = d, tot = tot, kw = kw, t3 = t3, pulih = 100 * tot / total_thn$wisman[total_thn$tahun == 2019])
  })
  k_hir <- reactive({
    d   <- pdrb %>% filter(tahun == tp, !is.na(nilai)); tot <- sum(d$nilai)
    sek <- d %>% group_by(sektor) %>% summarise(v = sum(nilai), .groups = "drop") %>% arrange(desc(v))
    kel <- d %>% group_by(kelompok) %>% summarise(v = sum(nilai), .groups = "drop") %>% arrange(desc(v))
    ak  <- sum(d$nilai[grepl("Akomodasi", d$sektor)])
    list(tot = tot, sek = sek, kel = kel, ak = ak)
  })
  k_geod <- reactive(geo_tab %>% arrange(desc(share)))
  output$k_aliran <- renderUI({
    a <- k_alir()
    kartu("Aliran: dari mana", "Sankey + peta aliran", "#0072B2", list(
      sprintf("Pada %d, wisman langsung ke Bali mencapai %s kunjungan, %s%% dari level 2019.", tmax, fmt(a$tot), fmt(a$pulih, 1)),
      sprintf("Tiga pasar terbesar: %s; bersama-sama %s%% dari total.",
              paste0(a$t3$negara, " (", fmt(100 * a$t3$wisman / a$tot, 1), "%)", collapse = ", "), fmt(100 * sum(a$t3$wisman) / a$tot, 1)),
      sprintf("Kawasan terbesar adalah %s (%s%% dari seluruh kunjungan negara yang tercatat).", a$kw$kawasan[1], fmt(100 * a$kw$v[1] / sum(a$kw$v), 1)),
      "Garis pada peta aliran memperlihatkan jarak asal; bandingkan ketebalannya antarnegara."))
  })
  output$k_hirarki <- renderUI({
    h <- k_hir()
    kartu("Hierarki: ke mana uangnya", "Sunburst + Icicle", "#E8963A", list(
      sprintf("Kelompok %s paling besar, yaitu %s%% dari PDRB tahun %d.", h$kel$kelompok[1], fmt(100 * h$kel$v[1] / h$tot, 1), tp),
      sprintf("Sektor terbesar adalah %s (%s%%), disusul %s (%s%%).", h$sek$sektor[1], fmt(100 * h$sek$v[1] / h$tot, 1),
              h$sek$sektor[2], fmt(100 * h$sek$v[2] / h$tot, 1)),
      sprintf("Akomodasi dan makan minum menyumbang %s%% PDRB kabupaten/kota yang tersedia.", fmt(100 * h$ak / h$tot, 1)),
      "Warna Icicle memperlihatkan sektor yang tumbuh di atas atau di bawah rata-rata Bali."))
  })
  output$k_geo <- renderUI({
    g <- k_geod(); t3 <- slice_head(g, n = 3); ab <- g %>% arrange(desc(akom)) %>% slice(1)
    kartu("Geospasial: di mana", "Choropleth + simbol proporsional", "#009E73", list(
      sprintf("Share akomodasi dan makan minum tertinggi: %s.", paste0(t3$kota, " (", fmt(t3$share, 1), "%)", collapse = ", ")),
      sprintf("Nilai absolut terbesar ada di %s (%s %s).", ab$kota, fmt(ab$akom, 0), SATUAN),
      sprintf("Selisih share tertinggi dan terendah %s poin persentase (%s di %s vs %s di %s), yang menunjukkan konsentrasi antarwilayah.",
              fmt(max(g$share) - min(g$share), 1), fmt(max(g$share), 1), g$kota[1], fmt(min(g$share), 1), g$kota[nrow(g)]),
      "Share terhadap PDRB total menunjukkan ketergantungan wilayah, bukan sekadar ukuran ekonominya."))
  })
  output$k_sintesis <- renderUI({
    a <- k_alir(); h <- k_hir(); g <- k_geod()
    div(class = "k-bar",
        tags$h5("Benang merah"),
        tags$p(sprintf("Kunjungan wisman pulih ke %s%% dari level 2019, dengan %s sebagai pasar terbesar. Di sisi ekonomi, akomodasi dan makan minum menyumbang %s%% PDRB, dan sektor terbesar secara keseluruhan adalah %s. Secara spasial, ketergantungan paling tinggi ada di %s (%s%% PDRB wilayahnya).",
                       fmt(a$pulih, 1), a$t3$negara[1], fmt(100 * h$ak / h$tot, 1), h$sek$sektor[1], g$kota[1], fmt(g$share[1], 1))),
        tags$p("Artinya: manfaat ekonomi pariwisata tidak merata antarwilayah; wilayah dengan share akomodasi tinggi paling terpapar naik-turunnya kunjungan wisman."))
  })
}

shinyApp(ui, server)

