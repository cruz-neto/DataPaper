### figura 2

library(dplyr)
library(ggplot2)
library(scales)

# 1️⃣ Agregar número de registros por ano
temporal_coverage <- database %>%
  filter(!is.na(ano)) %>%
  count(ano, name = "n_records") %>%
  arrange(ano)

# 2️⃣ Criar gráfico
fig2_temporal <- ggplot(temporal_coverage, aes(x = ano, y = n_records)) +
  geom_col(width = 0.8) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = seq(1980, 2025, by = 5)) +
  labs(
    x = "Year",
    y = "Number of infraction records"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    plot.title = element_blank()
  )

fig2_temporal

ggsave(
  filename = "Figure_2_Temporal_Coverage_1981_2025.png",
  plot = fig2_temporal,
  width = 180,      # mm padrão Nature
  height = 120,
  units = "mm",
  dpi = 600         # alta qualidade
)

########## Figura 3
library(dplyr)
library(ggplot2)
library(sf)
library(geobr)
library(scales)
library(ggspatial)  # para escala e norte

# 1️⃣ Agregar por estado
spatial_coverage <- database %>%
  count(state_abbr, name = "n_records")

# 2️⃣ Carregar shapefile dos estados
br_states <- geobr::read_state(year = 2020, showProgress = FALSE)

# 3️⃣ Unir dados ao mapa
map_data <- br_states %>%
  left_join(spatial_coverage, by = c("abbrev_state" = "state_abbr"))

# 4️⃣ Criar mapa
fig3_spatial <- ggplot(map_data) +
  geom_sf(aes(fill = n_records), color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(
    option = "D",          # viridis padrão — perceptualmente uniforme e acessível
    direction = -1,
    na.value = "grey85",
    trans = "log10",       # escala log para distribuições assimétricas
    labels = label_comma(),
    breaks = c(1, 10, 100, 1000, 10000)  # ajuste conforme seus dados
  ) +
  annotation_scale(
    location = "bl",       # escala no canto inferior esquerdo
    width_hint = 0.3,
    text_cex = 0.7
  ) +
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.3, "in"),
    style = north_arrow_fancy_orienteering(text_size = 8)
  ) +
  labs(fill = "Number of\nrecords") +
  theme_void(base_size = 11) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 9, face = "bold"),
    legend.text  = element_text(size = 8),
    legend.key.height = unit(1.2, "cm")
  )

# 5️⃣ Exportar em resolução adequada para Scientific Data
ggsave(
  filename = "fig3_spatial_distribution.tiff",
  plot     = fig3_spatial,
  width    = 140,      # mm — coluna dupla na Nature
  height   = 120,
  units    = "mm",
  dpi      = 300,
  compression = "lzw"  # TIFF com LZW é o formato preferido pela Nature
)

fig3_spatial

## ggsave(
filename = "Figure_3_Spatial_Coverage_1981_2025.png",
plot = fig3_spatial,
width = 180,
height = 160,
units = "mm",
dpi = 600
)
