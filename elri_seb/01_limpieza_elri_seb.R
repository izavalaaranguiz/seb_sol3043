library(tidyverse)
library(haven)


setwd("~/OneDrive - Universidad Católica de Chile/Nacho UC/Sociología UC (2021-2024)/Magíster 2025/4to Semestre Magíster (12vo Semestre)/Sociología del Estado de Bienestar/seb3043/elri_seb/data_elri")

# ============================================================
# 0. Selección de variables (agrego m, comuna y modo de aplicación)
# ============================================================
elri_ciir<- haven::read_dta("BBDD_ELRI_LONG_4.0.dta")

elri_ciir[] <- lapply(elri_ciir, function(col) {
  if (is.character(col)) {
    iconv(col, from = "latin1", to = "UTF-8", sub = "")
  } else {
    col
  }
})

elri <- elri_ciir %>%
  dplyr::select(folio,
         ano,
         pond,
         m,                      # muestra por diseño: Mapuche/No Mapuche/Andino/No Andino
         comuna,
         "auto_ident" = a1,
         "auto_color" = a9,
         "esc_pig"    = a11,     # escala PERLA (1-11 nominal, revisar rango empírico)
         "color_ent"  = a10,     # clasificación categórica del entrevistador
         "sexo" = g2,
         "edad" = g18,
         "religion" = r1_1,
         "est_civil" = g19,
         "educacion1" = g20,
         "educacion2" = g32_1,
         "actprin" = g23,
         "ISCO" = g21_cod,
         "ISCO_sos" = g22_1_cod,
         "ingreso_h" = g27,
         "region" = region,
         "urb_rur" = urbano_rural,
         "tipo_hogar" = tipo_hogar,
         # Preguntas politica indígena
         "devolver_tierras" = e1_1,
         "consulta_pueblos" = e1_2,
         "crear_ministerio" = e1_3,
         "pais_multicultural" = e1_4
  )


# =============================================================
# Limpieza de variables
# =============================================================

codigos_na <- c(88, 99, 8888, 9999)  # códigos de missing observados en la base (formatos mixtos por ola)


#¿Cuántos folios tienen más de una fila en alguna ola, y en cuáles?
duplicados <- elri %>%
  count(folio, ano) %>%
  filter(n > 1)

elri %>%
  filter(folio %in% duplicados$folio, ano == 2023) %>%
  arrange(folio)

# Limpieza de folios duplicados
folios_duplicados_2023 <- duplicados$folio  # los 3 folios

elri <- elri %>%
  filter(!(folio %in% folios_duplicados_2023 & ano == 2023))

# Confirmar que ya no quedan duplicados
elri %>%
  count(folio, ano) %>%
  filter(n > 1)  # debería devolver 0 filas


folios_panel_completo <- elri %>%
  group_by(folio) %>%
  summarise(n_olas = n_distinct(ano), .groups = "drop") %>%
  filter(n_olas == 4) %>%
  pull(folio)




#

# ============================================================
# Educación armonizada en 4 categorías (Básica y sin estudios /
# Media / Técnica / Universitaria), combinando educacion1 (2016-2018)
# y educacion2 (2021-2023)
# ============================================================

elri <- elri %>%
  mutate(
    educacion1_num = haven::zap_labels(educacion1),
    educacion2_num = haven::zap_labels(educacion2),
    
    educ_4cat = case_when(
      # --- Fuente 2016-2018 (educacion1) ---
      educacion1_num %in% c(1, 2, 3)        ~ "Básica y sin estudios",
      educacion1_num %in% c(4, 5)           ~ "Media",
      educacion1_num %in% c(6, 7, 8, 9)     ~ "Técnica",
      educacion1_num %in% c(10, 11, 12)     ~ "Universitaria",
      educacion1_num %in% c(88, 99, 8888, 9999) ~ NA_character_,
      
      # --- Fuente 2021-2023 (educacion2) ---
      educacion2_num %in% c(1, 2, 3)        ~ "Básica y sin estudios",
      educacion2_num %in% c(4, 5)           ~ "Media",
      educacion2_num %in% c(6, 7, 8, 13)    ~ "Técnica",   # incluye IP y el residuo "sistema antiguo" (13)
      educacion2_num %in% c(9, 10)          ~ "Universitaria",
      educacion2_num %in% c(88, 99, 8888, 9999) ~ NA_character_,
      
      TRUE ~ NA_character_
    ),
    educ_4cat = factor(educ_4cat,
                       levels = c("Básica y sin estudios", "Media", "Técnica", "Universitaria"),
                       ordered = TRUE)
  )

# Verificación: distribución final y que no queden folios sin clasificar
# entre quienes sí tenían respuesta válida en alguna de las dos fuentes
elri %>% count(educ_4cat, useNA = "always")

elri %>%
  filter(!is.na(educacion1_num) | !is.na(educacion2_num)) %>%
  filter(is.na(educ_4cat)) %>%
  nrow()  # debería ser 0 (o muy cercano), si no, algo se escapó del mapeo

elri_panel <- elri %>% filter(folio %in% folios_panel_completo)
stopifnot(n_distinct(elri_panel$folio) == 1589)

elri_panel %>% count(ano)   # tabla para el apéndice

# ============================================================
# 4. Recodificaciones sustantivas — versión consolidada
# ============================================================

elri_panel <- elri_panel %>%
  mutate(
    # -------- Educación --------
    educacion1_num = haven::zap_labels(educacion1),
    educacion1_num = if_else(educacion1_num %in% codigos_na, NA_real_, educacion1_num),
    educacion2_num = haven::zap_labels(educacion2),
    educacion2_num = if_else(educacion2_num %in% codigos_na, NA_real_, educacion2_num),
    
    educ_4cat = case_when(
      educacion1_num %in% c(1, 2, 3)     ~ "Básica y sin estudios",
      educacion1_num %in% c(4, 5)        ~ "Media",
      educacion1_num %in% c(6, 7, 8, 9)  ~ "Técnica",
      educacion1_num %in% c(10, 11, 12)  ~ "Universitaria",
      educacion2_num %in% c(1, 2, 3)     ~ "Básica y sin estudios",
      educacion2_num %in% c(4, 5)        ~ "Media",
      educacion2_num %in% c(6, 7, 8, 13) ~ "Técnica",
      educacion2_num %in% c(9, 10)       ~ "Universitaria",
      TRUE ~ NA_character_
    ),
    educ_4cat = factor(educ_4cat,
                       levels = c("Básica y sin estudios", "Media", "Técnica", "Universitaria"),
                       ordered = TRUE),
    
    # -------- Ingreso (proxy continuo se calcula después de imputar) --------
    ingreso_num = haven::zap_labels(ingreso_h),
    ingreso_num = if_else(ingreso_num %in% codigos_na, NA_real_, ingreso_num),
    
    # -------- Religión --------
    religion_num = haven::zap_labels(religion),
    religion_num = if_else(religion_num %in% codigos_na, NA_real_, religion_num),
    religion_cat = case_when(
      religion_num == 1 ~ "Católica",
      religion_num == 2 ~ "Evangélica/Protestante",
      religion_num %in% c(4, 8, 9, 10) ~ "Ninguna/Atea/Agnóstica",   # 8/9/10 = corrimiento de código en 2023
      religion_num %in% c(3, 5, 7)     ~ "Otra (incl. religión de pueblo originario)",  # 7 = corrimiento de código en 2023
      TRUE ~ NA_character_
    ),
    religion_cat = factor(religion_cat),
    
    # -------- Actividad principal --------
    actprin_num = haven::zap_labels(actprin),
    actprin_num = if_else(actprin_num %in% codigos_na, NA_real_, actprin_num),
    actprin_cat = case_when(
      actprin_num == 1 ~ "Empleado tiempo completo",
      actprin_num %in% c(2, 3) ~ "Empleado parcial/con licencia",
      actprin_num == 4 ~ "Buscando trabajo",
      actprin_num %in% c(5, 8, 9, 10) ~ "Fuera de fuerza laboral",
      actprin_num %in% c(6, 7) ~ "Estudiante",
      TRUE ~ NA_character_
    ),
    actprin_cat = factor(actprin_cat),
    
    # -------- Estado civil --------
    # Códigos 6 ("Anulado/a") y 9 ("Otro") son respuestas válidas,
    # deliberadamente excluidas del esquema de 4 categorías por tamaño
    # muestral marginal — NO son missing, quedan como NA_intencional en _cat
    est_civil_num = haven::zap_labels(est_civil),
    est_civil_num = if_else(est_civil_num %in% codigos_na, NA_real_, est_civil_num),
    est_civil_cat = case_when(
      est_civil_num %in% c(1, 2, 3) ~ "Casado/conviviente",
      est_civil_num == 4 ~ "Soltero/a",
      est_civil_num %in% c(7, 8) ~ "Separado/divorciado",
      est_civil_num == 5 ~ "Viudo/a",
      TRUE ~ NA_character_   # incluye 6 y 9, intencionalmente
    ),
    est_civil_cat = factor(est_civil_cat)
  )

# ============================================================
# Verificación — distingue "missing real sin limpiar" (bug) de
# "categoría válida deliberadamente excluida" (decisión, no error)
# ============================================================

codigos_excluidos_intencional <- list(
  actprin_num   = c(),      # todas las categorías 1-10 tienen mapeo explícito
  est_civil_num = c(6, 9)   # Anulado/a, Otro — deliberadamente fuera del esquema
)

verificar_recodificacion <- function(data, var_num, var_cat, excluidos = c()) {
  data %>%
    filter(!is.na(.data[[var_num]]),
           !(.data[[var_num]] %in% excluidos),
           is.na(.data[[var_cat]])) %>%
    nrow()
}

stopifnot(verificar_recodificacion(elri_panel, "actprin_num", "actprin_cat",
                                   codigos_excluidos_intencional$actprin_num) == 0)
stopifnot(verificar_recodificacion(elri_panel, "est_civil_num", "est_civil_cat",
                                   codigos_excluidos_intencional$est_civil_num) == 0)
stopifnot(verificar_recodificacion(elri_panel, "religion_num", "religion_cat") == 0)

# Educación tiene lógica de dos fuentes (educacion1/educacion2 por período),
# se verifica distinto: cualquiera con AL MENOS UNA fuente no-missing debería
# tener educ_4cat asignado
stopifnot(
  elri_panel %>%
    filter(!is.na(educacion1_num) | !is.na(educacion2_num)) %>%
    filter(is.na(educ_4cat)) %>%
    nrow() == 0
)
