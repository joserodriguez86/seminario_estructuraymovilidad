library(tidyverse)
library(haven)


# Importar base ----------------
base_esa <- read_sav("fuentes/base_esa_pisac.sav")


# Construcción de variables ----------------
base_esa <- base_esa %>%
  mutate(ciuo = CIUO_encuestado,
         sv = car::recode(M3.9, "1:2=1; 3=0; 99=NA"),
         categoria = case_when(M3.5 == 1 ~ 1,
                               M3.5 == 2 & sv == 1 ~ 1,
                               M3.5 == 2 & (sv != 1 | is.na(sv)) ~ 2,
                               M3.5 >= 3 & M3.5 < 99 ~ 3,
                               TRUE ~ NA_real_),
         tamano = case_when(M3.6 <= 2 ~ 1,
                            M3.6 > 2 & M3.6 < 99 ~ 2,
                            TRUE ~ NA_real_),
         rural = case_when(CAES_letra == "A" ~ 1,
                           TRUE ~ 0),
         sv_o = car::recode(M12.22, "1:2=1; 3=0; 99=NA"),
         categoria_o = case_when(M12.20 == 1 ~ 1,
                                 M12.20 == 2 & sv_o < 3 ~ 1,
                                 M12.20 == 2 & (sv_o >= 3 | is.na(sv_o)) ~ 2,
                                 M12.20 >= 3 & M12.20 < 99 ~ 3,
                                 TRUE ~ NA_real_),
         tamano_o = case_when(M12.21 <= 2 ~ 1,
                              M12.21 > 2 & M12.21 < 99 ~ 2,
                              TRUE ~ NA_real_),
         rural_o = case_when(M12.19 == 1 ~ 1,
                             TRUE ~ 0))

# EGP ------------------

##Destino
base_esa <- base_esa %>%
  mutate(egp = case_when(
    #Patrones
    categoria == 1 & tamano == 2 ~ 1,
    categoria == 1 & tamano == 1 & rural != 1 ~ 5,
    categoria == 1 & tamano == 1 & rural == 1 ~ 7,

    #Cuenta propia
    categoria == 2 & ((ciuo >= 1000 & ciuo < 1400)) ~ 1,
    categoria == 2 & (ciuo >= 1400 & ciuo < 2000) ~ 2,

    categoria == 2 & ciuo %in% c(2165, 2221, 2222, 2513:2519, 2320, 2330, 2341, 2342,
                                 2351:2359, 2421:2424, 2621, 2622, 2651:2659, 2636) ~ 2,
    categoria == 2 & (ciuo >= 2000 & ciuo < 3000) ~ 1,

    categoria == 2 & ciuo %in% c(3221, 3222, 3311:3314, 3411:3413) ~ 3,
    categoria == 2 & (ciuo >= 3000 & ciuo < 4000) ~ 2,

    categoria == 2 & (ciuo %in% c(5211, 5212, 5413, 5414) |
                        (ciuo >= 9000 & ciuo < 9999) | (ciuo >= 6300 & ciuo < 7000)) ~ 10,
    categoria == 2 & ((ciuo >= 4000 & ciuo < 6000) | (ciuo >= 7000 & ciuo < 9000)) ~ 6,
    categoria == 2 & (ciuo >= 6000 & ciuo < 6300) ~ 7,

    #Empleados
    categoria == 3 & (ciuo >= 1000 & ciuo < 1400) ~ 1,
    categoria == 3 & (ciuo >= 1400 & ciuo < 2000) ~ 2,

    categoria == 3 & ciuo %in% c(2165, 2221, 2222, 2513:2519, 2320, 2330, 2341, 2342,
                                 2351:2359, 2421:2424, 2621, 2622, 2651:2659, 2636) ~ 2,
    categoria == 3 & (ciuo >= 2000 & ciuo < 3000) ~ 1,

    categoria == 3 & ciuo %in% c(3221, 3222, 3311:3314, 3411:3413) & (sv != 1 | is.na(sv)) ~ 3,
    categoria == 3 & (ciuo >= 3000 & ciuo < 4000) ~ 2,

    categoria == 3 & ciuo %in% c(4412, 4419) & (sv != 1 | is.na(sv)) ~ 4,
    categoria == 3 & (ciuo >= 4000 & ciuo < 5000) & (sv != 1 | is.na(sv)) ~ 3,

    categoria == 3 & ciuo %in% c(5111:5113) & (sv != 1 | is.na(sv)) ~ 3,
    categoria == 3 & ciuo %in% c(5120, 5141, 5142, 5153, 5163, 5164, 5165, 5169) & (sv != 1 | is.na(sv)) ~ 9,
    categoria == 3 & ciuo %in% c(5211, 5212, 5413, 5414, 5419) ~ 10,
    categoria == 3 & ciuo %in% c(5411, 5412) ~ 10,
    categoria == 3 & (ciuo >= 5000 & ciuo < 6000) & (sv != 1 | is.na(sv)) ~ 4,

    categoria == 3 & (ciuo >= 3000 & ciuo < 6000) & sv == 1 ~ 2, #supervisores NM

    categoria == 3 & (ciuo >= 6000 & ciuo < 7000) ~ 11,

    categoria == 3 & (ciuo >= 7000 & ciuo < 9000) & (sv != 1 | is.na(sv)) ~ 9,
    categoria == 3 & (ciuo >= 9000 & ciuo < 9999) & (sv != 1 | is.na(sv)) ~ 10,
    categoria == 3 & (ciuo >= 7000 & ciuo < 9999) & sv == 1 ~ 8, #supervisores M
    ciuo == 110 ~ 2,
    ciuo == 210 | ciuo == 310 ~ 8,
    TRUE ~ NA_real_
  ))

base_esa$egp_f <- factor(base_esa$egp, labels = c("I", "II", "IIIa", "IIIb",
                                                  "IVa", "IVb", "IVc", "V",
                                                  "VI", "VIIa", "VIIb"))

base_esa$egp5 <- car::recode(base_esa$egp, "1:2=1; 3:4=2; 5:7=3; 8:9=4; 10:11=5")
base_esa$egp5_f <- factor(base_esa$egp5, labels = c("I+II", "III", "IV", "V+VI", "VII"))


##Origen
base_esa <- base_esa %>%
  mutate(egp_origen = case_when(
    #Patrones
    categoria_o == 1 & tamano_o == 2 ~ 1,
    categoria_o == 1 & tamano_o == 1 & rural_o != 1 ~ 5,
    categoria_o == 1 & tamano_o == 1 & rural_o == 1 ~ 7,

    #Cuenta propia
    categoria_o == 2 & ((CIUO_origen >= 1000 & CIUO_origen < 1400)) ~ 1,
    categoria_o == 2 & (CIUO_origen >= 1400 & CIUO_origen < 2000) ~ 2,

    categoria_o == 2 & CIUO_origen %in% c(2165, 2221, 2222, 2513:2519, 2320, 2330, 2341, 2342,
                                          2351:2359, 2421:2424, 2621, 2622, 2651:2659, 2636) ~ 2,
    categoria_o == 2 & (CIUO_origen >= 2000 & CIUO_origen < 3000) ~ 1,

    categoria_o == 2 & CIUO_origen %in% c(3221, 3222, 3311:3314, 3411:3413) ~ 3,
    categoria_o == 2 & (CIUO_origen >= 3000 & CIUO_origen < 4000) ~ 2,

    categoria_o == 2 & (CIUO_origen %in% c(5211, 5212, 5413, 5414) |
                          (CIUO_origen >= 9000 & CIUO_origen < 9999) | (CIUO_origen >= 6300 & CIUO_origen < 7000)) ~ 10,
    categoria_o == 2 & ((CIUO_origen >= 4000 & CIUO_origen < 6000) | (CIUO_origen >= 7000 & CIUO_origen < 9000)) ~ 6,
    categoria_o == 2 & (CIUO_origen >= 6000 & CIUO_origen < 6300) ~ 7,

    #Empleados
    categoria_o == 3 & (CIUO_origen >= 1000 & CIUO_origen < 1400) ~ 1,
    categoria_o == 3 & (CIUO_origen >= 1400 & CIUO_origen < 2000) ~ 2,

    categoria_o == 3 & CIUO_origen %in% c(2165, 2221, 2222, 2513:2519, 2320, 2330, 2341, 2342,
                                          2351:2359, 2421:2424, 2621, 2622, 2651:2659, 2636) ~ 2,
    categoria_o == 3 & (CIUO_origen >= 2000 & CIUO_origen < 3000) ~ 1,

    categoria_o == 3 & CIUO_origen %in% c(3221, 3222, 3311:3314, 3411:3413) & (sv_o != 1 | is.na(sv_o)) ~ 3,
    categoria_o == 3 & (CIUO_origen >= 3000 & CIUO_origen < 4000) ~ 2,

    categoria_o == 3 & CIUO_origen %in% c(4412, 4419) & (sv_o != 1 | is.na(sv_o)) ~ 4,
    categoria_o == 3 & (CIUO_origen >= 4000 & CIUO_origen < 5000) & (sv_o != 1 | is.na(sv_o)) ~ 3,

    categoria_o == 3 & CIUO_origen %in% c(5111:5113) & (sv_o != 1 | is.na(sv_o)) ~ 3,
    categoria_o == 3 & CIUO_origen %in% c(5120, 5141, 5142, 5153, 5163, 5164, 5165, 5169) & (sv_o != 1 | is.na(sv_o)) ~ 9,
    categoria_o == 3 & CIUO_origen %in% c(5211, 5212, 5413, 5414, 5419) ~ 10,
    categoria_o == 3 & CIUO_origen %in% c(5411, 5412) ~ 10,
    categoria_o == 3 & (CIUO_origen >= 5000 & CIUO_origen < 6000) & (sv_o != 1 | is.na(sv_o)) ~ 4,

    categoria_o == 3 & (CIUO_origen >= 3000 & CIUO_origen < 6000) & sv_o == 1 ~ 2, #supervisores NM

    categoria_o == 3 & (CIUO_origen >= 6000 & CIUO_origen < 7000) ~ 11,

    categoria_o == 3 & (CIUO_origen >= 7000 & CIUO_origen < 9000) & (sv_o != 1 | is.na(sv_o)) ~ 9,
    categoria_o == 3 & (CIUO_origen >= 9000 & CIUO_origen < 9999) & (sv_o != 1 | is.na(sv_o)) ~ 10,
    categoria_o == 3 & (CIUO_origen >= 7000 & CIUO_origen < 9999) & sv_o == 1 ~ 8, #supervisores M
    CIUO_origen == 110 ~ 2,
    CIUO_origen == 210 | CIUO_origen == 310 ~ 8,
    TRUE ~ NA_real_
  ))

base_esa$egp_origen_f <- factor(base_esa$egp, labels = c("I", "II", "IIIa", "IIIb",
                                                         "IVa", "IVb", "IVc", "V",
                                                         "VI", "VIIa", "VIIb"))

base_esa$egp5_origen <- car::recode(base_esa$egp_origen, "1:2=1; 3:4=2; 5:7=3; 8:9=4; 10:11=5")
base_esa$egp5_origen_f <- factor(base_esa$egp5_origen,
                                 labels = c("I+II", "III", "IV", "V+VI", "VII"))
