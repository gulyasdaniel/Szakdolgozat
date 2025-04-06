# A synthpop 1.9-0 bugos és nem kompatibilis az Ipfp package-el
# Korábbi verziók erről a linkről tölthetők le: 
# https://cran.r-project.org/src/contrib/Archive/synthpop/ 

# Az elemzés az 1.8-0 verzióval történt

# Adjuk meg a letöltött file pontos nevét és elérési útját, az alábbi példa szerint:
# install.packages("C:/Users/daniel.gulyas_intren/Dokumentumok/synthpop_1.8-0.tar.gz", 
# repos = NULL, type = "source")

# Ha lefutott a telepítés már nincs is más tennivalónk.

# További szükséges csomagok betöltése
library(synthpop)
library(haven)
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)
library(gridExtra)
library(ggplot2)

# Ez segít abban, hogy az R helyesen kezelje az ékezetes karaktereket
Sys.setlocale("LC_CTYPE", "hu_HU.UTF-8")

# Adatbázis betöltése (a haven package automatikusan átkódolja a hiányzó értékeket)
ESS <- read_spss("C:/Users/daniel.gulyas_intren/Desktop/Szakdoga/ESS11.sav", user_na = TRUE)

# Magyar megfigyelések kiválasztása
ESS_hun <- ESS[ESS$cntry == "HU", ]

# Tetszőleges változók kiválasztása
an_vars_raw <- ESS_hun[, c("anweight", "gndr", "agea", "edlvdahu", "nwspol", "ppltrst", 
                           "polintr", "prtvthhu", "stfgov", "height", "weighta",
                           "hltprhc", "hltprhb", "hltprdi")]

an_vars <- an_vars_raw[, c("gndr", "agea", "edlvdahu", "nwspol", "ppltrst", 
                           "polintr", "prtvthhu", "stfgov", "height", "weighta",
                           "hltprhc", "hltprhb", "hltprdi")]


########################### Változók kategorizálása ############################

an_vars$Nem <- as_factor(an_vars$gndr,
                         levels = "default",
                         ordered = FALSE)

an_vars$Kor <- cut(an_vars$agea,
                   breaks = c(15, 30, 45, 60, 75, 90),  # Határok beállítása
                   labels = c("15-29", "30-44", "45-59", "60-74", "75-90"), # Címkék beállítása
                   include.lowest = TRUE,
                   right = TRUE)  # Jobb oldali határértékeket beleértve

an_vars$Isk.végz <- as_factor(an_vars$edlvdahu, 
                              levels = "default",
                              ordered = T)

# nwspol változó értékeinek 5 kategóriába sorolása a kvantilisek alapján
an_vars$Pol.hír.perc <- cut(an_vars$nwspol, 
                            breaks = c(0, 10, 30, 100, 500, 1165), 
                            labels = c("Nagyon alacsony", "Alacsony", "Közepes", 
                                       "Magas", "Nagyon magas"),
                            include.lowest = TRUE)

an_vars$Szavazat <- as_factor(an_vars$prtvthhu,
                              levels = "default",
                              ordered = F) 

an_vars$Pol.érd <- as_factor(an_vars$polintr,
                             levels = "default",
                             ordered = T)

an_vars$Pol.érd <- factor(
  an_vars$Pol.érd,
  levels = c("Not at all interested", 
             "Hardly interested", 
             "Quite interested", 
             "Very interested", 
             "Refusal", 
             "Don't know", 
             "No answer"),
  ordered = TRUE
)


an_vars$Elégedettség.korm <- as_factor(an_vars$stfgov,
                                       levels = "default",
                                       ordered = T)

an_vars$Bizalom <- as_factor(an_vars$ppltrst,
                             levels = "default",
                             ordered = T)

# Szív vagy keringéssel kapcsolatos probléma az elmúlt 1 évben
an_vars$Szív.prob <- as_factor(an_vars$hltprhc,
                               levels = "default",
                               ordered = F)

# Magas vérnyomással kapcsolatos probléma az elmúlt 1 évben
an_vars$Vérnyomás.prob <- as_factor(an_vars$hltprhb,
                                    levels = "default",
                                    ordered = F)

# Diabetes-szel kapcsolatos probléma az elmúlt 1 évben
an_vars$Diabetes.prob <- as_factor(an_vars$hltprdi,
                                   levels = "default",
                                   ordered = F)

# BMI kiszámítása minden egyes válaszadóra (ha vmelyik változó értéke NA akkor ennek is)
an_vars$BMI <- ifelse(is.na(an_vars$weighta) | is.na(an_vars$height), NA, 
                      an_vars$weighta / (an_vars$height / 100)^2)

an_vars$BMI <- cut(an_vars$BMI, 
                   breaks = c(0, 18.5, 24.99, 29.9, 34.9, Inf), 
                   labels = c("Sovány", "Normál", "Túlsúlyos", "Elhízott", 
                              "Súlyosan elhízott"), 
                   include.lowest = TRUE)

# Testmagasság kategorizálása
an_vars$Magasság <- cut(an_vars$height, 
                        breaks = c(0, 155, 165, 175, 185, Inf), 
                        labels = c("-155 cm", "156 - 165 cm", "166 - 175 cm", 
                                   "176 - 185 cm", "186 cm +"), 
                        include.lowest = TRUE,
                        ordered_result = TRUE)

# Testsúly kategorizálása
an_vars$Súly <- cut(an_vars$weighta, 
                    breaks = c(0, 50, 65, 80, 95, Inf), 
                    labels = c("Nagyon sovány", "Sovány", "Átlagos", "Túlsúlyos", 
                               "Elhízott"), 
                    include.lowest = TRUE,
                    ordered_result = TRUE)

########################### Változószettek kiválasztása ########################

datasets <- list(
  an_1 = an_vars[, c("Nem", "Kor", "Isk.végz", "Pol.hír.perc", "Pol.érd", "Elégedettség.korm")],
  an_2 = an_vars[, c("Nem", "Kor", "Isk.végz", "Pol.hír.perc", "Bizalom", "Pol.érd")],
  an_3 = an_vars[, c("Nem", "Kor", "Isk.végz", "Pol.hír.perc", "Szavazat", "Elégedettség.korm")],
  an_4 = an_vars[, c("Nem", "Kor", "Magasság", "Súly", "Szív.prob", "Vérnyomás.prob", "Diabetes.prob")],
  an_5 = an_vars[, c("Nem", "Kor", "Magasság", "Súly", "BMI", "Szív.prob", "Vérnyomás.prob", "Diabetes.prob")]
) 

############################### SZINTETIZÁLÁS ##################################

# Alap seed beállítása a teljes reprodukálhatósághoz
set.seed(42)

# Különböző epsilon értékek listája
epsilon_values <- c(0.5, 1, 2, 10)

# Ötszöri szintetizálás függvénye minden epsilon értékhez
if (!exists("synth_results_ipf")) {
  synth_results_ipf <- list()
}

synthesize_data_ipf <- function(data, epsilon, seed_offset) {
  set.seed(42 + seed_offset) # Minden epsilonhoz különböző seed
  replicate(5, {
    syn_args <- list(
      data = data,
      method = "ipf",
      ipf.priorn = 1, 
      ipf.gmargins = "twoway", 
      ipf.othmargins = NULL, 
      ipf.epsilon = epsilon, 
      ipf.max.its = 5000
    )
    do.call(syn, syn_args)
  }, simplify = FALSE)
}

# Függvény alkalmazása a definiált adatszettekre különböző epsilon értékekkel
synth_results_ipf <- lapply(seq_along(epsilon_values), function(i) {
  lapply(datasets, function(data) {
    synthesize_data_ipf(data, epsilon_values[i], i)
  })
})

# Szintetikus adatbázisok kinyerése (synds -> dataframe)
synth_data_ipf <- lapply(synth_results_ipf, function(dataset_list) {
  lapply(dataset_list, function(syn_list) {
    lapply(syn_list, function(syn_obj) syn_obj$syn)
  })
})
################################# RU_SCORE #####################################

table_ru_avg <- function(syn_list, orig_data, exclude = NULL) {
  print("RU számolás elindult...")  # Debugging
  
  # RU értékek tárolása
  rep_values <- numeric(5)
  per_rep <- numeric(5)
  un <- numeric(5)
  per_un <-  numeric(5)
  ru_p1_values <- numeric(5)
  
  for (i in seq_along(syn_list)) {
    syn_obj <- syn_list[[i]]
    
    ru_scores <- replicated.uniques(syn_obj, orig_data, exclude)
    
    if (!is.null(ru_scores) && all(c("no.replications", "per.replications") %in% names(ru_scores))) {
      rep_values[i] <- mean(ru_scores$no.replications, na.rm = TRUE)
      per_rep[i] <- mean(ru_scores$per.replications, na.rm = TRUE)
      un[i] <- ru_scores$no.uniques 
      per_un[i] <- (ru_scores$no.uniques / nrow(orig_data)) * 100
      ru_p1_values[i] <- mean((per_rep[i] / per_un[i]) * 100, na.rm = TRUE)
    } else {
      print("HIBA: RU értékek hiányoznak!")  # Debug
      ru_values[i] <- NA
      per_rep[i] <- NA
      un[i] <- NA
      per_un[i] <- NA
      ru_p1_values[i] <- NA
    }
  }
  
  # RU átlagok kiszámítása az 5 elemre
  rep_avg <- mean(rep_values, na.rm = TRUE)
  per_rep_avg <- mean(per_rep, na.rm = TRUE)
  un_avg <- mean(un)
  per_un_avg <- mean(per_un, na.rm = TRUE) # egyedi megfigyelések %-a az eredeti adatbázisban
  ru_as_p1_avg <- mean(ru_p1_values, na.rm = TRUE)
  # CSAK AZ EGYEDI MEGFIGYELÉSEKET NÉZI!!!
  print(paste("No.replications átlag:", rep_avg,
              "% of replications átlag:", per_rep_avg,
              "ru as a % of p1", ru_as_p1_avg
  ))  # Debugging
  
  return(list(rep_avg = rep_avg, per_rep_avg = per_rep_avg, un = un_avg, per_un = per_un_avg,
              ru_as_p1_avg = ru_as_p1_avg))
}


ru_results_ipf <- setNames(
  lapply(seq_along(synth_results_ipf), function(i) {  # Végigmegyünk minden futtatáson
    print(paste("--Futtatás:", i))  # Debugging
    
    setNames(
      lapply(names(synth_results_ipf[[i]]), function(name) {  # Minden datasetre külön lefut
        print(paste("Feldolgozás alatt:", name, "Futtatás:", i))  # Debugging
        table_ru_avg(synth_results_ipf[[i]][[name]], datasets[[name]])  # Most már a teljes listát adjuk át
      }),
      names(synth_results_ipf[[i]])
    )
  }),
  paste0("Run_", seq_along(synth_results_ipf))  # Az egyes futtatásokat névvel látjuk el
)

######################## MEAN TWOWAY/THREEWAY UTILITY ##########################
# Ez az összes lehetséges változópár ("two-way") S_pMSE értékének átlaga 
# (az 5 futtatásra nézve)

table_utility_synthpop <- function(syn_list, orig_data) {
  print("Utility számolás elindult...")  # Debugging
  
  # Ellenőrizzük, hogy a lista nem üres és 5 elemet tartalmaz-e
  if (is.null(syn_list) || length(syn_list) != 5) {
    print("HIBA: A bemenet nem megfelelő méretű lista!")  # Debug
    return(NA)
  }
  
  # Utility értékek tárolása
  twoway_util_values <- numeric(5)
  threeway_util_values <- numeric(5)
  worst_values <- numeric(5)
  
  for (i in seq_along(syn_list)) {
    syn_df <- syn_list[[i]]  # Egy adott szintetikus adatbázis dataframe
    
    if (!inherits(syn_df, "data.frame")) {
      print(paste("HIBA: A(z)", i, ". elem nem data.frame!"))  
      twoway_util_values[i] <- NA
      threeway_util_values[i] <- NA
      worst_values[i] <- NA
      next
    }
    
    print(paste("Érvényes data.frame - utility.tables() hívása:", class(syn_df))) 
    twoway_util <- utility.tables(syn_df, orig_data, tables = "twoway", plot = FALSE, useNA = FALSE)$tabs[,2]
    threeway_util <- utility.tables(syn_df, orig_data, tables = "threeway", plot = FALSE, useNA = FALSE)$tabs[,2]
    worst_util <- utility.tables(syn_df, orig_data, tables = "threeway", plot = FALSE, useNA = FALSE)$worstn[1]
    
    if (!is.null(twoway_util)) {
      twoway_util_values[i] <- mean(twoway_util, na.rm = FALSE)
    } else {
      print("HIBA: Utility értékek hiányoznak!")
      twoway_util_values[i] <- NA
    }
    
    if (!is.null(threeway_util)) {
      threeway_util_values[i] <- mean(threeway_util, na.rm = FALSE)
    } else {
      print("HIBA: Threeway Utility értékek hiányoznak!")
      threeway_util_values[i] <- NA
    }
    
    if (!is.null(worst_util)) {
      worst_values[i] <- mean(worst_util, na.rm = FALSE)
    } else {
      print("HIBA: Worst threeway Utility értékek hiányoznak!")
      worst_values[i] <- NA
    }
  }
  
  # Átlagos utility kiszámítása az 5 adatbázis alapján
  mean_two_way <- mean(twoway_util_values, na.rm = FALSE)
  mean_three_way <- mean(threeway_util_values, na.rm = FALSE)
  mean_worstn <- mean(worst_values, na.rm = FALSE)
  print(paste("Átlagos kétváltozós utility:", mean_two_way))  # Debugging
  print(paste("Átlagos háromváltozós utility:", mean_three_way))
  print(paste("Legrosszabb háromváltozós utility:", mean_worstn))
  
  return(list(mean_two_way = mean_two_way, mean_three_way = mean_three_way, mean_worstn = mean_worstn))
}


utility_results_ipf <- setNames(
  lapply(seq_along(synth_data_ipf), function(i) {  # Végigmegyünk minden futtatáson
    print(paste("Futtatás:", i))  # Debugging
    
    setNames(
      lapply(names(synth_data_ipf[[i]]), function(name) {  # Minden datasetre külön lefut
        print(paste("Feldolgozás alatt:", name, "Futtatás:", i))  # Debugging
        table_utility_synthpop(synth_data_ipf[[i]][[name]], datasets[[name]])  # Most már a teljes listát adjuk át
      }),
      names(synth_data_ipf[[i]])
    )
  }),
  paste0("Run_", seq_along(synth_data_ipf))  # Az egyes futtatásokat névvel látjuk el
)

############################# UTILITY.GEN ###################################### 
# Propensity score-alapú módszert használ, vagyis azt becsli
# (CART klasszifikációs modell), hogy egy adott megfigyelés az eredeti vagy a 
# szintetikus adatbázisból származik-e.

# Funkció a utility értékek számítására és átlagolására
table_utility_avg <- function(syn_df, orig_data) {
  print(paste("Utility számolás elindult a dataseten:", class(syn_df)))  # Debugging
  
  orig_data <- as.data.frame(orig_data)  # Konvertálás biztosítása
  
  # Ellenőrizzük, hogy valóban synds objektumot kaptunk
  if (!inherits(syn_df, "data.frame")) {
    print("HIBA: A(z) elem nem data.frame!")  
    return(list(pMSE = NA, S_pMSE = NA))
  }
  
  print("Érvényes data.frame, utility.gen() hívása...")  # Debug
  utility_scores <- utility.gen(syn_df, orig_data)
  
  # Ha az utility.gen() NULL-t adott vissza
  if (is.null(utility_scores) || length(utility_scores) == 0) {
    print("HIBA: utility.gen() NULL-t vagy üres listát adott vissza!")  # Debug
    return(list(pMSE = NA, S_pMSE = NA))
  }
  
  # Utility értékek kinyerése
  pMSE <- if ("pMSE" %in% names(utility_scores)) utility_scores$pMSE else NA
  S_pMSE <- if ("S_pMSE" %in% names(utility_scores)) utility_scores$S_pMSE else NA
  
  print(paste("pMSE:", pMSE, "S_pMSE:", S_pMSE))  # Debugging
  
  return(list(pMSE = pMSE, S_pMSE = S_pMSE))
}

# Utility értékek kiszámítása minden szintetizált adatcsoportra
utility_gen_results_ipf <- setNames(
  lapply(seq_along(synth_data_ipf), function(i) {  # Végigmegyünk minden futtatáson
    print(paste("Futtatás:", i))  # Debugging
    
    setNames(
      lapply(names(synth_data_ipf[[i]]), function(name) {  # Minden datasetre külön lefut
        print(paste("Feldolgozás alatt:", name, "Futtatás:", i))  # Debugging
        lapply(synth_data_ipf[[i]][[name]], function(df) {
          table_utility_avg(df, datasets[[name]])
        })
      }),
      names(synth_data_ipf[[i]])
    )
  }),
  paste0("Run_", seq_along(synth_data_ipf))  # Az egyes futtatásokat névvel látjuk el
)

# Utility.gen értékek átlagolása az összes futtatásra és datasetre
utility_gen_results_avg_ipf <- setNames(
  lapply(names(utility_gen_results_ipf), function(run_name) {  # Minden Run-ra külön számítás
    setNames(
      lapply(names(synth_data_ipf[[1]]), function(name) {  # Minden datasetre külön
        all_pMSE <- unlist(lapply(utility_gen_results_ipf[[run_name]][[name]], `[[`, "pMSE"))
        all_S_pMSE <- unlist(lapply(utility_gen_results_ipf[[run_name]][[name]], `[[`, "S_pMSE"))
        
        list(
          pMSE_avg = mean(all_pMSE, na.rm = TRUE),
          S_pMSE_avg = mean(all_S_pMSE, na.rm = TRUE)
        )
      }),
      names(synth_data_ipf[[1]])
    )
  }),
  names(utility_gen_results_ipf)
)

# Az átlagolt utility eredmények kiírása
print(utility_gen_results_avg_ipf)

# assocstats(table(ESS_syn_data$gndr, ESS_syn_data$height))

################################# TÁBLÁZAT IPF #################################
# Táblázatkészítő függvény:

# Az epsilon értékek automatikus kinyerése a Run_ csoportokból
epsilon_values_ipf <- c(0.5, 1, 2, 10)

# Az adatszettek nevei
df_names_ipf <- names(ru_results_ipf[[1]])

# Lista létrehozása
final_table_list_ipf <- list()

# Minden epsilonhoz külön blokkot hozunk létre
for (i in seq_along(epsilon_values_ipf)) {
  eps <- epsilon_values_ipf[i]
  
  # Az aktuális epsilonhoz tartozó értékek kinyerése minden datasetre
  rep_avg <- sapply(df_names_ipf, function(ds) ru_results_ipf[[i]][[ds]]$rep_avg)
  per_rep <- sapply(df_names_ipf, function(ds) ru_results_ipf[[i]][[ds]]$per_rep_avg)
  ru_as_p1 <- sapply(df_names_ipf, function(ds) ru_results_ipf[[i]][[ds]]$ru_as_p1_avg)
  mean_twoway_utility <- sapply(df_names_ipf, function(ds) utility_results_ipf[[i]][[ds]]$mean_two_way)
  mean_threeway_utility <- sapply(df_names_ipf, function(ds) utility_results_ipf[[i]][[ds]]$mean_three_way)
  worst_threeway_utility <- sapply(df_names_ipf, function(ds) utility_results_ipf[[i]][[ds]]$mean_worstn)
  utility_pmse <- sapply(df_names_ipf, function(ds) utility_gen_results_avg_ipf[[i]][[ds]]$pMSE_avg)
  utility_spmse <- sapply(df_names_ipf, function(ds) utility_gen_results_avg_ipf[[i]][[ds]]$S_pMSE_avg)
  
  # Táblázati blokk létrehozása helyes struktúrával
  table_block <- rbind(
    c("$\\epsilon$", eps, rep("", length(df_names_ipf))),  # Epsilon sor
    c("Replikációk", "", rep_avg),  # RU százalék sor (üres epsilon cellával)
    c("Replikációk \\%", "", round(per_rep, digits = 2)),
    c("RU as \\% of p1", "", round(ru_as_p1, digits = 2)),  # RU as % of p1 sor (üres epsilon cellával)
    c("Kétdimenziós felh.", "", round(mean_twoway_utility, digits = 2)),
    c("Háromdimenziós felh.", "", round(mean_threeway_utility, digits = 2)),
    c("Legrosszabb háromdimenziós felh.", "", round(worst_threeway_utility, digits = 2)),
    c("Utility.gen pMSE", "", round(utility_pmse, digits = 3)),
    c("Utility.gen S_pMSE", "", round(utility_spmse, digits = 2))
  )
  
  # Oszlopnevek beállítása
  colnames(table_block) <- c("Mérőszám", "$\\epsilon$", df_names_ipf)
  
  # Az adott epsilonhoz tartozó blokkot hozzáadjuk a listához
  final_table_list_ipf[[i]] <- as.data.frame(table_block, stringsAsFactors = FALSE)
}

# Az összes táblázati blokk összefűzése
final_table_ipf <- do.call(rbind, final_table_list_ipf)

# LaTeX táblázat generálása helyes formázással
latex_output <- kable(final_table_ipf, format = "latex", booktabs = TRUE, escape = FALSE, align = "l") %>%
  kable_styling(latex_options = c("hold_position"))  %>%
  column_spec(1, width = "4cm") %>%  # Metric oszlop balra igazítva
  column_spec(2, width = "1.5cm")

# LaTeX táblázat kiírása .txt fájlba (Overleaf kompatibilis)
header <- "\\documentclass{article}
\\usepackage[table]{xcolor}
\\usepackage{booktabs}
\\usepackage{colortbl}
\\definecolor{mygray}{gray}{0.9}
\\rowcolors{2}{mygray!30}{white}
\\begin{document}"

footer <- "\\end{document}"

writeLines(enc2utf8(c(header, latex_output, footer)), "table_ipf_DP.tex", useBytes = TRUE)

# Opcionális gyors grafikus mentés
# pdf("table.pdf", height = 5, width = 10)
# grid.table(final_table)
# dev.off()

################################# CATALL DP ####################################
# Ötszöri szintetizálás függvénye minden epsilon értékhez
if (!exists("synth_results_catall")) {
  synth_results_catall <- list()
}

synthesize_data_catall <- function(data, epsilon, seed_offset) {
  set.seed(42 + seed_offset) # Minden epsilonhoz különböző seed
  replicate(5, {
    syn_args <- list(
      data = data,
      method = "catall",
      catall.priorn = 1, 
      catall.gmargins = "twoway", 
      catall.othmargins = NULL, 
      catall.epsilon = epsilon, 
      catall.max.its = 5000
    )
    do.call(syn, syn_args)
  }, simplify = FALSE)
}

# Függvény alkalmazása a definiált adatszettekre különböző epsilon értékekkel
synth_results_catall <- lapply(seq_along(epsilon_values), function(i) {
  lapply(datasets, function(data) {
    synthesize_data_catall(data, epsilon_values[i], i)
  })
})

# Szintetikus adatbázisok kinyerése (synds -> dataframe)
synth_data_catall <- lapply(synth_results_catall, function(dataset_list) {
  lapply(dataset_list, function(syn_list) {
    lapply(syn_list, function(syn_obj) syn_obj$syn)
  })
})

################################# RU_SCORE #####################################

ru_results_catall <- setNames(
  lapply(seq_along(synth_results_catall), function(i) {  # Végigmegyünk minden futtatáson
    print(paste("--Futtatás:", i))  # Debugging
    
    setNames(
      lapply(names(synth_results_catall[[i]]), function(name) {  # Minden datasetre külön lefut
        print(paste("Feldolgozás alatt:", name, "Futtatás:", i))  # Debugging
        table_ru_avg(synth_results_catall[[i]][[name]], datasets[[name]]) 
      }),
      names(synth_results_catall[[i]])
    )
  }),
  paste0("Run_", seq_along(synth_results_catall))  # Az egyes futtatásokat névvel látjuk el
)

############################# MEAN TWOWAY UTILITY ##############################

utility_results_catall <- setNames(
  lapply(seq_along(synth_data_catall), function(i) {  # Végigmegyünk minden futtatáson
    print(paste("Futtatás:", i))  # Debugging
    
    setNames(
      lapply(names(synth_data_catall[[i]]), function(name) {  # Minden datasetre külön lefut
        print(paste("Feldolgozás alatt:", name, "Futtatás:", i))  # Debugging
        table_utility_synthpop(synth_data_catall[[i]][[name]], datasets[[name]])  # Most már a teljes listát adjuk át
      }),
      names(synth_data_catall[[i]])
    )
  }),
  paste0("Run_", seq_along(synth_data_catall))  # Az egyes futtatásokat névvel látjuk el
)

############################# UTILITY.GEN ###################################### 
# Propensity score-alapú módszert használ, vagyis azt becsli
# (CART klasszifikációs modell), hogy egy adott megfigyelés az eredeti vagy a 
# szintetikus adatbázisból származik-e.

# Utility értékek kiszámítása minden szintetizált adatcsoportra
utility_gen_results_catall <- setNames(
  lapply(seq_along(synth_data_catall), function(i) {  # Végigmegyünk minden futtatáson
    print(paste("Futtatás:", i))  # Debugging
    
    setNames(
      lapply(names(synth_data_catall[[i]]), function(name) {  # Minden datasetre külön lefut
        print(paste("Feldolgozás alatt:", name, "Futtatás:", i))  # Debugging
        lapply(synth_data_catall[[i]][[name]], function(df) {
          table_utility_avg(df, datasets[[name]])
        })
      }),
      names(synth_data_catall[[i]])
    )
  }),
  paste0("Run_", seq_along(synth_data_catall))  # Az egyes futtatásokat névvel látjuk el
)

# Utility.gen értékek átlagolása az összes futtatásra és datasetre
utility_gen_results_avg_catall <- setNames(
  lapply(names(utility_gen_results_catall), function(run_name) {  # Minden Run-ra külön számítás
    setNames(
      lapply(names(synth_data_catall[[1]]), function(name) {  # Minden datasetre külön
        all_pMSE <- unlist(lapply(utility_gen_results_catall[[run_name]][[name]], `[[`, "pMSE"))
        all_S_pMSE <- unlist(lapply(utility_gen_results_catall[[run_name]][[name]], `[[`, "S_pMSE"))
        
        list(
          pMSE_avg = mean(all_pMSE, na.rm = TRUE),
          S_pMSE_avg = mean(all_S_pMSE, na.rm = TRUE)
        )
      }),
      names(synth_data_catall[[1]])
    )
  }),
  names(utility_gen_results_catall)
)

# Az átlagolt utility eredmények kiírása
print(utility_gen_results_avg_catall)

############################ TÁBLÁZAT CATALL ###################################
# Táblázatkészítő függvény:

# Az epsilon értékek automatikus kinyerése a Run_ csoportokból
epsilon_values_catall <- c(0.5, 1, 2, 10)

# Az adatszettek nevei
df_names_catall <- names(ru_results_catall[[1]])

# Lista létrehozása
final_table_list_catall <- list()

# Minden epsilonhoz külön blokkot hozunk létre
for (i in seq_along(epsilon_values_catall)) {
  eps <- epsilon_values_catall[i]
  
  # Az aktuális epsilonhoz tartozó értékek kinyerése minden datasetre
  rep_avg <- sapply(df_names_catall, function(ds) ru_results_catall[[i]][[ds]]$rep_avg)
  per_rep <- sapply(df_names_catall, function(ds) ru_results_catall[[i]][[ds]]$per_rep_avg)
  ru_as_p1 <- sapply(df_names_catall, function(ds) ru_results_catall[[i]][[ds]]$ru_as_p1_avg)
  mean_twoway_utility <- sapply(df_names_ipf, function(ds) utility_results_catall[[i]][[ds]]$mean_two_way)
  mean_threeway_utility <- sapply(df_names_ipf, function(ds) utility_results_catall[[i]][[ds]]$mean_three_way)
  worst_threeway_utility <- sapply(df_names_ipf, function(ds) utility_results_catall[[i]][[ds]]$mean_worstn)
  utility_pmse <- sapply(df_names_catall, function(ds) utility_gen_results_avg_catall[[i]][[ds]]$pMSE_avg)
  utility_spmse <- sapply(df_names_catall, function(ds) utility_gen_results_avg_catall[[i]][[ds]]$S_pMSE_avg)
  
  # Táblázati blokk létrehozása helyes struktúrával
  table_block <- rbind(
    c("$\\epsilon$", eps, rep("", length(df_names_catall))),  # Epsilon sor
    c("Replikációk", "", rep_avg),  # RU százalék sor (üres epsilon cellával)
    c("Replikációk \\%", "", round(per_rep, digits = 2)),
    c("RU as \\% of p1", "", round(ru_as_p1, digits = 2)),  # RU as % of p1 sor (üres epsilon cellával)
    c("Kétdimenziós felh.", "", round(mean_twoway_utility, digits = 2)),
    c("Háromdimenziós felh.", "", round(mean_threeway_utility, digits = 2)),
    c("Legrosszabb háromdimenziós felh.", "", round(worst_threeway_utility, digits = 2)),
    c("Utility.gen pMSE", "", round(utility_pmse, digits = 3)),
    c("Utility.gen S_pMSE", "", round(utility_spmse, digits = 2))
  )
  
  # Oszlopnevek beállítása
  colnames(table_block) <- c("Metric", "$\\epsilon$", df_names_catall)
  
  # Az adott epsilonhoz tartozó blokkot hozzáadjuk a listához
  final_table_list_catall[[i]] <- as.data.frame(table_block, stringsAsFactors = FALSE)
}

# Az összes táblázati blokk összefűzése
final_table_catall <- do.call(rbind, final_table_list_catall)

# LaTeX táblázat generálása helyes formázással
latex_output <- kable(final_table_catall, format = "latex", booktabs = TRUE, escape = FALSE, align = "l") %>%
  kable_styling(latex_options = c("hold_position"))  %>%
  column_spec(1, width = "4cm") %>%  # Metric oszlop balra igazítva
  column_spec(2, width = "1.5cm")

# LaTeX táblázat kiírása .txt fájlba (Overleaf kompatibilis)
header <- "\\documentclass{article}
\\usepackage[table]{xcolor}
\\usepackage{booktabs}
\\usepackage{colortbl}
\\definecolor{mygray}{gray}{0.9}
\\rowcolors{2}{mygray!30}{white}
\\begin{document}"

footer <- "\\end{document}"

writeLines(enc2utf8(c(header, latex_output, footer)), "table_catall_DP.tex", useBytes = TRUE)

################################# STANDARD SYNTH ###############################
# CATALL
# Ötszöri szintetizálás függvénye
if (!exists("synth_results_cat_st")) {
  synth_results_cat_st <- list()
}

synthesize_data_cat_st <- function(data) {
  replicate(5, syn(data, method = "catall", catall.priorn = 1, catall.gmargins = "twoway", 
                   catall.othmargins = NULL), simplify = FALSE)
}

# Függvény alkalmazása a definiált adatszettekre
synth_results_cat_st <- append(synth_results_cat_st, list(lapply(datasets, synthesize_data_cat_st)))

# Szintetikus adatbázisok kinyerése (synds -> dataframe)
synth_data_cat_st <- lapply(synth_results_cat_st, function(dataset_list) {
  lapply(dataset_list, function(syn_list) {
    lapply(syn_list, function(syn_obj) syn_obj$syn)
  })
}) # [[1]],[[2]]...stb sorrendben lesznek a különböző epsilonnal generált futtatások

################################# RU_SCORE #####################################

ru_results_cat_st <- setNames(
  lapply(seq_along(synth_results_cat_st), function(i) {  # Végigmegyünk minden futtatáson
    print(paste("--Futtatás:", i))  # Debugging
    
    setNames(
      lapply(names(synth_results_cat_st[[i]]), function(name) {  # Minden datasetre külön lefut
        print(paste("Feldolgozás alatt:", name, "Futtatás:", i))  # Debugging
        table_ru_avg(synth_results_cat_st[[i]][[name]], datasets[[name]]) 
      }),
      names(synth_results_cat_st[[i]])
    )
  }),
  paste0("Run_", seq_along(synth_results_cat_st))  # Az egyes futtatásokat névvel látjuk el
)

############################# MEAN TWOWAY UTILITY ##############################

utility_results_cat_st <- setNames(
  lapply(seq_along(synth_data_cat_st), function(i) {  # Végigmegyünk minden futtatáson
    print(paste("Futtatás:", i))  # Debugging
    
    setNames(
      lapply(names(synth_data_cat_st[[i]]), function(name) {  # Minden datasetre külön lefut
        print(paste("Feldolgozás alatt:", name, "Futtatás:", i))  # Debugging
        table_utility_synthpop(synth_data_cat_st[[i]][[name]], datasets[[name]])  # Most már a teljes listát adjuk át
      }),
      names(synth_data_cat_st[[i]])
    )
  }),
  paste0("Run_", seq_along(synth_data_cat_st))  # Az egyes futtatásokat névvel látjuk el
)

############################# UTILITY.GEN ###################################### 
# Propensity score-alapú módszert használ, vagyis azt becsli
# (CART klasszifikációs modell), hogy egy adott megfigyelés az eredeti vagy a 
# szintetikus adatbázisból származik-e.

# Utility értékek kiszámítása minden szintetizált adatcsoportra
utility_gen_results_cat_st <- setNames(
  lapply(seq_along(synth_data_cat_st), function(i) {  # Végigmegyünk minden futtatáson
    print(paste("Futtatás:", i))  # Debugging
    
    setNames(
      lapply(names(synth_data_cat_st[[i]]), function(name) {  # Minden datasetre külön lefut
        print(paste("Feldolgozás alatt:", name, "Futtatás:", i))  # Debugging
        lapply(synth_data_cat_st[[i]][[name]], function(df) {
          table_utility_avg(df, datasets[[name]])
        })
      }),
      names(synth_data_cat_st[[i]])
    )
  }),
  paste0("Run_", seq_along(synth_data_cat_st))  # Az egyes futtatásokat névvel látjuk el
)

# Utility.gen értékek átlagolása az összes futtatásra és datasetre
utility_gen_results_avg_cat_st <- setNames(
  lapply(names(utility_gen_results_cat_st), function(run_name) {  # Minden Run-ra külön számítás
    setNames(
      lapply(names(synth_data_cat_st[[1]]), function(name) {  # Minden datasetre külön
        all_pMSE <- unlist(lapply(utility_gen_results_cat_st[[run_name]][[name]], `[[`, "pMSE"))
        all_S_pMSE <- unlist(lapply(utility_gen_results_cat_st[[run_name]][[name]], `[[`, "S_pMSE"))
        
        list(
          pMSE_avg = mean(all_pMSE, na.rm = TRUE),
          S_pMSE_avg = mean(all_S_pMSE, na.rm = TRUE)
        )
      }),
      names(synth_data_cat_st[[1]])
    )
  }),
  names(utility_gen_results_cat_st)
)

# Az átlagolt utility eredmények kiírása
print(utility_gen_results_avg_cat_st)

############################ TÁBLÁZAT CATALL STANDARD ##########################
# Táblázatkészítő függvény:
# Ha nincs epsilon, akkor csak egy dummy érték kell a ciklushoz
epsilon_values_cat_st <- 1  # Csak azért kell, hogy a for ciklus működjön, ténylegesen nincs rá szükség

# Az adatszettek nevei
df_names_cat_st <- names(ru_results_cat_st[[1]])

# Lista létrehozása
final_table_list_cat_st <- list()

# Minden futtatásra külön blokk létrehozása
for (i in seq_along(epsilon_values_cat_st)) {
  
  # Minden futtatásra KÜLÖN generáljuk a megfelelő értékeket
  rep_avg <- sapply(df_names_cat_st, function(ds) ru_results_cat_st[[i]][[ds]]$rep_avg)
  per_rep <- sapply(df_names_cat_st, function(ds) ru_results_cat_st[[i]][[ds]]$per_rep_avg)
  ru_as_p1 <- sapply(df_names_cat_st, function(ds) ru_results_cat_st[[i]][[ds]]$ru_as_p1_avg)
  mean_twoway_utility <- sapply(df_names_ipf, function(ds) utility_results_cat_st[[i]][[ds]]$mean_two_way)
  mean_threeway_utility <- sapply(df_names_ipf, function(ds) utility_results_cat_st[[i]][[ds]]$mean_three_way)
  worst_threeway_utility <- sapply(df_names_ipf, function(ds) utility_results_cat_st[[i]][[ds]]$mean_worstn)
  utility_pmse <- sapply(df_names_cat_st, function(ds) utility_gen_results_avg_cat_st[[i]][[ds]]$pMSE_avg)
  utility_spmse <- sapply(df_names_cat_st, function(ds) utility_gen_results_avg_cat_st[[i]][[ds]]$S_pMSE_avg)
  
  # Táblázati blokk létrehozása helyes struktúrával
  table_block <- rbind(
    c("Replikációk", rep_avg),  # RU százalék sor (üres epsilon cellával)
    c("Replikációk \\%", round(per_rep, digits = 2)),
    c("RU as \\% of p1", round(ru_as_p1, digits = 2)),  # RU as % of p1 sor (üres epsilon cellával)
    c("Kétdimenziós felh.", round(mean_twoway_utility, digits = 2)),
    c("Háromdimenziós felh.", round(mean_threeway_utility, digits = 2)),
    c("Legrosszabb háromdimenziós felh.", round(worst_threeway_utility, digits = 2)),
    c("Utility.gen pMSE", round(utility_pmse, digits = 3)),
    c("Utility.gen S_pMSE", round(utility_spmse, digits = 2))
  )
  
  # Oszlopnevek beállítása
  colnames(table_block) <- c("Metric", df_names_cat_st)
  
  # Az adott futtatás táblázati eredményeit hozzáadjuk a listához
  final_table_list_cat_st[[i]] <- as.data.frame(table_block, stringsAsFactors = FALSE)
}

# Az összes táblázati blokk összefűzése, hogy minden futtatás külön kerüljön bele
final_table_cat_st <- do.call(rbind, final_table_list_cat_st)

# LaTeX táblázat generálása helyes formázással
latex_output <- kable(final_table_cat_st, format = "latex", booktabs = TRUE, escape = FALSE, align = "l") %>%
  kable_styling(latex_options = c("hold_position")) %>%
  column_spec(1, width = "4cm")

# LaTeX fájlba mentés (Overleaf kompatibilis)
header <- "\\documentclass{article}
\\usepackage[table]{xcolor}
\\usepackage{booktabs}
\\usepackage{colortbl}
\\definecolor{mygray}{gray}{0.9}
\\begin{document}
\\rowcolors{2}{mygray!30}{white}"

footer <- "\\end{document}"

writeLines(enc2utf8(c(header, latex_output, footer)), "table_catall.tex", useBytes = TRUE)

############################### STANDARD IPF ###################################

# Ötszöri szintetizálás függvénye
if (!exists("synth_results_ipf_st")) {
  synth_results_ipf_st <- list()
}

synthesize_data_ipf_st <- function(data) {
  replicate(5, syn(data, method = "ipf", ipf.priorn = 1, ipf.gmargins = "twoway", 
                   ipf.othmargins = NULL), simplify = FALSE)
}

# Függvény alkalmazása a definiált adatszettekre
synth_results_ipf_st <- append(synth_results_ipf_st, list(lapply(datasets, synthesize_data_ipf_st)))

# Szintetikus adatbázisok kinyerése (synds -> dataframe)
synth_data_ipf_st <- lapply(synth_results_ipf_st, function(dataset_list) {
  lapply(dataset_list, function(syn_list) {
    lapply(syn_list, function(syn_obj) syn_obj$syn)
  })
}) # [[1]],[[2]]...stb sorrendben lesznek a különböző epsilonnal generált futtatások

################################# RU_SCORE #####################################

ru_results_ipf_st <- setNames(
  lapply(seq_along(synth_results_ipf_st), function(i) {  # Végigmegyünk minden futtatáson
    print(paste("--Futtatás:", i))  # Debugging
    
    setNames(
      lapply(names(synth_results_ipf_st[[i]]), function(name) {  # Minden datasetre külön lefut
        print(paste("Feldolgozás alatt:", name, "Futtatás:", i))  # Debugging
        table_ru_avg(synth_results_ipf_st[[i]][[name]], datasets[[name]]) 
      }),
      names(synth_results_ipf_st[[i]])
    )
  }),
  paste0("Run_", seq_along(synth_results_ipf_st))  # Az egyes futtatásokat névvel látjuk el
)

############################# MEAN TWOWAY UTILITY ##############################

utility_results_ipf_st <- setNames(
  lapply(seq_along(synth_data_ipf_st), function(i) {  # Végigmegyünk minden futtatáson
    print(paste("Futtatás:", i))  # Debugging
    
    setNames(
      lapply(names(synth_data_ipf_st[[i]]), function(name) {  # Minden datasetre külön lefut
        print(paste("Feldolgozás alatt:", name, "Futtatás:", i))  # Debugging
        table_utility_synthpop(synth_data_ipf_st[[i]][[name]], datasets[[name]])  # Most már a teljes listát adjuk át
      }),
      names(synth_data_ipf_st[[i]])
    )
  }),
  paste0("Run_", seq_along(synth_data_ipf_st))  # Az egyes futtatásokat névvel látjuk el
)

############################# UTILITY.GEN ###################################### 
# Propensity score-alapú módszert használ, vagyis azt becsli
# (CART klasszifikációs modell), hogy egy adott megfigyelés az eredeti vagy a 
# szintetikus adatbázisból származik-e.

# Utility értékek kiszámítása minden szintetizált adatcsoportra
utility_gen_results_ipf_st <- setNames(
  lapply(seq_along(synth_data_ipf_st), function(i) {  # Végigmegyünk minden futtatáson
    print(paste("Futtatás:", i))  # Debugging
    
    setNames(
      lapply(names(synth_data_ipf_st[[i]]), function(name) {  # Minden datasetre külön lefut
        print(paste("Feldolgozás alatt:", name, "Futtatás:", i))  # Debugging
        lapply(synth_data_ipf_st[[i]][[name]], function(df) {
          table_utility_avg(df, datasets[[name]])
        })
      }),
      names(synth_data_ipf_st[[i]])
    )
  }),
  paste0("Run_", seq_along(synth_data_ipf_st))  # Az egyes futtatásokat névvel látjuk el
)

# Utility.gen értékek átlagolása az összes futtatásra és datasetre
utility_gen_results_avg_ipf_st <- setNames(
  lapply(names(utility_gen_results_ipf_st), function(run_name) {  # Minden Run-ra külön számítás
    setNames(
      lapply(names(synth_data_ipf_st[[1]]), function(name) {  # Minden datasetre külön
        all_pMSE <- unlist(lapply(utility_gen_results_ipf_st[[run_name]][[name]], `[[`, "pMSE"))
        all_S_pMSE <- unlist(lapply(utility_gen_results_ipf_st[[run_name]][[name]], `[[`, "S_pMSE"))
        
        list(
          pMSE_avg = mean(all_pMSE, na.rm = TRUE),
          S_pMSE_avg = mean(all_S_pMSE, na.rm = TRUE)
        )
      }),
      names(synth_data_ipf_st[[1]])
    )
  }),
  names(utility_gen_results_ipf_st)
)

# Az átlagolt utility eredmények kiírása
print(utility_gen_results_avg_ipf_st)

############################ TÁBLÁZAT IPF STANDARD ##########################
# Táblázatkészítő függvény:

# Ha nincs epsilon, akkor csak egy dummy érték kell a ciklushoz
epsilon_values_ipf_st <- 1  # Csak azért kell, hogy a for ciklus működjön, ténylegesen nincs rá szükség

# Az adatszettek nevei
df_names_ipf_st <- names(ru_results_ipf_st[[1]])

# Lista létrehozása
final_table_list_ipf_st <- list()

# Minden futtatásra külön blokk létrehozása
for (i in seq_along(epsilon_values_ipf_st)) {
  
  # Minden futtatásra KÜLÖN generáljuk a megfelelő értékeket
  rep_avg <- sapply(df_names_ipf_st, function(ds) ru_results_ipf_st[[i]][[ds]]$rep_avg)
  per_rep <- sapply(df_names_ipf_st, function(ds) ru_results_ipf_st[[i]][[ds]]$per_rep_avg)
  ru_as_p1 <- sapply(df_names_ipf_st, function(ds) ru_results_ipf_st[[i]][[ds]]$ru_as_p1_avg)
  mean_twoway_utility <- sapply(df_names_ipf, function(ds) utility_results_ipf_st[[i]][[ds]]$mean_two_way)
  mean_threeway_utility <- sapply(df_names_ipf, function(ds) utility_results_ipf_st[[i]][[ds]]$mean_three_way)
  worst_threeway_utility <- sapply(df_names_ipf, function(ds) utility_results_ipf_st[[i]][[ds]]$mean_worstn)
  utility_pmse <- sapply(df_names_ipf_st, function(ds) utility_gen_results_avg_ipf_st[[i]][[ds]]$pMSE_avg)
  utility_spmse <- sapply(df_names_ipf_st, function(ds) utility_gen_results_avg_ipf_st[[i]][[ds]]$S_pMSE_avg)
  
  # Táblázati blokk létrehozása helyes struktúrával
  table_block <- rbind(
    c("Replikációk", rep_avg),  # RU százalék sor (üres epsilon cellával)
    c("Replikációk \\%", round(per_rep, digits = 2)),
    c("RU as \\% of p1", round(ru_as_p1, digits = 2)),  # RU as % of p1 sor (üres epsilon cellával)
    c("Kétdimenziós felh.", round(mean_twoway_utility, digits = 2)),
    c("Háromdimenziós felh.", round(mean_threeway_utility, digits = 2)),
    c("Legrosszabb háromdimenziós felh.", round(worst_threeway_utility, digits = 2)),
    c("Utility.gen pMSE", round(utility_pmse, digits = 3)),
    c("Utility.gen S_pMSE", round(utility_spmse, digits = 2))
  )
  
  # Oszlopnevek beállítása
  colnames(table_block) <- c("Metric", df_names_ipf_st)
  
  # Az adott futtatás táblázati eredményeit hozzáadjuk a listához
  final_table_list_ipf_st[[i]] <- as.data.frame(table_block, stringsAsFactors = FALSE)
}

# Az összes táblázati blokk összefűzése, hogy minden futtatás külön kerüljön bele
final_table_ipf_st <- do.call(rbind, final_table_list_ipf_st)

# LaTeX táblázat generálása helyes formázással
latex_output <- kable(final_table_ipf_st, format = "latex", booktabs = TRUE, escape = FALSE, align = "l") %>%
  kable_styling(latex_options = c("hold_position")) %>%
  column_spec(1, width = "4cm")

# LaTeX fájlba mentés (Overleaf kompatibilis)
header <- "\\documentclass{article}
\\usepackage[table]{xcolor}
\\usepackage{booktabs}
\\usepackage{colortbl}
\\definecolor{mygray}{gray}{0.9}
\\begin{document}
\\rowcolors{2}{mygray!30}{white}"

footer <- "\\end{document}"

writeLines(enc2utf8(c(header, latex_output, footer)), "table_ipf.tex", useBytes = TRUE)

################################### ELEMZÉS ####################################

# Elemzésben/adatvizualizációkban szereplő eredeti, nem szintetizált adatok
pre_an <- an_vars[, c("Nem", "Kor", "Isk.végz", "Pol.hír.perc", "Pol.érd", 
                      "Elégedettség.korm", "Bizalom", "Szavazat", "Magasság","Súly",
                      "Szív.prob", "Vérnyomás.prob", "Diabetes.prob", "BMI")]

################################ ADATVIZUALIZÁCIÓK #############################
# Leíró statisztikák skimr
skim(pre_an)


freq_tables <- function(data) {
  for (varname in names(data)) {
    column <- data[[varname]]
    
    # Frekvencia táblázat
    freq <- as.data.frame(table(column, useNA = "ifany"))
    colnames(freq) <- c("Value", "Count")
    freq$Percent <- round(100 * freq$Count / sum(freq$Count), 2)
    
    # NA és unique info
    n_na <- sum(is.na(column))
    n_unique <- length(unique(column[!is.na(column)]))
    
    cat("\n\n==============================================\n")
    cat("Változó:", varname, "\n")
    cat("Hiányzó értékek:", n_na, " | Egyedi értékek száma:", n_unique, "\n")
    cat("==============================================\n")
    
    print(knitr::kable(freq, format = "simple", align = "lrr", caption = paste("Frekvenciatábla:", varname)))
  }
}

freq_tables(pre_an)


# Változóink listája
categorical_vars <- names(pre_an)

# Isk.végz & Szavazat 90°-kal elforgatva jelenjen meg
flip_vars <- c("Isk.végz", "Szavazat")  

for (var in categorical_vars) {
  p <- ggplot(an_vars, aes(x = .data[[var]])) +
    geom_bar(fill = "steelblue") +
    theme_minimal() +
    labs(y = "Gyakoriság")
  
  # Apply coord_flip() only for the selected variables
  if (var %in% flip_vars) {
    p <- p + coord_flip()
  }
  
  print(p)
}

round(prop.table(table(pre_an$`Pol.hír.perc`, pre_an$`Pol.érd`), margin = 1) * 100, 1)

# Változópárok együttes eloszlásának ábrázolása
key_pairs <- list(
  c("Nem", "Kor"),
  c("BMI", "Szív.prob"),
  c("BMI", "Diabetes.prob"),
  c("BMI", "Vérnyomás.prob"),
  c("Isk.végz", "Pol.érd"),
  c("Pol.hír.perc", "Pol.érd")
)

for (pair in key_pairs) {
  print(
    ggplot(an_vars, aes(x = .data[[pair[1]]], fill = .data[[pair[2]]])) +
      geom_bar(position = "fill") +
      theme_minimal() +
      labs(x = pair[1], y = "Arány", fill = pair[2]) +
      coord_flip() 
  )
}

# Külön a szavazat és elégedettség
key_pairs <- list(
  c("Szavazat", "Elégedettség.korm")
)

# Rövidítő függvény a hosszú pártnevekhez
shorten_labels <- function(x) {
  sub("\\s*\\(.*?\\)", "", x)  # Törli a zárójelek közötti részt
}

for (pair in key_pairs) {
  print(
    ggplot(an_vars, aes(x = .data[[pair[1]]], fill = .data[[pair[2]]])) +
      geom_bar(position = "fill", color = "black") +  # Kontúrral az oszlopok jól láthatók
      geom_text(
        aes(label = .data[[pair[2]]]),  # Címke a jelmagyarázat értéke
        stat = "count",
        position = position_fill(vjust = 0.5),  # Címkék az oszlopok alján
        size = 3.5, color = "white", fontface = "bold",
        check_overlap = T
      ) +
      theme_minimal() +
      labs(
        x = pair[1], y = "Arány", fill = pair[2]
      ) +
      coord_flip() +
      scale_x_discrete(labels = shorten_labels)  # Rövidített címkék alkalmazása
  )
}

# Hosszú formára alakítás
long_df <- an_vars %>%
  pivot_longer(
    cols = c("Szív.prob", "Diabetes.prob", "Vérnyomás.prob"),
    names_to = "Célváltozó",
    values_to = "Érték"
  )

ggplot(long_df, aes(x = BMI, fill = Érték)) +
  geom_bar(position = "fill") +
  facet_wrap(~ Célváltozó) +
  coord_flip() +
  theme_minimal() +
  labs(x = "BMI", y = "Arány", fill = "Érték") +
  scale_y_continuous(labels = scales::percent_format())

############################## LEÍRÓ STATISZTIKÁK ##############################

# Ha külön plottolva szeretnénk eljárni pl:
compare(synth_results_ipf[[1]]$an_4[[1]], datasets$an_4, utility.stats = c("S_pMSE", "df"))

# Függvény az összes változó oneway utility összehasonlításhoz
compare_all <- function(synth_results, observed_data, utility.stats = c("S_pMSE", "df")) {
  results <- list()
  for (i in seq_along(synth_results)) {
    for (j in 1:5) {
      for (k in 1:5) {
        syn_data <- synth_results[[i]][[paste0("an_", j)]][[k]]
        obs_data <- observed_data[[paste0("an_", j)]]
        comparison <- compare(syn_data, obs_data, utility.stats = utility.stats, plot = FALSE)
        key <- paste0("epsilon_", i, "_an_", j, "_rep_", k)
        results[[key]] <- comparison
      }
    }
  }
  return(results)
}

# Listába mentjük az eredményeket
all_ipf_utilities <- compare_all(synth_results_ipf, datasets)
all_catall_utilities <- compare_all(synth_results_catall, datasets)

all_ipf_utilities_st <- compare_all(synth_results_ipf_st, datasets)
all_catall_utilities_st <- compare_all(synth_results_cat_st, datasets)

# Függvény ami átlagolja a oneway értékeket futtatásonként és adatbázisokra lebontva
avg_oneway_SpMSE <- function(utility_results) {
  all_entries <- list()
  
  for (key in names(utility_results)) {
    this_entry <- utility_results[[key]]
    
    # Kinyerés
    variables <- this_entry$vars
    stats <- this_entry$tab.utility
    
    epsilon <- strsplit(key, "_")[[1]][2]
    analysis <- paste0("an_", strsplit(key, "_")[[1]][4])
    replicate <- paste0("rep_", strsplit(key, "_")[[1]][6])
    
    df <- data.frame(
      epsilon = epsilon,
      analysis = analysis,
      replicate = replicate,
      variable = variables,
      S_pMSE = stats[, 1]
    )
    
    all_entries[[key]] <- df
  }
  
  combined <- do.call(rbind, all_entries)
  
  # Átlagolás epsilon + analysis + variable szintjén
  library(dplyr)
  combined %>%
    group_by(epsilon, analysis, variable) %>%
    summarise(mean_SpMSE = mean(S_pMSE), .groups = "drop")
}

# Eredmények elmentése (DP és standard szintetizálások)
avg_oneway_ipf <- avg_oneway_SpMSE(all_ipf_utilities)
print(avg_oneway_ipf, n = 132)

avg_oneway_catall <- avg_oneway_SpMSE(all_catall_utilities)
print(avg_oneway_catall, n = 132)

avg_oneway_ipf_st <- avg_oneway_SpMSE(all_ipf_utilities_st)
print(avg_oneway_ipf_st, n = 33)
mean(avg_oneway_ipf_st$mean_SpMSE)

avg_oneway_catall_st <- avg_oneway_SpMSE(all_catall_utilities_st)
print(avg_oneway_catall_st, n = 33)
mean(avg_oneway_catall_st$mean_SpMSE)

# Ezzell a függvénnyel szűrhetünk adott epsilonra és azon futtatások az S_pMSE átlagait 
# fogja így kiátlagolni
avg_oneway_eps <- function(adatok, epszilon_ertek) {
  # Szűrés az adott epsilon értékre
  szurt <- subset(adatok, epsilon == epszilon_ertek)
  
  # Átlag számítás
  atlag <- mean(szurt$mean_SpMSE, na.rm = TRUE)
  
  return(atlag)
}

avg_oneway_eps(avg_oneway_catall, 1)
avg_oneway_eps(avg_oneway_ipf, 1) #... és így tovább vizsgálódhatunk


# Hasonlítsuk össze egy dataframe-ben a oneway S_pMSE eltéréseket
catall_renamed <- avg_oneway_catall %>%
  rename(mean_SpMSE_catall = mean_SpMSE)

ipf_renamed <- avg_oneway_ipf %>%
  rename(mean_SpMSE_ipf = mean_SpMSE)

# Összefűzés
merged_clean <- inner_join(
  catall_renamed,
  ipf_renamed,
  by = c("epsilon", "analysis", "variable")
)

# Különbség számítása és új oszlopban szerepeltetése
merged_clean <- merged_clean %>%
  mutate(difference = mean_SpMSE_catall - mean_SpMSE_ipf)

# Melyik a jobb? (oszlop)
merged_clean <- merged_clean %>%
  mutate(winner = ifelse(difference > 0, "ipf", "catall"))

print(merged_clean, n= 132)

# Vizualizáció onewayre
ggplot(merged_clean, aes(x = reorder(paste(analysis, variable), difference), y = difference, fill = winner)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~ epsilon, scales = "free_y") +
  labs(
    x = "Változó (elemzésenként)",
    y = "Különbség (SpMSE)",
    fill = "Jobb eljárás",
  ) +
  scale_fill_manual(values = c("ipf" = "steelblue", "catall" = "tomato")) +
  theme_minimal(base_size = 10)
#################################### GLM #######################################
# Itt is egy példa, ha adott adatbázisra akarjuk végezni
# model_syn <- glm.synds(Szív.prob ~ Kor + BMI, data = synth_results_ipf[[3]]$an_5[[1]], family = "binomial")
# summary(model_syn)
# compare(model_syn, pre_an, plot = "coef", print.coef = TRUE)

# Függvény, amely az általunk megadott adatbázisra futtatja le az összehasonlítást, mind az 5 futtatásra
glm_results <- function(synth_data, model_formula, pre_an, indices = 1:5) {
  compare_results <- list()
  
  for (i in seq_along(indices)) {
    model_syn <- try(eval(bquote(glm.synds(
      formula = .(model_formula),
      data = synth_data[[.(indices[i])]],
      family = "binomial"
    ))), silent = TRUE)
    
    comparison <- try(compare(model_syn, pre_an, plot = "coef", print.coef = FALSE), silent = TRUE)
    
    if (inherits(comparison, "try-error")) {
      message(paste("compare() hiba ", i, ". replikációval"))
      compare_results[[i]] <- NULL
    } else {
      compare_results[[i]] <- comparison
    }
  }
  
  return(compare_results)
}

# Szedjük ki a Nem változóból a 'No answer' szintet
# Meghatározzuk a megtartandó szinteket
valid_levels_glm <- c("Male", "Female")

# Az eredeti df-ből is kiszedjük ezeket a szinteket (megfigyelés nem volt, csak mint szint szerepel)
pre_an$Nem <- factor(pre_an$Nem,
                         levels = valid_levels_glm,
                         ordered = FALSE)


# Csak a synth_results_cat_st[[1]] eredményekre vonatkozik (átírva felhasználható máshoz is)
for (j in 1:5) {
  # Szűrés
  synth_results_cat_st[[1]][["an_5"]][[j]][["syn"]] <- subset(
    synth_results_cat_st[[1]][["an_5"]][[j]][["syn"]],
    Nem %in% valid_levels_glm
  )
  
  # Újrafaktorozás
  synth_results_cat_st[[1]][["an_5"]][[j]][["syn"]]$Nem <- factor(
    synth_results_cat_st[[1]][["an_5"]][[j]][["syn"]]$Nem,
    levels = valid_levels_glm,
    ordered = FALSE
  )
}

# IPF standard
for (j in 1:5) {
  # Szűrés
  synth_results_ipf_st[[1]][["an_5"]][[j]][["syn"]] <- subset(
    synth_results_ipf_st[[1]][["an_5"]][[j]][["syn"]],
    Nem %in% valid_levels_glm
  )
  
  # Újrafaktorozás
  synth_results_ipf_st[[1]][["an_5"]][[j]][["syn"]]$Nem <- factor(
    synth_results_ipf_st[[1]][["an_5"]][[j]][["syn"]]$Nem,
    levels = valid_levels_glm,
    ordered = FALSE
  )
}

# Catall DP
for (j in 1:5) {
  # Szűrés
  synth_results_catall[[4]][["an_5"]][[j]][["syn"]] <- subset(
    synth_results_catall[[4]][["an_5"]][[j]][["syn"]],
    Nem %in% valid_levels_glm
  )
  
  # Újrafaktorozás
  synth_results_catall[[4]][["an_5"]][[j]][["syn"]]$Nem <- factor(
    synth_results_catall[[4]][["an_5"]][[j]][["syn"]]$Nem,
    levels = valid_levels_glm,
    ordered = FALSE
  )
}

# IPF DP
for (j in 1:5) {
  # Szűrés
  synth_results_ipf[[4]][["an_5"]][[j]][["syn"]] <- subset(
    synth_results_ipf[[4]][["an_5"]][[j]][["syn"]],
    Nem %in% valid_levels_glm
  )
  
  # Újrafaktorozás
  synth_results_ipf[[4]][["an_5"]][[j]][["syn"]]$Nem <- factor(
    synth_results_ipf[[4]][["an_5"]][[j]][["syn"]]$Nem,
    levels = valid_levels_glm,
    ordered = FALSE
  )
}

# Printelés és plotolás
# STANDARD
glm_diabetes_cat <- glm_results(
  synth_results_cat_st[[1]]$an_5,
  model_formula = Diabetes.prob ~ Nem + Kor + BMI, # itt változtathatunk a modellen ( így az összes többinél is)
  pre_an = pre_an
)
glm_diabetes_cat

glm_sziv_cat <- glm_results(
  synth_results_cat_st[[1]]$an_5,
  model_formula = Szív.prob ~ Nem + Kor + BMI,
  pre_an = pre_an
)
glm_sziv_cat

glm_vernyomas_cat <- glm_results(
  synth_results_cat_st[[1]]$an_5,
  model_formula = Vérnyomás.prob ~ Nem + Kor + BMI,
  pre_an = pre_an
)
glm_vernyomas_cat

# IPF
glm_diabetes_ipf <- glm_results(
  synth_results_ipf_st[[1]]$an_5,
  model_formula = Diabetes.prob ~ Nem + Kor + BMI,
  pre_an = pre_an
)
glm_diabetes_ipf

glm_sziv_ipf <- glm_results(
  synth_results_ipf_st[[1]]$an_5,
  model_formula = Szív.prob ~ Nem + Kor + BMI,
  pre_an = pre_an
)
glm_sziv_ipf

glm_vernyomas_ipf <- glm_results(
  synth_results_ipf_st[[1]]$an_5,
  model_formula = Vérnyomás.prob ~ Nem + Kor + BMI,
  pre_an = pre_an
)
glm_vernyomas_ipf


# IPF DP
# Diabetes
glm_diabetes_ipf_dp <- glm_results(
  synth_results_ipf[[4]]$an_5,
  model_formula = Diabetes.prob ~ Nem + Kor + BMI,
  pre_an = pre_an
)
glm_diabetes_ipf_dp

# Szív- és érrendszeri probléma
glm_sziv_ipf_dp <- glm_results(
  synth_results_ipf[[4]]$an_5,
  model_formula = Szív.prob ~ Nem + Kor + BMI,
  pre_an = pre_an
)
glm_sziv_ipf_dp

# Vérnyomás probléma
glm_vernyomas_ipf_dp <- glm_results(
  synth_results_ipf[[4]]$an_5,
  model_formula = Vérnyomás.prob ~ Nem + Kor + BMI,
  pre_an = pre_an
)
glm_vernyomas_ipf_dp


# CATALL DP
glm_diabetes_cat_dp <- glm_results(
  synth_results_catall[[4]]$an_5,
  model_formula = Diabetes.prob ~ Nem + Kor + BMI,
  pre_an = pre_an
)
glm_diabetes_cat_dp

glm_sziv_cat_dp <- glm_results(
  synth_results_catall[[4]]$an_5,
  model_formula = Szív.prob ~ Nem + Kor + BMI,
  pre_an = pre_an
)
glm_sziv_cat_dp

glm_vernyomas_cat_dp <- glm_results(
  synth_results_catall[[4]]$an_5,
  model_formula = Vérnyomás.prob ~ Nem + Kor + BMI,
  pre_an = pre_an
)
glm_vernyomas_cat_dp

m2 <- glm(Vérnyomás.prob ~ Nem + Kor + BMI, family= "binomial", data = pre_an)
m1 <- glm.synds(Vérnyomás.prob ~ Nem + Kor + BMI, family= "binomial", data = synth_results_catall[[3]]$an_5[[1]])

m_p <- glm(Vérnyomás.prob ~ Nem + Kor + BMI, family= "binomial", data = pre_an)
summary(m_p)
compare(m1, pre_an, plot = "coef", print.coef = FALSE, na.rm=T) #replikálta a nownswert a catall nagy mennyiségben
################################### POLR #######################################
# polr modell illesztő fv. plusz compare() 
polr_results <- function(synth_data, model_formula, pre_an, indices = 1:5) {
  compare_results <- list()
  
  for (i in seq_along(indices)) {
    model_syn <- try(eval(bquote(polr.synds(
      formula = .(model_formula),
      data = .(synth_data[[indices[i]]])
    ))), silent = TRUE)
    
    comparison <- try(compare(model_syn, pre_an, plot = "coef", print.coef = FALSE), silent = TRUE)
    
    if (inherits(comparison, "try-error")) {
      message(paste("compare() hiba ", i, ". replikációval"))
      compare_results[[i]] <- NULL
    } else {
      compare_results[[i]] <- comparison
    }
  }
  
  return(compare_results)
}

# Kiszedjük az NA-ra utaló kategóriákat - mert ezek nem ordinális szintek
# Meghatározzuk a megtartandó szinteket
valid_levels <- c("Not at all interested", "Hardly interested", "Quite interested", "Very interested")

# Az eredeti df-ből is kiszedjük ezeket a szinteket (megfigyelés nem volt, csak mint szint szerepel)
pre_an$Pol.érd <- factor(pre_an$Pol.érd,
                         levels = valid_levels,
                         ordered = TRUE)

# Csak a synth_results_cat_st[[1]] eredményekre vonatkozik (átírva felhasználható máshoz is)
for (j in 1:5) {
  # Szűrés
  synth_results_cat_st[[1]][["an_1"]][[j]][["syn"]] <- subset(
    synth_results_cat_st[[1]][["an_1"]][[j]][["syn"]],
    Pol.érd %in% valid_levels
  )
  
  # Újrafaktorozás
  synth_results_cat_st[[1]][["an_1"]][[j]][["syn"]]$Pol.érd <- factor(
    synth_results_cat_st[[1]][["an_1"]][[j]][["syn"]]$Pol.érd,
    levels = valid_levels,
    ordered = TRUE
  )
}

# Csak a synth_results_ipf_st[[1]] eredményekre vonatkozik
for (j in 1:5) {
  # Szűrés
  synth_results_ipf_st[[1]][["an_1"]][[j]][["syn"]] <- subset(
    synth_results_ipf_st[[1]][["an_1"]][[j]][["syn"]],
    Pol.érd %in% valid_levels
  )
  
  # Újrafaktorozás
  synth_results_ipf_st[[1]][["an_1"]][[j]][["syn"]]$Pol.érd <- factor(
    synth_results_ipf_st[[1]][["an_1"]][[j]][["syn"]]$Pol.érd,
    levels = valid_levels,
    ordered = TRUE
  )
}

# Csak a synth_results_catall[[4]] eredményekre vonatkozik
for (j in 1:5) {
  # Szűrés
  synth_results_catall[[4]][["an_1"]][[j]][["syn"]] <- subset(
    synth_results_catall[[4]][["an_1"]][[j]][["syn"]],
    Pol.érd %in% valid_levels
  )
  
  # Újrafaktorozás
  synth_results_catall[[4]][["an_1"]][[j]][["syn"]]$Pol.érd <- factor(
    synth_results_catall[[4]][["an_1"]][[j]][["syn"]]$Pol.érd,
    levels = valid_levels,
    ordered = TRUE
  )
}

# Csak a synth_results_ipf[[4]] eredményekre vonatkozik - itt igazából csak 
# a szintek közül szedi ki, mert ilyen megfigyelések nem voltak
for (j in 1:5) {
  # Szűrés
  synth_results_ipf[[4]][["an_1"]][[j]][["syn"]] <- subset(
    synth_results_ipf[[4]][["an_1"]][[j]][["syn"]],
    Pol.érd %in% valid_levels
  )
  
  # Újrafaktorozás
  synth_results_ipf[[4]][["an_1"]][[j]][["syn"]]$Pol.érd <- factor(
    synth_results_ipf[[4]][["an_1"]][[j]][["syn"]]$Pol.érd,
    levels = valid_levels,
    ordered = TRUE
  )
}

# Catall DP (Nem-et itt is javítjuk a catallnál)
for (j in 1:5) {
  # Szűrés
  synth_results_catall[[4]][["an_1"]][[j]][["syn"]] <- subset(
    synth_results_catall[[4]][["an_1"]][[j]][["syn"]],
    Nem %in% valid_levels_glm
  )
  
  # Újrafaktorozás
  synth_results_catall[[4]][["an_1"]][[j]][["syn"]]$Nem <- factor(
    synth_results_catall[[4]][["an_1"]][[j]][["syn"]]$Nem,
    levels = valid_levels_glm,
    ordered = FALSE
  )
}

# Printelés és plotolás
# sima catall
polr_polérd_cat <- polr_results(
  synth_results_cat_st[[1]]$an_1,
  model_formula = Pol.érd ~ Nem + Kor + Pol.hír.perc,
  pre_an = pre_an
)
print(polr_polérd_cat[[1]]) #jó
print(polr_polérd_cat[[2]]) #jó
print(polr_polérd_cat[[3]]) #jó
print(polr_polérd_cat[[4]]) 
print(polr_polérd_cat[[5]]) #jó

# sima ipf
polr_polérd_ipf <- polr_results(
  synth_results_ipf_st[[1]]$an_1,
  model_formula = Pol.érd ~ Nem + Kor + Pol.hír.perc,
  pre_an = pre_an
)
print(polr_polérd_ipf[[1]]) #jó
print(polr_polérd_ipf[[2]]) #jó
print(polr_polérd_ipf[[3]])
print(polr_polérd_ipf[[4]])
print(polr_polérd_ipf[[5]]) #jó

# DP catall
polr_polérd_cat_dp <- polr_results(
  synth_results_catall[[4]]$an_1, # csak eps = 10 mellett lettek használható adatok
  model_formula = Pol.érd ~ Nem + Kor + Pol.hír.perc,
  pre_an = pre_an
)
print(polr_polérd_cat_dp[[1]]) #jó
print(polr_polérd_cat_dp[[2]]) #jó
print(polr_polérd_cat_dp[[3]]) #jó
print(polr_polérd_cat_dp[[4]]) #jó
print(polr_polérd_cat_dp[[5]]) #jó

# DP ipf
polr_polérd_ipf_dp <- polr_results(
  synth_results_ipf[[4]]$an_1,  # csak eps = 10 mellett lettek használható adatok
  model_formula = Pol.érd ~ Nem + Kor + Pol.hír.perc,
  pre_an = pre_an
)
print(polr_polérd_ipf_dp[[1]]) #jó
print(polr_polérd_ipf_dp[[2]]) #jó
print(polr_polérd_ipf_dp[[3]])
print(polr_polérd_ipf_dp[[4]])
print(polr_polérd_ipf_dp[[5]]) #jó


# # Összehasonlításra manuális példa
# compare(mod_syn, pre_an, plot = "coef", print.coef = TRUE)
# "Pol.hír.perc", "Pol.érd", "Elégedettség.korm"
# 
# test <- polr.synds(Pol.érd ~ Nem + Kor + Pol.hír.perc, synth_results_ipf[[3]]$an_1[[5]])
# compare(test, pre_an, plot = "coef", print.coef = F)

################################### MULTINOM ###################################
mult_results <- function(synth_data, model_formula, pre_an, indices = 1:5) {
  compare_results <- list()
  
  for (i in seq_along(indices)) {
    model_syn <- try(eval(bquote(multinom.synds(
      formula = .(model_formula),
      data = .(synth_data[[indices[i]]])
    ))), silent = TRUE)
    
    comparison <- try(compare(model_syn, pre_an, plot = "coef", print.coef = FALSE), silent = TRUE)
    compare_results[[i]] <- comparison
    }
  
  return(compare_results)
}

# Printelés és plotolás
mult_szavazat <- mult_results(
  synth_results_ipf[[3]]$an_3,
  model_formula = Szavazat ~ Nem + Kor + Elégedettség.korm,
  pre_an = pre_an
)
mult_szavazat
