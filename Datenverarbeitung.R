source("load_packages.R")

# Datum in korrektes Format umwandeln
data$date <- as.Date(data$date)

# Definition des Datums für den Regierungswechsel 1982
regierungswechsel_1982 <- as.Date("1982-10-01")

# 1. SPD in der Regierung
spd_reg <- data %>%
  filter(
    faction_id == 1 & (
      electoral_term == 8 |
        (electoral_term == 9 & date < regierungswechsel_1982) |
        electoral_term == 15
    )
  )

# 2. SPD in der Opposition
spd_opp <- data %>%
  filter(
    faction_id == 1 & (
      (electoral_term == 9 & date >= regierungswechsel_1982) |
        electoral_term == 10 |
        electoral_term == 11 |
        electoral_term == 12 |
        electoral_term == 13 |
        electoral_term == 17
    )
  )

# 3. CDU/CSU in der Regierung
cdu_reg <- data %>%
  filter(
    faction_id == 2 & (
      (electoral_term == 9 & date >= regierungswechsel_1982) |
        electoral_term == 10 |
        electoral_term == 11 |
        electoral_term == 12 |
        electoral_term == 13 |
        electoral_term == 17
    )
  )

# 4. CDU/CSU in der Opposition
cdu_opp <- data %>%
  filter(
    faction_id == 2 & (
      electoral_term == 8 |
        (electoral_term == 9 & date < regierungswechsel_1982) |
        electoral_term == 15
    )
  )

# Überprüfen der Datenmengen
cat("SPD in Regierung:", nrow(spd_reg), "Reden\n")
cat("SPD in Opposition:", nrow(spd_opp), "Reden\n")
cat("CDU/CSU in Regierung:", nrow(cdu_reg), "Reden\n")
cat("CDU/CSU in Opposition:", nrow(cdu_opp), "Reden\n")


save.image(file = "Datenverarbeitung_var.RData")
