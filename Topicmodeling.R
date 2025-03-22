source("load_packages.R")
common_dtm_loaded <- readRDS("common_dtm.rds")

# Identifiziere und entferne leere Dokumente
sel_idx <- rowSums(common_dtm_loaded) > 0
common_dtm_filtered <- common_dtm_loaded[sel_idx, ]

# Information über die gefilterten Daten ausgeben
cat("DTM vor Filterung:", nrow(common_dtm_loaded), "Dokumente,", ncol(common_dtm_loaded), "Features\n")
cat("DTM nach Filterung:", nrow(common_dtm_filtered), "Dokumente,", ncol(common_dtm_filtered), "Features\n")

library(topicmodels)
set.seed(1)  # Für Reproduzierbarkeit

K <- 30  # Anzahl der Topics
lda_model <- LDA(common_dtm_filtered, K, method = "Gibbs", 
                 control = list(iter = 500, seed = 1, verbose = 25, alpha = 0.02))


# Posterior-Verteilungen extrahieren
tmResult <- posterior(lda_model)
beta <- tmResult$terms   # Topics × Terms Matrix
theta <- tmResult$topics  # Documents × Topics Matrix

# Top-Begriffe für jedes Topic anzeigen
top_terms <- 20
terms_per_topic <- terms(lda_model, top_terms)
topic_names <- apply(terms_per_topic, 2, paste, collapse = " ")

topic_names <- sapply(1:K, function(k) {
  paste(terms(lda_model, 3)[,k], collapse = "-") # Top 3 Wörter pro Topic
}) %>% 
  setNames(paste0("T", sprintf("%02d", 1:K), ": "))
topicNames <- topic_names


# Ausgabe der häufigsten Begriffe pro Topic
for (i in 1:K) {
  cat("Topic", i, ": ", paste(terms_per_topic[, i], collapse = ", "), "\n")
}

library(LDAvis)
library(tsne)

# Funktion für dimensionsreduzierte Darstellung mit t-SNE
svd_tsne <- function(x) {
  # Zunächst SVD anwenden, um die Dimensionalität zu reduzieren
  svd_result <- svd(x)
  # Dann t-SNE auf die reduzierte Matrix anwenden
  tsne_result <- tsne(svd_result$u, perplexity = 30, max_iter = 1000)
  return(tsne_result)
}

# JSON für die Visualisierung erstellen
json <- createJSON(
  phi = beta,                           # Topic-Term-Matrix
  theta = theta,                        # Dokument-Topic-Matrix
  doc.length = rowSums(common_dtm_filtered),  # Dokument-Längen
  vocab = colnames(common_dtm_filtered),      # Vokabular
  term.frequency = colSums(common_dtm_filtered),  # Term-Frequenzen
  mds.method = svd_tsne,                # t-SNE für die Dimensionsreduktion
  plot.opts = list(xlab = "", ylab = "")
)

# Visualisierung anzeigen
serVis(json)


library(servr)

# Visualisierung für spätere Verwendung speichern
serVis(json, out.dir = "ldavis_output", open.browser = FALSE)

# Document-Topic-Matrix mit Metadaten anreichern
topic_proportions <- as.data.frame(theta)
colnames(topic_proportions) <- paste0("Topic_", 1:K)
topic_proportions$doc_id <- rownames(common_dtm_filtered)

# Partei und Regierungsstatus aus Dokument-IDs extrahieren
topic_proportions$party_gov <- sub("_.*", "", topic_proportions$doc_id)

# Nach Partei/Regierungsstatus aggregieren
library(dplyr)
library(reshape2)

topic_prop_by_party_gov <- topic_proportions %>%
  group_by(party_gov) %>%
  summarise(across(starts_with("Topic_"), mean))

# Für Visualisierung umformen
topic_prop_melted <- melt(as.data.frame(topic_prop_by_party_gov), 
                          id.vars = "party_gov", 
                          variable.name = "Topic", 
                          value.name = "Proportion")
# Heatmap erstellen(verworfen)
library(ggplot2)
ggplot(topic_prop_melted, aes(x = Topic, y = party_gov, fill = Proportion)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red")
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Themenanteile nach Partei und Regierungsstatus",
       x = "topic_names", y = "Partei_Regierungsstatus", fill = "Anteil")

ggsave("topic_by_party_gov_heatmap.png", width = 12, height = 8)



###### nach  Regierung/Opposition aufschlüsseln
################################################
head(rownames(theta))
summary(theta)

theta_spd_reg <- theta[grep("^SPD_Reg_", rownames(theta)), ]
theta_spd_opp <- theta[grep("^SPD_Opp_", rownames(theta)), ]
theta_cdu_reg <- theta[grep("^CDU_Reg_", rownames(theta)), ]
theta_cdu_opp <- theta[grep("^CDU_Opp_", rownames(theta)), ]

# Überprüfen der Größe der gefilterten Matrizen
cat("SPD Regierung:", nrow(theta_spd_reg), "Dokumente\n")
cat("SPD Opposition:", nrow(theta_spd_opp), "Dokumente\n")
cat("CDU Regierung:", nrow(theta_cdu_reg), "Dokumente\n")
cat("CDU Opposition:", nrow(theta_cdu_opp), "Dokumente\n")

# Überprüfung der unterschiedlichen Mittelwerte
head(colMeans(theta_spd_reg))
head(colMeans(theta_spd_opp))

# Überprüfung der Normalisierung
rowSums(theta)

# Berechnen der durchschnittlichen Topic-Anteile für jede Gruppe
mean_topics <- rbind(
  SPD_Reg = colMeans(theta_spd_reg),
  SPD_Opp = colMeans(theta_spd_opp),
  CDU_Reg = colMeans(theta_cdu_reg),
  CDU_Opp = colMeans(theta_cdu_opp)
)

# Erstellen eines Dataframes für die Visualisierung
topic_comparison <- as.data.frame(mean_topics)
topic_comparison$Group <- rownames(topic_comparison)
topic_comparison_long <- reshape2::melt(topic_comparison, id.vars = "Group")
colnames(topic_comparison_long) <- c("Group", "Topic", "Proportion")
topic_comparison_long$Topic <- factor(topic_comparison_long$Topic, levels = 1:K, labels = topic_names)




# Balkendiagramm erstellen
color_mapping <- c(
  "CDU_Reg" = "black",
  "CDU_Opp" = "brown",
  "SPD_Reg" = "red",
  "SPD_Opp" = "purple"
)
ggplot(topic_comparison_long, aes(x = Topic, y = Proportion, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = color_mapping) +  # Farbpalette anwenden
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Vergleich der Topic-Anteile zwischen Gruppen",
       x = "Topics", y = "Durchschnittlicher Anteil", fill = "Gruppe")

ggsave("topic_comparison_long.png", width = 12, height = 8)

topic_names
for (i in 1:K) {
  cat("Topic", i, ": ", paste(terms_per_topic[, i], collapse = ", "), "\n")
}
#####unterschiede spd und cdu(reg vs opp)
spd_diff <- abs(colMeans(theta_spd_reg) - colMeans(theta_spd_opp))
spd_diff_df <- data.frame(
  Topic = 1:length(spd_diff),
  SPD_Reg_Value = colMeans(theta_spd_reg),
  SPD_Opp_Value = colMeans(theta_spd_opp),
  Diff = spd_diff
)

# Topic-Namen hinzufügen, wenn vorhanden
if(exists("topic_names")) {
  spd_diff_df$Topic_Name <- topic_names
}

# Nach Differenzgröße sortieren (absteigend)
spd_diff_df <- spd_diff_df[order(-spd_diff_df$Diff),]

# Top 3 Topics mit den größten Unterschieden für SPD
spd_top5 <- head(spd_diff_df, 5)

# Berechnung der Differenzen zwischen Regierung und Opposition für CDU
cdu_diff <- abs(colMeans(theta_cdu_reg) - colMeans(theta_cdu_opp))
cdu_diff_df <- data.frame(
  Topic = 1:length(cdu_diff),
  CDU_Reg_Value = colMeans(theta_cdu_reg),
  CDU_Opp_Value = colMeans(theta_cdu_opp),
  Diff = cdu_diff
)

# Topic-Namen hinzufügen, wenn vorhanden
if(exists("topic_names")) {
  cdu_diff_df$Topic_Name <- topic_names
}

# Nach Differenzgröße sortieren (absteigend)
cdu_diff_df <- cdu_diff_df[order(-cdu_diff_df$Diff),]

# Top 3 Topics mit den größten Unterschieden für CDU
cdu_top5 <- head(cdu_diff_df, 5)

#visualisierung ## klappt nicht richtig, kann auch tabellen verwenden

library(ggplot2)
library(reshape2)

# SPD: Daten für Visualisierung vorbereiten
spd_top5_viz <- melt(spd_top5[, c("Topic", "SPD_Reg_Value", "SPD_Opp_Value")], id.vars = "Topic")
if(exists("topic_names")) {
  spd_top5_viz$Topic_Name <- rep(spd_top5$Topic_Name, each = 2)
  spd_top5_viz$Topic <- factor(spd_top5_viz$Topic_Name)
} else {
  spd_top5_viz$Topic <- factor(paste("Topic", spd_top5_viz$Topic))
}
spd_top5_viz$variable <- factor(spd_top5_viz$variable, 
                                levels = c("SPD_Reg_Value", "SPD_Opp_Value"),
                                labels = c("SPD_Reg", "SPD_Opp"))

# CDU: Daten für Visualisierung vorbereiten
cdu_top5_viz <- melt(cdu_top5[, c("Topic", "CDU_Reg_Value", "CDU_Opp_Value")], id.vars = "Topic")
if(exists("topic_names")) {
  cdu_top5_viz$Topic_Name <- rep(cdu_top5$Topic_Name, each = 2)
  cdu_top5_viz$Topic <- factor(cdu_top5_viz$Topic_Name)
} else {
  cdu_top5_viz$Topic <- factor(paste("Topic", cdu_top5_viz$Topic))
}
cdu_top5_viz$variable <- factor(cdu_top5_viz$variable, 
                                levels = c("CDU_Reg_Value", "CDU_Opp_Value"),
                                labels = c("CDU_Reg", "CDU_Opp"))

# SPD Plot erstellen
spd_plot <- ggplot(spd_top5_viz, aes(x = Topic, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("SPD_Reg" = "red", "SPD_Opp" = "purple")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Top 3 Topics mit den größten Unterschieden für SPD",
       x = "Topic", y = "Durchschnittlicher Anteil", fill = "Gruppe") +
  geom_text(aes(label = round(value, 4)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3.5)

# CDU Plot erstellen
cdu_plot <- ggplot(cdu_top5_viz, aes(x = Topic, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("CDU_Reg" = "black", "CDU_Opp" = "brown")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Top 3 Topics mit den größten Unterschieden für CDU",
       x = "Topic", y = "Durchschnittlicher Anteil", fill = "Gruppe") +
  geom_text(aes(label = round(value, 4)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3.5)

# Plots anzeigen
print(spd_plot)
print(cdu_plot)

# Plots speichern
ggsave("spd_top5_diff.png", spd_plot, width = 10, height = 6)
ggsave("cdu_top5_diff.png", cdu_plot, width = 10, height = 6)

# Kombinierte Tabelle mit den wichtigsten Metriken erstellen
spd_summary <- data.frame(
  Topic = if(exists("topic_names")) spd_top5$Topic_Name else paste("Topic", spd_top5$Topic),
  SPD_Reg = round(spd_top5$SPD_Reg_Value, 4),
  SPD_Opp = round(spd_top5$SPD_Opp_Value, 4),
  Abs_Diff = round(spd_top5$Diff, 4),
  Rel_Diff_Percent = round(spd_top5$Diff / 
                             pmax(spd_top5$SPD_Reg_Value, spd_top5$SPD_Opp_Value) * 100, 2)
)

cdu_summary <- data.frame(
  Topic = if(exists("topic_names")) cdu_top5$Topic_Name else paste("Topic", cdu_top5$Topic),
  CDU_Reg = round(cdu_top5$CDU_Reg_Value, 4),
  CDU_Opp = round(cdu_top5$CDU_Opp_Value, 4),
  Abs_Diff = round(cdu_top5$Diff, 4),
  Rel_Diff_Percent = round(cdu_top5$Diff / 
                             pmax(cdu_top5$CDU_Reg_Value, cdu_top5$CDU_Opp_Value) * 100, 2)
)

# Tabellen ausgeben
cat("SPD Top 3 Unterschiede:\n")
print(spd_summary)
cat("\nCDU Top 3 Unterschiede:\n")
print(cdu_summary)

# Für schönere Ausgabe in RStudio mit kable
if(require(knitr)) {
  cat("\nSPD Top 3 (formatiert):\n")
  print(knitr::kable(spd_summary, format = "pipe", 
                     caption = "SPD: Top 3 Topics mit größten Unterschieden zwischen Regierung und Opposition"))
  
  cat("\nCDU Top 3 (formatiert):\n")
  print(knitr::kable(cdu_summary, format = "pipe", 
                     caption = "CDU: Top 3 Topics mit größten Unterschieden zwischen Regierung und Opposition"))
}

library(gridExtra)
print(colnames(spd_top5))

spd_table <- tableGrob(spd_top5[, c("Topic_Name", "SPD_Reg_Value", "SPD_Opp_Value", "Diff")])

# Tabelle für CDU erstellen
cdu_table <- tableGrob(cdu_top5[, c("Topic_Name", "CDU_Reg_Value", "CDU_Opp_Value", "Diff")])

# Tabellen als Bilder speichern
library(grid)
png("spd_top5_table.png", width = 800, height = 600)
grid.draw(spd_table)
dev.off()

png("cdu_top5_table.png", width = 800, height = 600)
grid.draw(cdu_table)
dev.off()

##signifikanztests
# Bootstrap-Test für die Unterschiede zwischen SPD Regierung und Opposition
library(boot)

# Funktion, die die Differenz zwischen den mittleren Topic-Anteilen berechnet
diff_topic_means_spd <- function(data, indices) {
  d <- data[indices, ]  # Bootstrap-Stichprobe
  spd_reg_indices <- grep("^SPD_Reg_", rownames(d))
  spd_opp_indices <- grep("^SPD_Opp_", rownames(d))
  
  # Mittlere Topic-Anteile berechnen
  mean_spd_reg <- colMeans(d[spd_reg_indices, ])
  mean_spd_opp <- colMeans(d[spd_opp_indices, ])
  
  # Differenzen berechnen
  diffs <- mean_spd_reg - mean_spd_opp
  
  # Betrag der Differenzen für die Top 5 Topics
  abs_diffs <- abs(diffs)
  sorted_diffs <- sort(abs_diffs, decreasing = TRUE)
  top5_diffs <- sorted_diffs[1:5]
  
  return(top5_diffs)
}

# Bootstrap mit 1000 Wiederholungen durchführen
set.seed(123)  # Für Reproduzierbarkeit
bootstrap_results <- boot(data = theta, statistic = diff_topic_means_spd, R = 1000)

# Konfidenzintervalle für die Top 5 Differenzen
confidence_intervals <- boot.ci(bootstrap_results, type = "perc", index = 1:5)
print(confidence_intervals)

# Bootstrap-Test für die Unterschiede zwischen CDU Regierung und Opposition


# Funktion zur Berechnung der Differenz zwischen CDU in Regierung und Opposition
diff_topic_means_cdu <- function(data, indices) {
  d <- data[indices, ]  # Bootstrap-Stichprobe
  cdu_reg_indices <- grep("^CDU_Reg_", rownames(d))
  cdu_opp_indices <- grep("^CDU_Opp_", rownames(d))
  
  # Mittlere Topic-Anteile berechnen
  mean_cdu_reg <- colMeans(d[cdu_reg_indices, ])
  mean_cdu_opp <- colMeans(d[cdu_opp_indices, ])
  
  # Differenzen berechnen
  diffs <- mean_cdu_reg - mean_cdu_opp
  
  # Betrag der Differenzen für die Top 5 Topics
  abs_diffs <- abs(diffs)
  sorted_indices <- order(abs_diffs, decreasing = TRUE)
  top5_diffs <- abs_diffs[sorted_indices[1:5]]
  
  return(top5_diffs)
}

# Bootstrap mit 1000 Wiederholungen durchführen
set.seed(123)  # Für Reproduzierbarkeit
bootstrap_results_cdu <- boot(data = theta, statistic = diff_topic_means_cdu, R = 1000)

# Konfidenzintervalle für die Top 5 Differenzen
confidence_intervals_cdu <- boot.ci(bootstrap_results_cdu, type = "perc", index = 1:5)
print(confidence_intervals_cdu)
print(bootstrap_results_cdu)

# Funktion, um p-Werte aus Bootstrap-Ergebnissen zu berechnen
calc_p_value <- function(bootstrap_obj, index) {
  # Extrahiere die Bootstrap-Verteilung für das angegebene Topic
  boot_distribution <- bootstrap_obj$t[, index]
  
  # Berechne, wie oft die Bootstrap-Werte ≤ 0 sind (für zweiseitigen Test)
  p_value <- 2 * min(
    mean(boot_distribution <= 0),
    mean(boot_distribution >= 0)
  )
  
  # Begrenze p-Wert auf min. 0.001 für Reporting-Zwecke
  return(max(p_value, 0.001))
}

# SPD: Signifikanzinformationen hinzufügen
spd_summary$Std_Error <- c(
  bootstrap_results$t0[1], 
  bootstrap_results$t0[2], 
  bootstrap_results$t0[3], 
  bootstrap_results$t0[4], 
  bootstrap_results$t0[5]
)

# P-Werte für die Top-5-Topics berechnen
spd_summary$p_value <- sapply(1:5, function(i) calc_p_value(bootstrap_results, i))

# Signifikanz-Sternchen hinzufügen
spd_summary$Signifikanz <- sapply(spd_summary$p_value, function(p) {
  if (p < 0.001) return("***")
  else if (p < 0.01) return("**") 
  else if (p < 0.05) return("*")
  else return("n.s.")
})

# CDU: Identisches Vorgehen für CDU-Daten
cdu_summary$Std_Error <- c(
  bootstrap_results_cdu$t0[1], 
  bootstrap_results_cdu$t0[2], 
  bootstrap_results_cdu$t0[3], 
  bootstrap_results_cdu$t0[4], 
  bootstrap_results_cdu$t0[5]
)

cdu_summary$p_value <- sapply(1:5, function(i) calc_p_value(bootstrap_results_cdu, i))

cdu_summary$Signifikanz <- sapply(cdu_summary$p_value, function(p) {
  if (p < 0.001) return("***")
  else if (p < 0.01) return("**") 
  else if (p < 0.05) return("*")
  else return("n.s.")
})

# Tabellen formatieren und anzeigen
if(require(knitr)) {
  # SPD-Tabelle mit Signifikanzinformationen
  spd_table <- knitr::kable(spd_summary, format = "html", 
                            caption = "SPD: Top 5 Topics mit größten Unterschieden zwischen Regierung und Opposition",
                            digits = c(0, 4, 4, 4, 2, 4, 3, 0))
  
  # CDU-Tabelle mit Signifikanzinformationen
  cdu_table <- knitr::kable(cdu_summary, format = "html", 
                            caption = "CDU: Top 5 Topics mit größten Unterschieden zwischen Regierung und Opposition",
                            digits = c(0, 4, 4, 4, 2, 4, 3, 0))
  
  # Tabellen anzeigen
  cat("SPD Top 5 mit Signifikanz:\n")
  print(spd_table)
  
  cat("\nCDU Top 5 mit Signifikanz:\n")
  print(cdu_table)
  
  # Als Bilder speichern mit gridExtra
  library(gridExtra)
  library(grid)
  
  # SPD-Tabelle als Bild
  png("spd_top5_table_signifikanz.png", width = 900, height = 500)
  grid.table(spd_summary, rows = NULL)
  dev.off()
  
  # CDU-Tabelle als Bild
  png("cdu_top5_table_signifikanz.png", width = 900, height = 500)
  grid.table(cdu_summary, rows = NULL)
  dev.off()
}

#json
########implementierung funktioniert nicht mit gruppierungen
# Gruppenzuordnung für alle Dokumente erstellen
doc_groups <- character(length = nrow(theta))
doc_groups[grep("^SPD_Reg_", rownames(theta))] <- "SPD_Reg"
doc_groups[grep("^SPD_Opp_", rownames(theta))] <- "SPD_Opp"
doc_groups[grep("^CDU_Reg_", rownames(theta))] <- "CDU_Reg"
doc_groups[grep("^CDU_Opp_", rownames(theta))] <- "CDU_Opp"

doc_colors <- color_mapping[doc_groups]

# 4. Metadaten-DataFrame erstellen
metadata_df <- data.frame(
  doc_id = rownames(theta),
  group = doc_groups,
  color = doc_colors,
  stringsAsFactors = FALSE
)


json_group <- createJSON(
  phi = beta,                              # Topic-Term-Matrix
  theta = theta,                           # Dokument-Topic-Matrix
  doc.length = rowSums(common_dtm_filtered), # Dokumentlängen
  vocab = colnames(common_dtm_filtered),    # Vokabular
  term.frequency = colSums(common_dtm_filtered), # Term-Frequenzen
  topic.labels = topic_names,              # Topic-Namen explizit setzen
  mds.method = svd_tsne,                   # Dimensionsreduktion
  plot.opts = list(
    xlab = "",
    ylab = "",
    colors = as.character(doc_colors)      # Farben basierend auf Gruppen
  )
)

# Visualisierung anzeigen im Browser
serVis(json_group)

# Visualisierung speichern für spätere Nutzung
serVis(json_group, out.dir = "ldavis_group_output", open.browser = TRUE)

servr::httd("ldavis_output")

############# topic over time############
#########################################
spd_reg_perioden <- c("8", "15") # Plus Teil von 9
spd_opp_perioden <- c("10", "11", "12", "13", "17") # Plus Teil von 9
cdu_reg_perioden <- c("10", "11", "12", "13", "17") # Plus Teil von 9
cdu_opp_perioden <- c("8", "15") # Plus Teil von 9



library(reshape2)

# Funktion zum Extrahieren der Wahlperiode aus der Dokument-ID
extract_electoral_term <- function(doc_id) {
  parts <- strsplit(doc_id, "_")[[1]]
  if (length(parts) >= 3) {
    return(parts[3])
  } else {
    return(NA)
  }
}


# Theta-Matrix in DataFrame umwandeln und Gruppenzuordnung hinzufügen
theta_df <- as.data.frame(theta)
theta_df$doc_id <- rownames(theta)

#  Wahlperiode extrahieren und als Faktor festlegen
theta_df$electoral_term <- sapply(theta_df$doc_id, extract_electoral_term)
theta_df$electoral_term <- factor(theta_df$electoral_term, 
                                  levels = c("8", "9", "10", "11", "12", "13", "15", "17"))
theta_df$electoral_term <- as.numeric(as.character(theta_df$electoral_term))
theta_df_sorted <- theta_df %>%
  arrange(electoral_term)

# Überprüfen der Sortierung
print(theta_df_sorted)

# Historisch korrekte Gruppenzuordnung hinzufügen
theta_df$corrected_group <- NA_character_  # Initialisieren mit NA

# SPD in Regierung
spd_reg_idx <- which(grepl("^SPD", theta_df$doc_id) & theta_df$electoral_term %in% c("8", "15"))
theta_df$corrected_group[spd_reg_idx] <- "SPD_Reg"

# SPD in Opposition
spd_opp_idx <- which(grepl("^SPD", theta_df$doc_id) & theta_df$electoral_term %in% c("10", "11", "12", "13", "17"))
theta_df$corrected_group[spd_opp_idx] <- "SPD_Opp"

# CDU in Regierung
cdu_reg_idx <- which(grepl("^CDU", theta_df$doc_id) & theta_df$electoral_term %in% c("10", "11", "12", "13", "17"))
theta_df$corrected_group[cdu_reg_idx] <- "CDU_Reg"

# CDU in Opposition
cdu_opp_idx <- which(grepl("^CDU", theta_df$doc_id) & theta_df$electoral_term %in% c("8", "15"))
theta_df$corrected_group[cdu_opp_idx] <- "CDU_Opp"

print(unique(theta_df[c("corrected_group", "electoral_term")]))



####heatmap
# Durchschnittliche Topic-Anteile pro Wahlperiode und korrigierter Gruppe berechnen
topic_props_by_term_korr <- theta_df %>%
  filter(!is.na(electoral_term), !is.na(corrected_group)) %>%
  group_by(electoral_term, corrected_group) %>%
  summarise(across(1:ncol(theta), \(x) mean(x, na.rm = TRUE)), .groups = "drop")

# Daten für Visualisierung umformen
library(reshape2)
viz_data_korr <- melt(as.data.frame(topic_props_by_term_korr),
                      id.vars = c("electoral_term", "corrected_group"),
                      variable.name = "Topic",
                      value.name = "Proportion")
heatmap_data <- topic_props_by_term_korr %>%
  mutate(electoral_term = as.numeric(as.character(electoral_term))) %>%
  arrange(electoral_term) %>%
  pivot_longer(cols = -c(electoral_term, corrected_group), 
               names_to = "Topic", 
               values_to = "Proportion")

heatmap_data <- heatmap_data %>%
  mutate(electoral_term = as.numeric(as.character(electoral_term))) %>%
  arrange(electoral_term, corrected_group)

# Topic-Namen zuweisen (falls noch nicht geschehen)
if (exists("topic_names") && length(topic_names) == length(unique(heatmap_data$Topic))) {
  heatmap_data$Topic <- factor(heatmap_data$Topic, 
                               levels = unique(heatmap_data$Topic), 
                               labels = topic_names)
} else {
  stop("Die Anzahl der Topic-Namen stimmt nicht mit den Topics überein.")
}

# Heatmap erstellen
library(ggplot2)


# Berechnung der Grenzen zwischen den Gruppen
group_boundaries <- heatmap_data %>%
  mutate(group_order = as.numeric(factor(corrected_group))) %>%
  group_by(corrected_group) %>%
  summarise(boundary = max(group_order)) %>%
  pull(boundary)

# Grenzen anpassen für ggplot (y-Achse)
hline_positions <- cumsum(table(heatmap_data$corrected_group)) + 0.5

heatmap_plot <- ggplot(heatmap_data, aes(x = Topic, y = paste(corrected_group, electoral_term), fill = Proportion)) +
  geom_tile(color = "black", linewidth = 0.5) + # Kachelränder hinzufügen
  geom_hline(yintercept = hline_positions, color = "red", linewidth = 0.8) + 
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    panel.grid.major = element_blank(), # Entferne ggplot-Standardgitter
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Heatmap der Topic-Verteilung nach Gruppe und Wahlperiode",
    x = "Topics",
    y = "Gruppe und Wahlperiode",
    fill = "Anteil"
  )



ggsave("topic_heatmap_corrected.png", heatmap_plot, width = 14, height = 10, dpi = 300)



######gestapeltes Flächendiagramm
#################################
library(reshape2)
library(ggplot2)
library(dplyr)



# Topic-Proportionen pro Gruppe und Wahlperiode berechnen



# Topic-Namen zuweisen
if (exists("topic_names") && length(topic_names) == ncol(theta)) {
  levels(viz_data_korr$Topic) <- topic_names
}



# Gestapeltes Flächendiagramm für jede Gruppe erstellen
ggplot(viz_data_korr, aes(x = electoral_term, y = Proportion, fill = Topic)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~ corrected_group) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  labs(title = "Topic-Proportionen nach Wahlperiode und Gruppe",
       x = "Wahlperiode", y = "Proportionaler Anteil", fill = "Topic") +
  guides(fill = guide_legend(ncol = 3))

# Als PNG speichern
ggsave("topic_proportions_by_group_period.png", width = 14, height = 10, dpi = 300)




