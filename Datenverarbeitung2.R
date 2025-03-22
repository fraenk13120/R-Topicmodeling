load("Datenverarbeitung_var.RData")

library(udpipe)

# Deutsches Sprachmodell herunterladen
ud_model_file <- "german-gsd-ud-2.5-191206.udpipe"
if(!file.exists(ud_model_file)) {
  udpipe_download_model(language = "german", 
                        model_dir = ".")
}
ud_model <- udpipe_load_model(file = ud_model_file)

str(spd_reg, list.len = 20)
colnames(spd_reg)

# Funktion zur Extraktion von Substantiven und Lemmatisierung
extract_nouns <- function(df, text_column = "speech_content", sample_size = NULL) {
  # 1. Überprüfen, ob die Textspalte überhaupt existiert
  if (!text_column %in% colnames(df)) {
    stop(paste("Spalte", text_column, "existiert nicht im Dataframe!"))
  }
  
  # 2. Spaltentyp prüfen und sicherstellen, dass sie nicht leer ist
  print(paste("Spaltentyp von", text_column, ":", class(df[[text_column]])))
  print(paste("Anzahl der NA-Werte:", sum(is.na(df[[text_column]]))))
  
  # 3. Leere Datensatz-Prüfung
  if (nrow(df) == 0) {
    stop("Dataframe hat keine Zeilen!")
  }
  
  # 4. Text in character umwandeln und NA-Werte durch leere Strings ersetzen
  df[[text_column]] <- as.character(df[[text_column]])
  df[[text_column]][is.na(df[[text_column]])] <- ""  # NA durch leeren String ersetzen[1][3]
  
  # 5. Dokument-IDs erstellen bevor die Stichprobe gezogen wird
  df$doc_id <- as.character(seq_len(nrow(df)))
  
  # 6. Stichprobe nehmen
  if (!is.null(sample_size) && nrow(df) > sample_size) {
    set.seed(123)
    df <- df %>% sample_n(sample_size)
    print(paste("Stichprobe von", sample_size, "Dokumenten gezogen"))
  }
  
  # 7. Udpipe-Verarbeitung
  print("Starte UDPipe-Annotation...")
  annotations <- udpipe_annotate(
    ud_model,
    x = df[[text_column]],
    doc_id = df$doc_id
  )
  
  annotations_df <- as.data.frame(annotations)
  print(paste("Annotation abgeschlossen. Gefundene Wörter:", nrow(annotations_df)))
  
  # 8. Substantive extrahieren
  nouns_df <- annotations_df %>%
    filter(upos == "NOUN") %>%
    select(doc_id, lemma)
  
  
  # 9. Gruppieren nach Dokument-ID
  processed_texts <- nouns_df %>%
    group_by(doc_id) %>%
    summarise(processed_text = paste(lemma, collapse = " "))
  
  # Typ-Debugging
  cat("Typ der doc_id in annotations_df:", class(annotations_df$doc_id), "\n")
  df$doc_id <- as.character(df$doc_id)  # Nochmals sicherstellen
  processed_texts$doc_id <- as.character(processed_texts$doc_id)
  
  # 10. Mit ursprünglichen Daten zusammenführen
  result <- df %>%
    left_join(processed_texts, by = "doc_id") %>%
    filter(!is.na(processed_text))  # Dokumente ohne Substantive entfernen
  
  print(paste("Verarbeitung abgeschlossen. Ergebnisse:", nrow(result), "Dokumente"))
  return(result)
}



# Stichprobengröße 
sample_size <- 5000

# Texte verarbeiten 
process_with_log <- function(data, name, text_column = "speech_content") {
  cat("Verarbeite", name, "...\n")
  cat("Dataframe hat", nrow(data), "Zeilen\n")
  
  # Prüfen der Spalte
  if (!text_column %in% colnames(data)) {
    stop(paste("Spalte", text_column, "existiert nicht in", name))
  }
  
  # Stichprobe zum Testen der Verarbeitung
  tryCatch({
    result <- extract_nouns(data, text_column = text_column, sample_size = sample_size)
    cat(nrow(result), "verarbeitete Dokumente erhalten\n")
    return(result)
  }, error = function(e) {
    cat("FEHLER bei", name, ":", e$message, "\n")
    return(NULL)
  })
}

spd_reg_processed <- process_with_log(spd_reg, "SPD Regierung")
spd_opp_processed <- process_with_log(spd_opp, "SPD Opposition")
cdu_reg_processed <- process_with_log(cdu_reg, "CDU Regierung")
cdu_opp_processed <- process_with_log(cdu_opp, "CDU Opposition")

# Diagnostik
check_doc_id <- function(processed_data) {
  cat("Überprüfung für", deparse(substitute(processed_data)), ":\n")
  cat("- doc_id vorhanden?", "doc_id" %in% colnames(processed_data), "\n")
  cat("- Fehlende doc_id:", sum(is.na(processed_data$doc_id)), "\n")
  cat("- Leere doc_id:", sum(processed_data$doc_id == ""), "\n")
}

# Aufruf für alle Datensätze
check_doc_id(spd_reg_processed)
check_doc_id(spd_opp_processed)
check_doc_id(cdu_reg_processed)
check_doc_id(cdu_opp_processed)

# Zwischenspeichern der verarbeiteten Daten
saveRDS(spd_reg_processed, "spd_reg_processed.rds")
saveRDS(spd_opp_processed, "spd_opp_processed.rds")
saveRDS(cdu_reg_processed, "cdu_reg_processed.rds")
saveRDS(cdu_opp_processed, "cdu_opp_processed.rds")


