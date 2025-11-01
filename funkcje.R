library(data.table)

odciecie <- function(df, prog = NULL) {
  
  srednie_or <- df[["Średnia"]]  
  df_punkty <- df[, !c("Nr", "Uczestnik", "Średnia"), with = FALSE]
  jurorzy <- copy(colnames(df_punkty))
  
  df_punkty[, srednia := rowMeans(.SD, na.rm = TRUE)]
  df_punkty_kor_lista <- lapply(split(df_punkty, seq_len(NROW(df_punkty))), function(x) {
    # srednia <- round(x[["srednia"]], 2)
    srednia <- x[["srednia"]]
    x <- as.vector(t(x))[-length(x)]
    if (!is.null(prog)) {
      x <- ifelse(x > srednia + prog, srednia + prog, x)
      x <- ifelse(x < srednia - prog, srednia - prog, x)
    }
    as.data.table(t(x))
  })
  df_punkty_kor <- rbindlist(df_punkty_kor_lista)
  setnames(df_punkty_kor, jurorzy)
  # df_punkty_kor[, Średnia := round(rowMeans(.SD, na.rm = TRUE), 2)]
  df_punkty_kor[, Średnia := rowMeans(.SD, na.rm = TRUE)]
  df_punkty_kor
  cbind(df[, c("Nr", "Uczestnik")], df_punkty_kor)
}

policz_konkurs <- function(etap_1,
                           etap_2,
                           etap_3,
                           etap_4,
                           progi,
                           wagi) {
  
  etap_1 <- odciecie(etap_1, prog = progi[1])[order(get("Uczestnik")), ]
  etap_2 <- odciecie(etap_2, prog = progi[2])[order(get("Uczestnik")), ]
  etap_3 <- odciecie(etap_3, prog = progi[3])[order(get("Uczestnik")), ]
  etap_4 <- odciecie(etap_4, prog = progi[4])[order(get("Uczestnik")), ]
  
  finalisci <- etap_4[["Uczestnik"]]
  
  etap_1 <- etap_1[Uczestnik %in% finalisci, ]
  etap_2 <- etap_2[Uczestnik %in% finalisci, ]
  etap_3 <- etap_3[Uczestnik %in% finalisci, ]
  Średnia <- wagi[1] * etap_1[["Średnia"]] + wagi[2] * etap_2[["Średnia"]] +
    wagi[3] * etap_3[["Średnia"]] + wagi[4] * etap_4[["Średnia"]]
  
  cbind(etap_4[, c("Nr", "Uczestnik")], data.table(Średnia))[order(-get("Średnia")), ]
  
}
