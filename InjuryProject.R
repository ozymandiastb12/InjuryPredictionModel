# =============================== NYG Availability — Full Visual Pack (Weekly $) ===============================
# Outputs: ./visuals/*.png and ./artifacts/*.csv
# ============================================================================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(janitor)
  library(ggplot2); library(pROC); library(ranger)
})

`%||%` <- function(a,b) if (!is.null(a)) a else b
set.seed(42)
dir.create("visuals",  showWarnings = FALSE)
dir.create("artifacts", showWarnings = FALSE)

# ---------- Load leakage-free modeling frame ----------
dfm <- read_csv("availability_model_frame_clean.csv", show_col_types = FALSE) %>% clean_names()
stopifnot(all(c("season","week","player_full","position","did_play") %in% names(dfm)))

if (!"cap_hit" %in% names(dfm)) {
  warning("cap_hit column not found — business $ metrics will be NA.")
  dfm$cap_hit <- NA_real_
}

# ---------- Features ----------
meta_cols <- c("season","week","player_full","position","did_play")
prob_cols <- grep("^prob_", names(dfm), value = TRUE)
feat <- setdiff(names(dfm), c(meta_cols, prob_cols))
feat <- feat[vapply(dfm[feat], is.numeric, logical(1))]
if (length(feat) < 10) stop("Too few features detected; check your frame.")

make_form <- function(target, features, with_pos = TRUE) {
  rhs <- paste(features, collapse = " + ")
  if (with_pos) rhs <- paste(rhs, "+ position")
  as.formula(paste(target, "~", rhs))
}
auc_safe <- function(y, p) if (length(unique(y)) < 2) NA_real_ else as.numeric(pROC::auc(pROC::roc(y, p, quiet=TRUE, direction="<")))
platt_calibrate <- function(train_prob, train_y, test_prob) {
  eps <- 1e-6
  lp_tr <- qlogis(pmin(pmax(train_prob, eps), 1 - eps))
  fit   <- glm(train_y ~ lp_tr, family = binomial)
  lp_te <- qlogis(pmin(pmax(test_prob,  eps), 1 - eps))
  as.numeric(plogis(coef(fit)[1] + coef(fit)[2] * lp_te))
}
reliability_tbl <- function(y, p, bins = 10) {
  brks <- unique(quantile(p, probs = seq(0,1,length.out=bins+1), na.rm=TRUE, type=8))
  if (length(brks) < 2) brks <- unique(sort(c(0, p, 1)))
  cutp <- cut(p, breaks = brks, include.lowest = TRUE, right = TRUE)
  tibble::tibble(y=y, p=p, bin=cutp) |>
    group_by(bin, .drop = FALSE) |>
    summarise(n=n(), pred_mean=mean(p, na.rm=TRUE), obs_rate=mean(y, na.rm=TRUE), .groups="drop") |>
    mutate(bin = as.character(bin))
}

# =================== Rolling-origin CV (2023) ===================
df23 <- dfm %>% filter(season == 2023)
weeks_eval <- sort(unique(df23$week))
weeks_eval <- weeks_eval[weeks_eval >= 3]  # ensure lags exist

cv_rows <- vector("list", length(weeks_eval))
for (i in seq_along(weeks_eval)) {
  w <- weeks_eval[i]
  train_w <- df23 %>% filter(week < w)
  test_w  <- df23 %>% filter(week == w)
  
  pos_rate <- mean(train_w$did_play == 1)
  w_pos <- ifelse(is.finite(pos_rate) && pos_rate > 0 && pos_rate < 1, (1 - pos_rate) / pos_rate, 1)
  w_glm <- ifelse(train_w$did_play == 1, w_pos, 1)
  
  # GLM
  fit_glm  <- glm(make_form("did_play", feat, TRUE), data=train_w, family=binomial, weights=w_glm)
  p_glm    <- as.numeric(predict(fit_glm, newdata=test_w, type="response"))
  auc_glm  <- auc_safe(test_w$did_play, p_glm)
  
  # RF
  train_rf <- train_w %>% mutate(did_play_f = factor(did_play, levels=c(0,1))) %>%
    select(-season, -week, -player_full, -did_play)
  cw <- c(`0`=1, `1`=w_pos); cw <- cw[levels(train_rf$did_play_f)]
  fit_rf  <- ranger(make_form("did_play_f", feat, TRUE), data=train_rf, probability=TRUE,
                    num.trees=600, mtry=floor(sqrt(length(feat)+1L)), min.node.size=25,
                    class.weights=cw, seed=42)
  p_rf <- as.numeric(predict(fit_rf, data=test_w %>% select(-season,-week,-player_full))$predictions[, "1"])
  auc_rf <- auc_safe(test_w$did_play, p_rf)
  
  cv_rows[[i]] <- tibble::tibble(week=w, n=nrow(test_w), auc_glm=auc_glm, auc_rf=auc_rf)
}
cv_tbl <- bind_rows(cv_rows)
write_csv(cv_tbl, "artifacts/cv_rolling_2023_weekly_auc.csv")

ggsave("visuals/cv_rolling_auc_2023.png",
       ggplot(cv_tbl, aes(week)) +
         geom_line(aes(y=auc_glm, color="GLM")) + geom_point(aes(y=auc_glm, color="GLM")) +
         geom_line(aes(y=auc_rf,  color="RF"))  + geom_point(aes(y=auc_rf,  color="RF")) +
         scale_color_manual(values=c("GLM"="black","RF"="red")) +
         labs(title="Rolling-Origin AUC by Week (2023 CV)", x="Week", y="AUC", color="Model") +
         theme_minimal(),
       width=8, height=5, dpi=140
)

# =================== Train(2023) → Test(2024) + Calibration ===================
train <- dfm %>% filter(season == 2023)
test  <- dfm %>% filter(season == 2024)

pos_rate <- mean(train$did_play == 1)
w_pos <- ifelse(is.finite(pos_rate) && pos_rate > 0 && pos_rate < 1, (1 - pos_rate) / pos_rate, 1)
w_glm <- ifelse(train$did_play == 1, w_pos, 1)

# GLM
fit_glm  <- glm(make_form("did_play", feat, TRUE), data=train, family=binomial, weights=w_glm)
test$prob_glm <- as.numeric(predict(fit_glm, newdata=test, type="response"))

# RF
train_rf <- train %>% mutate(did_play_f = factor(did_play, levels=c(0,1))) %>%
  select(-season, -week, -player_full, -did_play)
cw <- c(`0`=1, `1`=w_pos); cw <- cw[levels(train_rf$did_play_f)]
fit_rf <- ranger(make_form("did_play_f", feat, TRUE), data=train_rf, probability=TRUE,
                 num.trees=600, mtry=floor(sqrt(length(feat)+1L)), min.node.size=25,
                 class.weights=cw, seed=42)
test$prob_rf <- as.numeric(predict(fit_rf, data=test %>% select(-season,-week,-player_full))$predictions[, "1"])

# Calibration
train$prob_glm <- as.numeric(predict(fit_glm, newdata=train, type="response"))
train$prob_rf  <- as.numeric(predict(fit_rf,  data=train %>% select(-season,-week,-player_full))$predictions[, "1"])
test$prob_glm_cal <- platt_calibrate(train$prob_glm, train$did_play, test$prob_glm)
test$prob_rf_cal  <- platt_calibrate(train$prob_rf,  train$did_play, test$prob_rf)

# =================== Build a clean BUSINESS table (always has position + cap) ===================
biz <- dfm %>% filter(season == 2024) %>%
  select(season, week, player_full, position, cap_hit, did_play) %>%
  left_join(
    test %>% select(week, player_full, prob_glm, prob_glm_cal, prob_rf, prob_rf_cal),
    by = c("week","player_full")
  )

# Hard checks
if (!"cap_hit" %in% names(biz)) stop("cap_hit missing from business table after join.")
if (!"position" %in% names(biz)) stop("position missing from business table after join.")

# Weekly cap normalization
n_weeks_2024 <- biz %>% summarise(n_weeks = max(week, na.rm=TRUE)) %>% pull(n_weeks)
if (!is.finite(n_weeks_2024) || n_weeks_2024 <= 0) n_weeks_2024 <- 18L
biz <- biz %>% mutate(cap_hit_weekly = cap_hit / n_weeks_2024)

# =================== Performance Curves ===================
roc_glm <- pROC::roc(biz$did_play, biz$prob_glm_cal, quiet = TRUE, direction = "<")
roc_rf  <- pROC::roc(biz$did_play, biz$prob_rf_cal,  quiet = TRUE, direction = "<")

png("visuals/roc_calibrated_2024.png", 900, 600)
plot(roc_glm, main = sprintf("ROC (2024, calibrated) — GLM=%.3f, RF=%.3f",
                             as.numeric(pROC::auc(roc_glm)), as.numeric(pROC::auc(roc_rf))))
plot(roc_rf, add=TRUE, col="red"); legend("bottomright", c("GLM","RF"), col=c("black","red"), lwd=2, bty="n")
dev.off()

if (!requireNamespace("PRROC", quietly = TRUE)) install.packages("PRROC")
library(PRROC)
pr_glm <- PRROC::pr.curve(scores.class0 = biz$prob_glm_cal[biz$did_play==1],
                          scores.class1 = biz$prob_glm_cal[biz$did_play==0], curve = TRUE)
pr_rf  <- PRROC::pr.curve(scores.class0 = biz$prob_rf_cal[biz$did_play==1],
                          scores.class1 = biz$prob_rf_cal[biz$did_play==0], curve = TRUE)
png("visuals/pr_calibrated_2024.png", 900, 600)
plot(pr_glm, main = sprintf("PR Curve (2024, calibrated) — GLM AUPRC=%.3f, RF AUPRC=%.3f",
                            pr_glm$auc.integral, pr_rf$auc.integral))
lines(pr_rf$curve[,1], pr_rf$curve[,2], col="red", lwd=2)
legend("bottomright", c("GLM","RF"), col=c("black","red"), lwd=2, bty="n")
dev.off()

# Reliability plots
rel_plot <- function(df_p, title, out_png) {
  rel <- reliability_tbl(df_p$did_play, df_p$prob)
  p <- ggplot(rel, aes(pred_mean, obs_rate)) +
    geom_abline(slope=1, intercept=0, linetype=2) +
    geom_point(aes(size=n)) + geom_line() +
    coord_cartesian(xlim=c(0,1), ylim=c(0,1)) +
    labs(title = title, x="Predicted probability (bin mean)", y="Observed rate") +
    theme_minimal()
  ggsave(out_png, p, width=7.5, height=5, dpi=140)
}
rel_plot(biz %>% transmute(did_play, prob = prob_glm),      "Reliability — GLM (uncalibrated, 2024)", "visuals/reliability_glm_2024.png")
rel_plot(biz %>% transmute(did_play, prob = prob_glm_cal),  "Reliability — GLM (Platt-calibrated, 2024)", "visuals/reliability_glm_cal_2024.png")
rel_plot(biz %>% transmute(did_play, prob = prob_rf),       "Reliability — RF (uncalibrated, 2024)", "visuals/reliability_rf_2024.png")
rel_plot(biz %>% transmute(did_play, prob = prob_rf_cal),   "Reliability — RF (Platt-calibrated, 2024)", "visuals/reliability_rf_cal_2024.png")

# =================== Business Visuals (WEEKLY $) ===================

# 1) Cap-$ at risk by position
cap_by_pos <- biz %>%
  mutate(cap_risk = (1 - prob_rf_cal) * cap_hit_weekly) %>%
  group_by(position) %>%
  summarise(cap_at_risk = sum(cap_risk, na.rm = TRUE),
            players = n(), .groups = "drop") %>%
  arrange(desc(cap_at_risk))
write_csv(cap_by_pos, "artifacts/cap_at_risk_by_position_2024.csv")

ggsave("visuals/cap_at_risk_by_position_2024.png",
       ggplot(cap_by_pos, aes(x=reorder(position, cap_at_risk), y=cap_at_risk/1e6)) +
         geom_col(fill="grey30") + coord_flip() +
         labs(title="2024 Expected Cap-$ at Risk by Position (RF calibrated, weekly)",
              x="Position", y="Cap-$ at risk (Millions)") +
         theme_minimal(),
       width=8, height=5, dpi=140
)

# 2) Top 15 players by expected weekly cap-$ at risk
top_players <- biz %>%
  transmute(season=2024, week, player_full, position,
            cap_hit_weekly, prob_play = prob_rf_cal,
            cap_risk = (1 - prob_rf_cal) * cap_hit_weekly) %>%
  arrange(desc(cap_risk)) %>% slice_head(n=15)
write_csv(top_players, "artifacts/top15_cap_risk_players_2024.csv")

ggsave("visuals/top15_cap_risk_players_2024.png",
       ggplot(top_players %>% arrange(cap_risk),
              aes(x=reorder(paste0(player_full, " (", position, ")"), cap_risk),
                  y=cap_risk/1e6)) +
         geom_col(fill="grey30") + coord_flip() +
         labs(title="Top 15 Expected Cap-$ at Risk — 2024 (RF calibrated, weekly)",
              x="Player (Position)", y="Cap-$ at risk (Millions, weekly)") +
         theme_minimal(),
       width=9, height=6, dpi=140
)

# 3) Weekly team cap-$ at risk
weekly_risk <- biz %>%
  group_by(week) %>%
  summarise(cap_at_risk = sum((1 - prob_rf_cal) * cap_hit_weekly, na.rm = TRUE), .groups = "drop")
write_csv(weekly_risk, "artifacts/weekly_cap_risk_2024.csv")

ggsave("visuals/weekly_cap_risk_2024.png",
       ggplot(weekly_risk, aes(week, cap_at_risk/1e6)) +
         geom_line() + geom_point() +
         labs(title="Weekly Team Cap-$ at Risk (2024, RF calibrated, weekly)",
              x="Week", y="Cap-$ at risk (Millions)") +
         theme_minimal(),
       width=8, height=4.5, dpi=140
)

# 4) Threshold sweep (GLM/RF calibrated) — operations knobs (weekly $)
if (!requireNamespace("purrr", quietly = TRUE)) install.packages("purrr")
library(purrr)
sweep_thresholds <- function(y, p, cap_weekly, label) {
  thr <- seq(0.05, 0.95, by = 0.01)
  map_dfr(thr, function(t0) {
    pred <- as.integer(p >= t0)
    tp <- sum(pred==1 & y==1); fp <- sum(pred==1 & y==0)
    tn <- sum(pred==0 & y==0); fn <- sum(pred==0 & y==1)
    tibble::tibble(
      model = label, threshold = t0,
      acc  = mean(pred == y),
      sens = ifelse((tp+fn)>0, tp/(tp+fn), NA_real_),
      spec = ifelse((tn+fp)>0, tn/(tn+fp), NA_real_),
      cap_flagged_out = sum(cap_weekly[pred==0], na.rm=TRUE),
      cap_risk_total  = sum((1 - p) * cap_weekly, na.rm=TRUE)
    )
  })
}
thr_tbl <- bind_rows(
  sweep_thresholds(biz$did_play, biz$prob_glm_cal, biz$cap_hit_weekly, "GLM"),
  sweep_thresholds(biz$did_play, biz$prob_rf_cal,  biz$cap_hit_weekly, "RF")
)
write_csv(thr_tbl, "artifacts/threshold_sweep_metrics_2024.csv")

ggsave("visuals/threshold_sensitivity_2024.png",
       ggplot(thr_tbl, aes(threshold, sens, color=model)) +
         geom_line() + labs(title="Sensitivity vs Threshold (2024)", y="Sensitivity", x="Threshold") +
         theme_minimal(),
       width=7.5, height=5, dpi=140
)
ggsave("visuals/threshold_specificity_2024.png",
       ggplot(thr_tbl, aes(threshold, spec, color=model)) +
         geom_line() + labs(title="Specificity vs Threshold (2024)", y="Specificity", x="Threshold") +
         theme_minimal(),
       width=7.5, height=5, dpi=140
)
ggsave("visuals/threshold_cap_flagged_out_2024.png",
       ggplot(thr_tbl, aes(threshold, cap_flagged_out/1e6, color=model)) +
         geom_line() +
         labs(title="Weekly Cap-$ Labeled OUT vs Threshold (2024)",
              y="Cap-$ labeled OUT (Millions, weekly)", x="Threshold") +
         theme_minimal(),
       width=7.5, height=5, dpi=140
)

# 5) By-position AUC (calibrated)
bypos_auc <- biz %>%
  group_by(position) %>%
  summarise(
    n = n(),
    auc_glm = auc_safe(did_play, prob_glm_cal),
    auc_rf  = auc_safe(did_play, prob_rf_cal),
    .groups = "drop"
  )
write_csv(bypos_auc, "artifacts/auc_by_position_2024.csv")

ggsave("visuals/auc_by_position_2024_bar.png",
       ggplot(bypos_auc, aes(x=reorder(position, auc_rf), y=auc_rf)) +
         geom_col() + coord_flip() +
         labs(title="AUC by Position — 2024 (RF calibrated)", x="Position", y="AUC") +
         theme_minimal(),
       width=6.5, height=4.5, dpi=140
)

# 6) Prediction dump (for TA)
preds_2024 <- biz %>%
  select(season, week, player_full, position, did_play, cap_hit, cap_hit_weekly,
         prob_glm, prob_glm_cal, prob_rf, prob_rf_cal)
write_csv(preds_2024, "artifacts/availability_predictions_2024_with_calibration.csv")

cat("\n=== DONE ===\nArtifacts (CSV): ./artifacts\nCharts (PNG):    ./visuals\n")
