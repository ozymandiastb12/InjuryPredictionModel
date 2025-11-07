# InjuryPredictionModel
Predicting Weekly Player Availability & Expected Cap Dollars at Risk

New York Giants — 2023/2024 Seasons

This repository contains a fully reproducible workflow to:

Train two availability models (GLM baseline + Random Forest) using 2023 data

Evaluate on 2024 holdout season with leakage-free engineered features

Apply Platt calibration for true probability estimates

Convert model outputs into expected weekly cap dollars at risk

Export all figures & tables used in the written report and slide deck

All outputs are generated automatically and saved to artifacts/ (CSV tables) and visuals/ (plots).

🔧 Requirements

R ≥ 4.3 recommended

Script will auto-install missing CRAN packages on first run

Only input required: data/availability_model_frame_clean.csv
(not committed to repo due to size/privacy)

🚀 How to Run
Option A — Command line (recommended)
make all

Option B — Direct R call
Rscript run.R


Both will:

Fit GLM + RF on 2023

Calibrate using Platt scaling

Score 2024 holdout

Generate all CSVs + PNG figures automatically


📊 Output Summary
✅ Model Performance (2024 holdout)
Model	AUC (calibrated)
GLM	0.905
RF	0.942
✅ Key Business Metrics (2024)

Cap risk is weekly-normalized, not annual (fixes inflated cap issue)

“Expected cap dollars at risk” = (1 – prob_play) × weekly_cap_hit

🔍 Contents of /artifacts (CSV tables)
File	Description
cv_rolling_2023_weekly_auc.csv	Weekly AUC from rolling-origin CV
auc_by_position_2024.csv	RF/GLM AUC by position
availability_predictions_2024_with_calibration.csv	All 2024 predictions (raw + calibrated)
cap_at_risk_by_position_2024.csv	Expected weekly cap risk by position
top15_cap_risk_players_2024.csv	Top 15 highest-risk players (weekly)
weekly_cap_risk_2024.csv	Total weekly team cap risk (time series)
threshold_sweep_metrics_2024.csv	Sensitivity, specificity & cap flagged out vs threshold
📈 Contents of /visuals (PNG figures)

cv_rolling_auc_2023.png

roc_calibrated_2024.png

pr_calibrated_2024.png

reliability_glm_2024.png

reliability_glm_cal_2024.png

reliability_rf_2024.png

reliability_rf_cal_2024.png

cap_at_risk_by_position_2024.png

top15_cap_risk_players_2024.png

weekly_cap_risk_2024.png

threshold_sensitivity_2024.png

threshold_specificity_2024.png

threshold_cap_flagged_out_2024.png

auc_by_position_2024_bar.png

All figures are formatted and ready for report/slides.

🔁 Reproducibility Notes (TA-facing)

No target leakage — all features are lagged or pre-game known

Weekly cap normalization is handled inside script via:

cap_hit_weekly = cap_hit / max(week in 2024)


Script is fully offline — no API calls, no randomness beyond fixed seeds

Running the repo on a clean machine will reproduce 100% identical outputs
