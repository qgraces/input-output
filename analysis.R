# Spanish input study: full analysis pipeline (all models, tables and figures)
# Coding: two-level factors are sum-coded as -0.5 / +0.5 (second level = +0.5):
#   group (mono -> biling), time (t1 -> t2), speaker (child -> mother), gender (Female -> Male),
#   has_older_sibling (no -> yes)
# Continuous predictors are grand-mean centered, so every main effect is an
# average effect, not a simple effect at a reference level.

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(lme4)
library(lmerTest)
library(emmeans)
library(effectsize)
library(ggplot2)
library(ggeffects)

# Run this script from the repository root (the folder that contains data/ and figures/).
if (!dir.exists("data")) stop("Run from the repository root: setwd() to the folder containing data/.")

# ---------- helpers ----------

code_factors <- function(df) {
  for (v in intersect(c("group", "time", "speaker", "gender", "has_older_sibling"), names(df))) {
    df[[v]] <- droplevels(factor(df[[v]]))
    if (nlevels(df[[v]]) == 2) {
      contrasts(df[[v]]) <- matrix(c(-0.5, 0.5), ncol = 1,
                                   dimnames = list(levels(df[[v]]), levels(df[[v]])[2]))
    }
  }
  df
}

center <- function(df, vars) {
  df %>% mutate(across(all_of(vars), ~ .x - mean(.x, na.rm = TRUE), .names = "{.col}_c"))
}

read_mlu <- function(path) {
  read_excel(path) %>%
    rename_with(tolower) %>%
    mutate(id = as.character(id), speaker = tolower(speaker), group = tolower(group),
           time = case_when(age_months == 36 ~ "t1", age_months == 42 ~ "t2", TRUE ~ as.character(age_months))) %>%
    select(id, group, speaker, time, mlu)
}

read_lex_sheet <- function(path, sheet_name, group_label, time_label) {
  read_excel(path, sheet = sheet_name) %>%
    rename_with(tolower) %>%
    rename(lex_types = word_types) %>%
    mutate(id = as.character(id), group = group_label,
           speaker = tolower(speaker), time = time_label) %>%
    select(-recording_age)
}

read_lexical <- function(path, group_label, time_label) {
  bind_rows(lapply(excel_sheets(path), function(s)
    read_lex_sheet(path, s, group_label, time_label)))
}

standardise_speaker <- function(df) {
  df %>% mutate(speaker = case_when(
    tolower(speaker) %in% c("chi", "child")                   ~ "child",
    tolower(speaker) %in% c("mot", "mother", "mom", "parent") ~ "mother",
    TRUE ~ tolower(speaker)))
}


# colours differ in lightness (dark blue vs lighter orange) and groups also differ in point shape / line type,
# so the figures stay legible in grayscale
group_colours <- c("mono" = "#2C5AA0", "biling" = "#F28E2B")
group_shapes  <- c("mono" = 16, "biling" = 17)
group_lines   <- c("mono" = "solid", "biling" = "22")  # short dashes (2 on, 2 off)
group_labels  <- c("mono" = "Monolingual", "biling" = "Bilingual")
time_labels   <- c(t1 = "Age 3", t2 = "Age 3;6")

theme_lang <- function() {
  theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold", size = 14, margin = margin(b = 6)),
          plot.subtitle = element_text(color = "grey40", size = 11, margin = margin(b = 10)),
          axis.title = element_text(size = 12), legend.position = "bottom",
          legend.title = element_text(face = "bold"), strip.text = element_text(face = "bold"),
          panel.grid.minor = element_blank(), plot.margin = margin(12, 12, 12, 12))
}

# model-predicted means by time, one line per group
plot_time_pred <- function(model, title, ylab) {
  ggpredict(model, terms = c("time", "group")) %>% as.data.frame() %>%
    ggplot(aes(x = x, y = predicted, color = group, group = group)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.12, color = NA) +
    geom_line(aes(linetype = group), linewidth = 1) + geom_point(aes(shape = group), size = 3) +
    scale_x_discrete(labels = time_labels) +
    scale_color_manual(values = group_colours, labels = group_labels) +
    scale_fill_manual(values = group_colours, labels = group_labels) +
    scale_shape_manual(values = group_shapes, labels = group_labels) +
    scale_linetype_manual(values = group_lines, labels = group_labels) +
    labs(title = title, subtitle = "Shaded bands = 95% CI", x = "Age", y = ylab,
         color = "Group", fill = "Group", shape = "Group", linetype = "Group") + theme_lang()
}

# model-predicted slope of a centered predictor, one line per group.
# mean_x = the mean that was subtracted when centering, added back so the x-axis is on the raw scale.
plot_slope_pred <- function(model, predictor, mean_x, title, xlab, ylab, facet_time = FALSE) {
  terms <- c(paste0(predictor, " [all]"), "group", if (facet_time) "time")
  d <- ggpredict(model, terms = terms) %>% as.data.frame() %>% mutate(x = x + mean_x)
  p <- ggplot(d, aes(x = x, y = predicted, color = group, fill = group)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.12, color = NA) +
    geom_line(aes(linetype = group), linewidth = 1.1) +
    scale_color_manual(values = group_colours, labels = group_labels) +
    scale_fill_manual(values = group_colours, labels = group_labels) +
    scale_linetype_manual(values = group_lines, labels = group_labels) +
    labs(title = title, subtitle = "Shaded bands = 95% CI",
         x = xlab, y = ylab, color = "Group", fill = "Group", linetype = "Group") + theme_lang()
  if (facet_time) p <- p + facet_wrap(~facet, labeller = labeller(facet = time_labels))
  p
}

# ---------- load and merge ----------

mlu_all <- read_mlu("data/MLU_all.xlsx") %>%
  standardise_speaker() %>% mutate(id = str_remove(id, "^0+"))

lex_all <- bind_rows(
  read_lexical("data/lexical_biling_age3.xlsx",   "biling", "t1"),
  read_lexical("data/lexical_biling_age3-6.xlsx", "biling", "t2"),
  read_lexical("data/lexical_mono_age3.xlsx",       "mono",   "t1"),
  read_lexical("data/lexical_mono_age3-6.xlsx",     "mono",   "t2")
) %>% standardise_speaker() %>% mutate(id = str_remove(id, "^0+"))

common_ids <- intersect(mlu_all$id, lex_all$id)
mlu_all <- filter(mlu_all, id %in% common_ids)
lex_all <- filter(lex_all, id %in% common_ids)

df_master <- full_join(mlu_all, lex_all, by = c("id", "group", "speaker", "time")) %>%
  mutate(group   = factor(group,   levels = c("mono", "biling")),
         speaker = factor(speaker, levels = c("child", "mother")),
         time    = factor(time,    levels = c("t1", "t2"))) %>%
  arrange(group, id, time, speaker)

complete_ids <- df_master %>% distinct(id, speaker) %>% count(id) %>%
  filter(n == 2) %>% pull(id)
df_master <- filter(df_master, id %in% complete_ids)

# ============================================================
# PART 1. BOTH GROUPS (spanishinput.R)
# ============================================================

# ---- 1a. Group differences, child and mother separately ----

df_child  <- df_master %>% filter(speaker == "child")  %>% code_factors()
df_mother <- df_master %>% filter(speaker == "mother") %>% code_factors()

model_MLU_child  <- lmer(mlu       ~ group * time + (1 | id), data = df_child,  REML = TRUE)
model_MLU_mother <- lmer(mlu       ~ group * time + (1 | id), data = df_mother, REML = TRUE)
model_lex_child  <- lmer(lex_types ~ group * time + (1 | id), data = df_child,  REML = TRUE)
model_lex_mother <- lmer(lex_types ~ group * time + (1 | id), data = df_mother, REML = TRUE)

summary(model_MLU_child)
summary(model_MLU_mother)
summary(model_lex_child)
summary(model_lex_mother)

# change over time within each group; the two group-wise contrasts are treated as one family (Holm)
time_within_group <- function(m) rbind(pairs(emmeans(m, ~ time | group)), adjust = "holm")
time_within_group(model_MLU_child)
time_within_group(model_MLU_mother)
time_within_group(model_lex_child)
time_within_group(model_lex_mother)

# ---- 1b. Mother -> child (concurrent), and the reverse ----

child_data  <- df_master %>% filter(speaker == "child") %>%
  select(id, group, time, child_mlu = mlu, child_lex = lex_types)
mother_data <- df_master %>% filter(speaker == "mother") %>%
  select(id, group, time, mother_mlu = mlu, mother_lex = lex_types)

df_optionB <- left_join(child_data, mother_data, by = c("id", "group", "time")) %>%
  center(c("child_mlu", "child_lex", "mother_mlu", "mother_lex")) %>%
  code_factors()

model_MLU_B <- lmer(child_mlu ~ mother_mlu_c * group * time + (1 | id), data = df_optionB, REML = TRUE)
model_lex_B <- lmer(child_lex ~ mother_lex_c * group * time + (1 | id), data = df_optionB, REML = TRUE)
model_MLU_C <- lmer(mother_mlu ~ child_mlu_c * group * time + (1 | id), data = df_optionB, REML = TRUE)
model_lex_C <- lmer(mother_lex ~ child_lex_c * group * time + (1 | id), data = df_optionB, REML = TRUE)

summary(model_MLU_B)
summary(model_lex_B)
summary(model_MLU_C)
summary(model_lex_C)

# slope of mother -> child within each group x time cell
test(emtrends(model_MLU_B, ~ group * time, var = "mother_mlu_c"), adjust = "holm")  # 4 cell slopes vs 0
test(emtrends(model_lex_B, ~ group * time, var = "mother_lex_c"), adjust = "holm")
emtrends(model_MLU_B, pairwise ~ group * time, var = "mother_mlu_c", adjust = "tukey")$contrasts  # 6 pairwise slope contrasts
emtrends(model_lex_B, pairwise ~ group * time, var = "mother_lex_c", adjust = "tukey")$contrasts

# ---- 1c. Cross-domain (MLU <-> lexical diversity) ----

model_MLU_cross_B <- lmer(child_lex ~ mother_mlu_c * group * time + (1 | id), data = df_optionB, REML = TRUE)
model_lex_cross_B <- lmer(child_mlu ~ mother_lex_c * group * time + (1 | id), data = df_optionB, REML = TRUE)
model_MLU_cross_C <- lmer(mother_mlu ~ child_lex_c * group * time + (1 | id), data = df_optionB, REML = TRUE)
model_lex_cross_C <- lmer(mother_lex ~ child_mlu_c * group * time + (1 | id), data = df_optionB, REML = TRUE)

summary(model_MLU_cross_B)
summary(model_lex_cross_B)
summary(model_MLU_cross_C)
summary(model_lex_cross_C)

# ---- 1d. Lagged regression (directionality) ----

df_lagged <- df_master %>%
  select(id, group, speaker, time, mlu, lex_types) %>%
  pivot_wider(names_from = c(speaker, time), values_from = c(mlu, lex_types), names_sep = "_") %>%
  center(c("mlu_mother_t1", "mlu_child_t1", "mlu_mother_t2",
           "lex_types_mother_t1", "lex_types_child_t1", "lex_types_mother_t2")) %>%
  code_factors()

lag_MLU <- lm(mlu_child_t2       ~ mlu_mother_t1_c       * group + mlu_child_t1_c,       data = df_lagged)
lag_lex <- lm(lex_types_child_t2 ~ lex_types_mother_t1_c * group + lex_types_child_t1_c, data = df_lagged)
rev_MLU <- lm(mlu_mother_t2       ~ mlu_child_t1_c       * group + mlu_mother_t1_c,       data = df_lagged)
rev_lex <- lm(lex_types_mother_t2 ~ lex_types_child_t1_c * group + lex_types_mother_t1_c, data = df_lagged)

summary(lag_MLU)
summary(lag_lex)
summary(rev_MLU)
summary(rev_lex)

# ---- 1e. Plots ----

facet_lab <- labeller(time = time_labels, speaker = c(child = "Child", mother = "Mother"))

p_dist_mlu <- ggplot(df_master, aes(x = mlu, fill = group)) +
  geom_histogram(alpha = 0.5, bins = 20, position = "identity") +
  facet_grid(speaker ~ time, labeller = facet_lab) +
  scale_fill_manual(values = group_colours, labels = group_labels) +
  labs(title = "Distribution of MLU by Speaker, Group, and Age",
       x = "MLU (morphemes)", y = "Count", fill = "Group") + theme_lang()

p_dist_lex <- ggplot(df_master, aes(x = lex_types, fill = group)) +
  geom_histogram(alpha = 0.5, bins = 20, position = "identity") +
  facet_grid(speaker ~ time, labeller = facet_lab) +
  scale_fill_manual(values = group_colours, labels = group_labels) +
  labs(title = "Distribution of Lexical Diversity by Speaker, Group, and Age",
       x = "Word Types", y = "Count", fill = "Group") + theme_lang()

p_mean_mlu <- ggplot(df_master, aes(x = time, y = mlu, color = group, shape = group)) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  stat_summary(fun = mean, geom = "line", aes(group = group, linetype = group), linewidth = 1.2) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  facet_wrap(~speaker, labeller = facet_lab) +
  scale_x_discrete(labels = time_labels) +
  scale_color_manual(values = group_colours, labels = group_labels) +
  scale_shape_manual(values = group_shapes, labels = group_labels) +
  scale_linetype_manual(values = group_lines, labels = group_labels) +
  labs(title = "MLU: Raw Data and Group Means", x = "Age", y = "MLU (morphemes)",
       color = "Group", shape = "Group", linetype = "Group") +
  theme_lang()

p_mean_lex <- ggplot(df_master, aes(x = time, y = lex_types, color = group, shape = group)) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  stat_summary(fun = mean, geom = "line", aes(group = group, linetype = group), linewidth = 1.2) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  facet_wrap(~speaker, labeller = facet_lab) +
  scale_x_discrete(labels = time_labels) +
  scale_color_manual(values = group_colours, labels = group_labels) +
  scale_shape_manual(values = group_shapes, labels = group_labels) +
  scale_linetype_manual(values = group_lines, labels = group_labels) +
  labs(title = "Lexical Diversity: Raw Data and Group Means", x = "Age", y = "Word Types",
       color = "Group", shape = "Group", linetype = "Group") +
  theme_lang()

p_pred_MLU_child  <- plot_time_pred(model_MLU_child,  "Child MLU: Model Predictions",           "Predicted MLU (morphemes)")
p_pred_MLU_mother <- plot_time_pred(model_MLU_mother, "Maternal MLU: Model Predictions",         "Predicted MLU (morphemes)")
p_pred_lex_child  <- plot_time_pred(model_lex_child,  "Child Lexical Diversity: Model Predictions",   "Predicted Word Types")
p_pred_lex_mother <- plot_time_pred(model_lex_mother, "Maternal Lexical Diversity: Model Predictions", "Predicted Word Types")

p_B_MLU <- plot_slope_pred(model_MLU_B, "mother_mlu_c", mean(df_optionB$mother_mlu, na.rm = TRUE),
                           "Maternal MLU Predicting Child MLU", "Maternal MLU (morphemes)", "Predicted Child MLU", facet_time = TRUE)
p_B_lex <- plot_slope_pred(model_lex_B, "mother_lex_c", mean(df_optionB$mother_lex, na.rm = TRUE),
                           "Maternal Lexical Diversity Predicting Child Lexical Diversity", "Maternal word types", "Predicted Child Word Types", facet_time = TRUE)
p_C_MLU <- plot_slope_pred(model_MLU_C, "child_mlu_c", mean(df_optionB$child_mlu, na.rm = TRUE),
                           "Child MLU Predicting Maternal MLU", "Child MLU (morphemes)", "Predicted Maternal MLU", facet_time = TRUE)
p_C_lex <- plot_slope_pred(model_lex_C, "child_lex_c", mean(df_optionB$child_lex, na.rm = TRUE),
                           "Child Lexical Diversity Predicting Maternal Lexical Diversity", "Child word types", "Predicted Maternal Word Types", facet_time = TRUE)

p_lag_forward_MLU <- plot_slope_pred(lag_MLU, "mlu_mother_t1_c", mean(df_lagged$mlu_mother_t1, na.rm = TRUE),
                                     "Forward Lagged: Maternal MLU (Age 3) to Child MLU (Age 3;6)", "Maternal MLU at Age 3", "Predicted Child MLU at Age 3;6")
p_lag_forward_lex <- plot_slope_pred(lag_lex, "lex_types_mother_t1_c", mean(df_lagged$lex_types_mother_t1, na.rm = TRUE),
                                     "Forward Lagged: Maternal Lexical Diversity (Age 3) to Child (Age 3;6)", "Maternal word types at Age 3", "Predicted Child Word Types at Age 3;6")
p_lag_reverse_MLU <- plot_slope_pred(rev_MLU, "mlu_child_t1_c", mean(df_lagged$mlu_child_t1, na.rm = TRUE),
                                     "Reverse Lagged: Child MLU (Age 3) to Maternal MLU (Age 3;6)", "Child MLU at Age 3", "Predicted Maternal MLU at Age 3;6")
p_lag_reverse_lex <- plot_slope_pred(rev_lex, "lex_types_child_t1_c", mean(df_lagged$lex_types_child_t1, na.rm = TRUE),
                                     "Reverse Lagged: Child Lexical Diversity (Age 3) to Maternal (Age 3;6)", "Child word types at Age 3", "Predicted Maternal Word Types at Age 3;6")

for (p in list(p_dist_mlu, p_dist_lex, p_mean_mlu, p_mean_lex,
               p_pred_MLU_child, p_pred_MLU_mother, p_pred_lex_child, p_pred_lex_mother,
               p_B_MLU, p_B_lex, p_C_MLU, p_C_lex,
               p_lag_forward_MLU, p_lag_forward_lex, p_lag_reverse_MLU, p_lag_reverse_lex)) print(p)

# ---- 1f. Lexical categories (verbs, nouns, adjectives) ----

group_colours2 <- c("Monolingual" = "#2C5AA0", "Bilingual" = "#F28E2B")
cat_colours    <- c("Verbs" = "#6C5CA8", "Nouns" = "#5FB3A1", "Adjectives" = "#F6E8A6")  # dark / medium / light

df_categories <- df_master %>%
  mutate(verbs      = rowSums(across(any_of("v")),   na.rm = TRUE),
         nouns      = rowSums(across(any_of("n")),   na.rm = TRUE),
         adjectives = rowSums(across(any_of("adj")), na.rm = TRUE)) %>%
  select(id, group, speaker, time, verbs, nouns, adjectives) %>%
  mutate(time    = dplyr::recode(time,    "t1" = "Age 3", "t2" = "Age 3;6"),
         group   = dplyr::recode(group,   "mono" = "Monolingual", "biling" = "Bilingual"),
         speaker = dplyr::recode(speaker, "child" = "Child", "mother" = "Mother"),
         group   = factor(group,   levels = c("Monolingual", "Bilingual")),
         speaker = factor(speaker, levels = c("Child", "Mother")))

desc_stats <- df_categories %>%
  group_by(speaker, group, time) %>%
  summarise(n = n(),
            verb_mean = mean(verbs),      verb_sd = sd(verbs),      verb_median = median(verbs),
            noun_mean = mean(nouns),      noun_sd = sd(nouns),      noun_median = median(nouns),
            adj_mean  = mean(adjectives), adj_sd  = sd(adjectives), adj_median  = median(adjectives),
            .groups = "drop") %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  arrange(speaker, group, time)
print(desc_stats, width = Inf)

df_long_cats <- df_categories %>%
  pivot_longer(c(verbs, nouns, adjectives), names_to = "category", values_to = "count") %>%
  mutate(category = factor(category, levels = c("verbs", "nouns", "adjectives"),
                           labels = c("Verbs", "Nouns", "Adjectives")))

summary_long <- df_long_cats %>%
  group_by(speaker, group, time, category) %>%
  summarise(mean = mean(count), sd = sd(count), se = sd / sqrt(n()), .groups = "drop")

plot_bar <- function(who) {
  ggplot(filter(summary_long, speaker == who), aes(x = category, y = mean, fill = group)) +
    geom_col(position = position_dodge(0.7), width = 0.6) +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                  position = position_dodge(0.7), width = 0.2, linewidth = 0.5) +
    facet_wrap(~time) + scale_fill_manual(values = group_colours2) +
    labs(title = paste(who, "Lexical Categories by Group and Age"),
         subtitle = "Mean lemma counts ± SD  |  Verbs = main verbs only",
         x = "Lexical Category", y = "Mean Lemma Count", fill = "Group") + theme_lang()
}

plot_box <- function(who) {
  ggplot(filter(df_long_cats, speaker == who), aes(x = group, y = count, fill = category)) +
    geom_boxplot(position = position_dodge(width = 0.75), width = 0.6, alpha = 0.8) +
    scale_fill_manual(values = cat_colours) +
    labs(title = paste(who, "Lexical Category Distributions by Group"),
         x = "Group", y = "Lemma Count", fill = "Category") + theme_lang()
}

plot_trajectory <- function(who) {
  ggplot(filter(summary_long, speaker == who), aes(x = time, y = mean, color = group, group = group)) +
    geom_ribbon(aes(ymin = mean - se, ymax = mean + se, fill = group), alpha = 0.12, color = NA) +
    geom_line(linewidth = 1.1) + geom_point(size = 3) +
    facet_wrap(~category, scales = "free_y") +
    scale_color_manual(values = group_colours2) + scale_fill_manual(values = group_colours2) +
    labs(title = paste(who, "Lexical Category Growth by Group"),
         subtitle = "Mean lemma counts ± SE  |  Verbs = main verbs only",
         x = "Age", y = "Mean Lemma Count", color = "Group", fill = "Group") + theme_lang()
}

for (who in c("Child", "Mother")) print(plot_bar(who))
for (who in c("Child", "Mother")) print(plot_box(who))
for (who in c("Child", "Mother")) print(plot_trajectory(who))

# ---- 1g. Does maternal speech account for the group gap in child language? ----
# total = bilingual - monolingual child gap; direct = gap with the mother measure in the model;
# a = group -> mother measure; b = mother measure -> child measure; indirect = a * b.
# 95% CIs from a bootstrap that resamples whole dyads.

df_gap <- df_optionB %>% filter(!is.na(child_mlu), !is.na(mother_mlu), !is.na(child_lex), !is.na(mother_lex))

gap_paths <- function(d, y, x) {
  d$m_c <- d[[x]] - mean(d[[x]])
  total  <- lmer(as.formula(paste(y, "~ group * time + (1 | id)")), d)
  direct <- lmer(as.formula(paste(y, "~ group * time + m_c + (1 | id)")), d)
  apath  <- lmer(as.formula(paste(x, "~ group * time + (1 | id)")), d)
  a <- fixef(apath)[["groupbiling"]]; b <- fixef(direct)[["m_c"]]
  c(total = fixef(total)[["groupbiling"]], direct = fixef(direct)[["groupbiling"]],
    a = a, b = b, indirect = a * b)
}

gap_boot <- function(d, y, x, B = 1000) {
  ids <- unique(d$id)
  t(replicate(B, {
    s <- sample(ids, replace = TRUE)
    bd <- bind_rows(lapply(seq_along(s), function(i) mutate(filter(d, id == s[i]), id = paste0(id, "_", i)))) %>%
      code_factors()
    tryCatch(gap_paths(bd, y, x), error = function(e) rep(NA_real_, 5))
  }))
}

# The bootstrap is slow (~6,000 model fits), so the result is cached; delete data/gap_results.rds to re-run it (about 6,000 model fits).
if (file.exists("data/gap_results.rds")) {
  gap_results <- readRDS("data/gap_results.rds")
} else {
  set.seed(1)
  gap_results <- lapply(list(MLU = c("child_mlu", "mother_mlu"), lex = c("child_lex", "mother_lex")), function(v) {
    est <- gap_paths(df_gap, v[1], v[2])
    bt  <- gap_boot(df_gap, v[1], v[2])
    ci  <- apply(bt, 2, quantile, c(0.025, 0.975), na.rm = TRUE)
    rbind(estimate = est, lo95 = ci[1, ], hi95 = ci[2, ])
  })
  saveRDS(gap_results, "data/gap_results.rds")
}
lapply(gap_results, round, 3)

# ============================================================
# PART 2. BILINGUAL GROUP WITH METADATA (bilingandmeta.R)
# ============================================================

meta_raw <- read_excel("data/Hoffmeta_subset.xlsx") %>%
  rename_with(tolower) %>%
  mutate(across(where(is.numeric), ~ na_if(.x, -997))) %>%
  mutate(across(where(is.numeric), ~ na_if(.x, -999))) %>%
  mutate(id = str_remove(str_extract(participant_id, "\\d+$"), "^0+")) %>%
  select(-participant_id)

meta_static <- meta_raw %>%
  select(id, gender, ethnicity, num_siblings_30, birth_order_30, preschool_hist_hrs_week,
         mothers_native_language, mother_education_spa_at_5, mother_education_eng_at_5,
         fathers_native_language, father_education_spa_at_5, father_education_eng_at_5) %>%
  mutate(
    gender = factor(case_when(gender == 0 ~ "Female", gender == 1 ~ "Male")),
    # birth_order_30 is categorical (1 = only child, 2 = oldest, 3 = middle, 4 = youngest; inferred,
    # the codebook does not specify), so it is collapsed to whether the child has an older sibling
    has_older_sibling = factor(case_when(birth_order_30 %in% c(3, 4) ~ "yes",
                                         birth_order_30 %in% c(1, 2) ~ "no"), levels = c("no", "yes")),
    ethnicity = factor(case_when(
      ethnicity == 1 ~ "African American", ethnicity == 2 ~ "Hispanic White",
      ethnicity == 3 ~ "Hispanic African", ethnicity == 4 ~ "European American",
      ethnicity == 5 ~ "Asian",            ethnicity == 6 ~ "Other")),
    across(c(mothers_native_language, fathers_native_language), ~ factor(case_when(
      .x == 1 ~ "English", .x == 2 ~ "Spanish", .x == 3 ~ "Bilingual (Spanish-English)",
      .x == 4 ~ "Monolingual Other", .x == 5 ~ "Bilingual Other"))))

meta_timevarying <- meta_raw %>%
  select(id, chronological_age_36, chronological_age_42,
         input_percent_spa_home_36, input_percent_spa_home_42,
         preschool_hrs_week_36, preschool_hrs_week_42) %>%
  pivot_longer(-id, names_to = c(".value", "age"), names_pattern = "^(.+)_(36|42)$") %>%
  mutate(time = case_when(age == "36" ~ "t1", age == "42" ~ "t2")) %>%
  select(-age)

meta_long <- left_join(meta_static, meta_timevarying, by = "id")

df_biling <- df_master %>% filter(group == "biling") %>%
  left_join(meta_long, by = c("id", "time"))

mother_measures <- df_biling %>% filter(speaker == "mother") %>%
  select(id, time, mother_mlu = mlu, mother_lex_types = lex_types)

pred_vars <- c("mother_education_spa_at_5", "father_education_spa_at_5",
               "input_percent_spa_home", "preschool_hrs_week")

df_child_b <- df_biling %>% filter(speaker == "child") %>%
  left_join(mother_measures, by = c("id", "time")) %>%
  center(c(pred_vars, "mother_mlu", "mother_lex_types")) %>%
  code_factors()

df_mother_b <- df_biling %>% filter(speaker == "mother") %>%
  center("mother_education_spa_at_5") %>%
  code_factors()

meta_terms <- "time + gender + has_older_sibling + mother_education_spa_at_5_c +
               father_education_spa_at_5_c + input_percent_spa_home_c + preschool_hrs_week_c"

fm <- function(y, rhs) as.formula(paste(y, "~", rhs, "+ (1 | id)"))

# ---- 2a. Metadata models, child and mother ----

model_MLU_child_meta  <- lmer(fm("mlu", meta_terms), data = df_child_b, REML = TRUE)
model_lex_child_meta  <- lmer(fm("lex_types", meta_terms), data = df_child_b, REML = TRUE)
model_MLU_mother_meta <- lmer(fm("mlu", "time + mother_education_spa_at_5_c"), data = df_mother_b, REML = TRUE)
model_lex_mother_meta <- lmer(fm("lex_types", "time + mother_education_spa_at_5_c"), data = df_mother_b, REML = TRUE)

summary(model_MLU_child_meta)
summary(model_MLU_mother_meta)
summary(model_lex_child_meta)
summary(model_lex_mother_meta)

# ---- 2b. Concurrent mother -> child, with all metadata ----

model_MLU_child_with_mom <- lmer(fm("mlu", paste(meta_terms, "+ mother_mlu_c")), data = df_child_b, REML = TRUE)
model_lex_child_with_mom <- lmer(fm("lex_types", paste(meta_terms, "+ mother_lex_types_c")), data = df_child_b, REML = TRUE)

summary(model_MLU_child_with_mom)
summary(model_lex_child_with_mom)

standardize_parameters(model_MLU_child_with_mom, method = "refit")
standardize_parameters(model_lex_child_with_mom, method = "refit")

# does the mother measure add beyond the metadata? (likelihood-ratio test, ML fits)
anova(update(model_MLU_child_meta, REML = FALSE), update(model_MLU_child_with_mom, REML = FALSE))
anova(update(model_lex_child_meta, REML = FALSE), update(model_lex_child_with_mom, REML = FALSE))

# does the mother slope change with time? (main effect = average slope across times)
model_MLU_mom_x_time <- lmer(fm("mlu", paste(meta_terms, "+ mother_mlu_c * time")), data = df_child_b, REML = TRUE)
model_lex_mom_x_time <- lmer(fm("lex_types", paste(meta_terms, "+ mother_lex_types_c * time")), data = df_child_b, REML = TRUE)
summary(model_MLU_mom_x_time)
summary(model_lex_mom_x_time)

# ---- 2c. Reduced models ----

model_MLU_two <- lmer(fm("mlu", "input_percent_spa_home_c + mother_mlu_c"), data = df_child_b, REML = TRUE)
model_lex_two <- lmer(fm("lex_types", "input_percent_spa_home_c + mother_lex_types_c"), data = df_child_b, REML = TRUE)
simplemlu     <- lmer(fm("mlu", "mother_mlu_c"), data = df_child_b, REML = TRUE)
simplelex     <- lmer(fm("lex_types", "mother_lex_types_c"), data = df_child_b, REML = TRUE)

summary(model_MLU_two)
summary(model_lex_two)
summary(simplemlu)
summary(simplelex)

# ---- 2d. Participant information (children in the models) ----

model_ids <- unique(df_child_b$id)

meta_timevarying_wide <- meta_timevarying %>%
  filter(id %in% model_ids) %>%
  select(id, time, chronological_age, input_percent_spa_home, preschool_hrs_week) %>%
  pivot_wider(names_from = time,
              values_from = c(chronological_age, input_percent_spa_home, preschool_hrs_week))

meta_model <- meta_static %>% filter(id %in% model_ids) %>%
  left_join(meta_timevarying_wide, by = "id")

cont_vars <- c("num_siblings_30", "preschool_hist_hrs_week",
               "mother_education_spa_at_5", "father_education_spa_at_5",
               "mother_education_eng_at_5", "father_education_eng_at_5",
               "input_percent_spa_home_t1", "input_percent_spa_home_t2",
               "preschool_hrs_week_t1", "preschool_hrs_week_t2",
               "chronological_age_t1", "chronological_age_t2")

cont_df <- do.call(rbind, lapply(cont_vars, function(v) {
  x <- meta_model[[v]]
  data.frame(Variable = v, M = round(mean(x, na.rm = TRUE), 2), SD = round(sd(x, na.rm = TRUE), 2),
             Min = round(min(x, na.rm = TRUE), 2), Max = round(max(x, na.rm = TRUE), 2),
             N_miss = sum(is.na(x)))
}))
print(cont_df, row.names = FALSE)

for (v in c("gender", "has_older_sibling", "ethnicity", "mothers_native_language", "fathers_native_language")) {
  tbl <- table(meta_model[[v]], useNA = "ifany")
  print(data.frame(variable = v, level = names(tbl), n = as.integer(tbl),
                   pct = round(100 * as.numeric(prop.table(tbl)), 1)), row.names = FALSE)
}

n_children <- length(model_ids)
n_obs <- c(MLU = nobs(model_MLU_child_with_mom), lex = nobs(model_lex_child_with_mom))
n_children
n_obs

# ---- 2e. Bilingual plots: mother -> child slope at each age ----

plot_biling_slope <- function(model, predictor, mean_x, title, xlab, ylab) {
  ggpredict(model, terms = c(paste0(predictor, " [all]"), "time")) %>% as.data.frame() %>%
    mutate(x = x + mean_x) %>%
    ggplot(aes(x = x, y = predicted, color = group, fill = group)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.12, color = NA) +
    geom_line(linewidth = 1.1) +
    scale_color_discrete(labels = time_labels) + scale_fill_discrete(labels = time_labels) +
    labs(title = title, subtitle = "Bilingual group; other predictors held at their means",
         x = xlab, y = ylab, color = "Age", fill = "Age") + theme_lang()
}

p_biling_MLU <- plot_biling_slope(model_MLU_mom_x_time, "mother_mlu_c", mean(df_child_b$mother_mlu, na.rm = TRUE),
                                  "Bilingual: Maternal MLU Predicting Child MLU", "Maternal MLU (morphemes)", "Predicted Child MLU")
p_biling_lex <- plot_biling_slope(model_lex_mom_x_time, "mother_lex_types_c", mean(df_child_b$mother_lex_types, na.rm = TRUE),
                                  "Bilingual: Maternal Lexical Diversity Predicting Child", "Maternal word types", "Predicted Child Word Types")
print(p_biling_MLU)
print(p_biling_lex)

# ============================================================
# FIGURE EXPORT (300 dpi JPEGs for the IJB manuscript and supplementary materials)
# ============================================================

dir.create("figures", showWarnings = FALSE)

# Figure 3 has no plotting code above, so it is built here from gap_results (section 1g):
# total = bilingual - monolingual child gap; direct = gap adjusted for concurrent maternal speech
df_gap_plot <- bind_rows(lapply(names(gap_results), function(m) {
  r <- gap_results[[m]]
  data.frame(measure = m, path = c("total", "direct"),
             est = r["estimate", c("total", "direct")],
             lo  = r["lo95",     c("total", "direct")],
             hi  = r["hi95",     c("total", "direct")])
})) %>%
  mutate(measure = factor(measure, levels = c("MLU", "lex"),
                          labels = c("Child MLU (morphemes)", "Child lexical diversity (word types)")),
         path    = factor(path, levels = c("total", "direct"),
                          labels = c("Total gap", "Adjusted for\nmaternal speech")))

p_gap <- ggplot(df_gap_plot, aes(x = path, y = est)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.8) +
  geom_linerange(aes(ymin = lo, ymax = hi), color = "grey20", linewidth = 1.2) +
  geom_point(color = "grey20", size = 5) +
  facet_wrap(~measure, scales = "free_y") +
  labs(title = "Bilingual–Monolingual Gap in Child Language",
       subtitle = "Estimated group difference (bilingual minus monolingual)\nwith and without maternal speech; 95% bootstrap CIs",
       x = NULL, y = "Group difference") + theme_lang()

# Figure 2 is p_B_lex without its title/subtitle (the caption carries the title) and with a capitalised x label
p_fig2 <- p_B_lex + labs(title = NULL, subtitle = NULL, x = "Maternal Word Types")

# ---- Main manuscript ----
# Figure 1 (top panel): MLU raw data and group means, by speaker
ggsave("figures/Figure1_top_MLU_means.jpg",           p_mean_mlu, width = 6.5, height = 5.1, dpi = 300, bg = "white", quality = 100)
# Figure 1 (bottom panel): lexical diversity raw data and group means, by speaker
ggsave("figures/Figure1_bottom_lex_means.jpg",        p_mean_lex, width = 6.5, height = 5.1, dpi = 300, bg = "white", quality = 100)
# Figure 2: maternal lexical diversity predicting child lexical diversity, by group and age
ggsave("figures/Figure2_maternal_lex_predicting_child_lex.jpg", p_fig2, width = 7.4, height = 5.0, dpi = 300, bg = "white", quality = 100)
# Figure 3: bilingual-monolingual gap in child MLU and lexical diversity, with/without maternal speech
ggsave("figures/Figure3_group_gap.jpg",               p_gap,      width = 7.4, height = 5.5, dpi = 300, bg = "white", quality = 100)

# ---- Supplementary materials ----
# Figure S1 (top): distribution of MLU by speaker, group, and age
ggsave("figures/FigureS1_top_MLU_distribution.jpg",   p_dist_mlu, width = 6.5, height = 6.0, dpi = 300, bg = "white", quality = 100)
# Figure S1 (bottom): distribution of lexical diversity by speaker, group, and age
ggsave("figures/FigureS1_bottom_lex_distribution.jpg", p_dist_lex, width = 6.5, height = 6.0, dpi = 300, bg = "white", quality = 100)
# Figure S2 (left): child lexical categories, bar graph by age
ggsave("figures/FigureS2_child_bar.jpg",              plot_bar("Child"),  width = 6, height = 5.5, dpi = 300, bg = "white", quality = 100)
# Figure S2 (right): child lexical categories, boxplot ages combined
ggsave("figures/FigureS2_child_box.jpg",              plot_box("Child"),  width = 6, height = 5.5, dpi = 300, bg = "white", quality = 100)
# Figure S3 (top): mother lexical categories, boxplot ages combined (order as in the supplementary file)
ggsave("figures/FigureS3_mother_box.jpg",             plot_box("Mother"), width = 6, height = 5.5, dpi = 300, bg = "white", quality = 100)
# Figure S3 (bottom): mother lexical categories, bar graph by age
ggsave("figures/FigureS3_mother_bar.jpg",             plot_bar("Mother"), width = 6, height = 5.5, dpi = 300, bg = "white", quality = 100)
