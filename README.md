# Replication materials

Code and derived data to reproduce every model, table value and figure in the manuscript
and supplementary materials.

## What is here

| Path | Contents |
|---|---|
| `analysis.R` | The full analysis, top to bottom: data assembly, all mixed models (Part 1: both groups; Part 2: bilingual group with home-environment metadata), lagged models, bootstrapped indirect effects, descriptive tables, and figure export. |
| `install_packages.R` | Installs the required R packages. |
| `data/MLU_all.xlsx` | One row per speaker per age: `id`, `group`, `speaker` (child/mother), `age_months` (36 = 3;0, 42 = 3;6), `morphemes`, `utterances`, `mlu`. |
| `data/lexical_{biling,mono}_age{3,3-6}.xlsx` | Lexical counts, one file per group and age, with a `child` sheet and a `mother` sheet. Columns: `id`, `speaker`, `recording_age`, `word_types` (distinct lemmas), `word_tokens`, then one column per part-of-speech tag giving the number of lemma types with that tag (e.g. `n` nouns, `v` verbs, `adj` adjectives; tags follow CLAN's %mor tier). |
| `data/Hoffmeta_subset.xlsx` | The survey variables from the Hoff corpus metadata file that the bilingual-group models use (column names as in the corpus file). |
| `data/gap_results.rds` | Cached output of the bootstrap in section 1g (seed 1, 1,000 resamples of dyads). Delete it to re-run the bootstrap (about 6,000 model fits; slow). |
| `figures/` | Created by `analysis.R` (300 dpi JPEGs named by figure number). |

## How to reproduce

1. Run `Rscript install_packages.R` once.
2. From this folder: `Rscript analysis.R > results.txt` (or open `analysis.R` in RStudio with this folder as the working directory and run it).
3. Model summaries, contrasts and descriptive tables are written to `results.txt`; figures are written to `figures/`.

The script stops with a message if it is not run from this folder. No other paths need editing.

## Where each result comes from

| Manuscript content | Section of `analysis.R` |
|---|---|
| Group x time models for child and mother MLU and word types, Holm-adjusted change over time | 1a |
| Concurrent mother-child models and slope tests by group and age | 1b |
| Cross-domain models (MLU <-> word types) | 1c |
| Lagged (forward and reverse) models | 1d |
| Figure 1, Figure S1; model-prediction plots | 1e |
| Noun, verb and adjective types; Figures S2 and S3 | 1f |
| Group gap with and without maternal speech; bootstrap CIs; Figure 3 | 1g, figure export |
| Bilingual home-environment models, reduced models, participant information table, Figure 2 inputs | Part 2 |

Figure 2 is the maternal-to-child word-types plot (`p_B_lex`) with its title removed; Figure 3 is built in the figure-export block at the end of the script.

## Data sources and how the counts were obtained

All analyses use two existing corpora accessed through CHILDES (TalkBank); no new data were collected.

- Bilingual group (51 dyads): the Spanish-English Hoff Corpus (Hoff, Core, & Shanks, 2020). Only the Spanish-only sessions were used.
- Monolingual group (41 dyads): the Spanish Fernández/Aguado Corpus (Fernández Vázquez, 2002).

Dyads contributing at each age, and the rest of the sample description, are in the manuscript.

**Counting procedure.** Transcripts were tagged automatically with CLAN `mor` (MacWhinney, 2000).
In the Hoff corpus, utterances marked as English or mixed Spanish-English were excluded from all MLU and lexical
counts; in the Fernández/Aguado corpus, non-target-language items tagged L2 on the %mor tier were excluded from
type counts. MLU was computed per speaker and age with CLAN `mlu` on the morpheme tier (`morphemes`,
`utterances`, `mlu` in `MLU_all.xlsx`). Word types were computed with CLAN `freq` on the %mor tier, reducing each
word form to its lemma, counting distinct lemmas per speaker, and aggregating across a child's sessions at
each age (`word_types`). The same `freq` output gave the lemma types within each part-of-speech category (noun, main verb, adjective;
auxiliaries, copulas, participles, infinitives and pronominals are excluded as described in the Supplementary Methods).
The CLAN output was compiled into the spreadsheets in `data/` with a short script not included here, because it
requires the raw transcripts.

**What is and is not shared.** The raw transcripts are not redistributed; they are available from CHILDES under
its terms of use, and anyone using them should cite the corpus references above. The files in `data/` contain only
per-speaker counts and the survey variables listed above, not utterance-level text. Monolingual `id`s are the
first-name pseudonyms supplied with the corpus; bilingual `id`s are the corpus participant numbers.

## Software

R packages: readxl, dplyr, tidyr, stringr, lme4, lmerTest, emmeans, effectsize, ggplot2, ggeffects.
Coding: two-level factors are sum-coded (-0.5/+0.5, second level = +0.5) and continuous predictors
are grand-mean centered, so main effects are average effects.
