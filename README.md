# Beyond Prediction: A Causal Workflow using R for solving real world problems

[![PositConf 2026](https://img.shields.io/badge/PositConf-2026-blue)](https://posit.co/conference/)
[![R](https://img.shields.io/badge/R-4.5-blue)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## The Problem

Standard machine learning excels at prediction, but often fails to answer strategic questions like: *"What caused this?"* or *"What if we intervened?"*

**Example:** A churn model finds that delivery exceptions are associated with *less* churn, and its predictive accuracy is excellent. Taken at face value, the business should slow deliveries down. That is obviously wrong. The model isn't broken; it's answering a different question (*who will churn?*) than the one the business is asking (*what happens if we intervene?*).

## The Solution: A Causal Workflow on Top of the Models You Already Use

This repository demonstrates a **reproducible causal workflow in R**, not new ML machinery, but a discipline applied to the tools you already know (`dagitty`, `tidymodels`, `glm`):

1. **Map assumptions explicitly** using a Directed Acyclic Graph (DAG), and let `dagitty` compute the adjustment set
2. **Re-specify the same models** with the causally-correct variable set (add the confounder, drop the collider)
3. **Validate against simulated ground truth** so we can check whether the method actually works before trusting it, with a bootstrap CI and an E-value sensitivity analysis

There is a real trade-off: adding causal structure can cost some predictive accuracy, but it buys interpretability and helps to answer the intervention question. It's not a worse model, it's the right model for a different question. Keep your predictive model for scoring; use the causal specification for decisions.

### The Headline Trap: Collider Bias (with Simpson's in the Data)

The simulated scenario — *do delivery exceptions cause churn?* — embeds **two** classic causal pitfalls, and the headline model commits only **one** of them:

1. **Simpson's Paradox (confounding by order volume).** High-volume, highly-engaged customers place more orders, so they encounter *more* delivery exceptions — yet they are loyal and churn *less*. In the aggregate this makes exceptions look protective. This reversal lives in the **data** (see `figures/02_simpsons_paradox.png`); the naive model below actually *includes* the confounder, so this is not what breaks it.
2. **Collider bias (Berkson's paradox, via support tickets).** Both delivery exceptions **and** a latent trait (customer impatience) cause support tickets, so the ticket is a **collider**. It is also one of the most *predictive* features — which is exactly why a predictive-first workflow keeps it. The naive model throws in **every** observed feature (confounder included) and **still** gets the wrong sign: the collider alone is enough. **Good metrics, wrong advice.** The fix is to **REMOVE** the collider.

**The result:** the model that uses every feature (naive model), genuinely good test AUC, estimates the effect with the **wrong sign** (exceptions appear to *reduce* churn), while the DAG-guided model (add the confounder, drop the collider) **recovers the true effect**. The exact figures are computed live — see below.

> **A note:** logistic-regression coefficients are **non-collapsible**, so when a strong latent cause of churn is unmeasured the correctly-specified model returns a slightly *attenuated* log-odds coefficient. We therefore headline the **marginal Average Treatment Effect (ATE)** on the probability (risk-difference) scale, recovered by **g-computation**. The ATE is collapsible, decision-relevant, and recovers the truth.

---

## 🚀 Quick Start

### Prerequisites
- R 4.5+
- Positron or RStudio
- `rv` for dependency management (included in repo)

### Run the Complete Analysis

```r
rv sync
source("R/05_generate_figures.R")
```

This will:
- Create the publication-quality figures in `figures/`
- Validate estimates against ground truth (marginal ATE, percentage points)
- Print the comparison table with bias quantification
- Generate a bootstrap 95% confidence interval
- Compute an E-value sensitivity analysis for unmeasured confounding

## 📁 Repository Structure

```
causal-decision-framework/
├── R/
│   ├── 01_define_dag.R            # DAG specification + adjustment set
│   ├── 02_simulate_data.R         # Data generating process (ground truth)
│   ├── 03_naive_models.R          # ML models (the trap)
│   ├── 04_causal_models.R         # DAG-guided models (the solution)
│   ├── 06_validate_ground_truth.R # Validation: ATE, g-computation, bootstrap, E-value
│   ├── 07_comparison_plots.R      # Visualization functions
│   ├── 08_dgp_calibration.R       # In-silico lab design (collider calibration)
│   ├── 09_naive_roc_plots.R       # Naive-model ROC figures (metrics look great)
│   ├── 10_auc_comparison_plot.R   # AUC trade-off figure (naive vs causal)
│   └── 05_generate_figures.R      # Main execution script
├── tests/
│   └── testthat/
│       └── test_simulation.R      # Validates the headline contract + both traps
├── docs/
│   └── causal-framework-explained.qmd  # Live-computed wiki deep-dive
├── figures/                       # Generated plots
├── rv/                            # Dependency management
├── SLIDE_CONTEXT.md               # Tiered briefing pack for building the talk
└── README.md                      # This file
```

---

## Tech Stack

- **Causal Design:** `dagitty`, `ggdag`
- **Modeling:** `tidymodels` (GLM, Random Forest, XGBoost)
- **Estimation:** g-computation for the marginal ATE
- **Visualization:** `ggplot2`, `patchwork`
- **Validation:** bootstrap CI, bias quantification, E-value sensitivity analysis
- **Reproducibility:** `rv` for exact package versioning

---

## Running Tests

```r
# Run the test suite to validate the DGP creates BOTH traps
testthat::test_dir("tests/testthat")
```

The suite asserts, among other things, that:
1. The data hides the latent trait (`impatience` is unobserved)
2. A genuine Simpson's Paradox emerges in the data (aggregate negative; positive within every order-volume decile)
3. Omitting the confounder reverses the sign; adjusting for it recovers the positive sign
4. With the confounder already adjusted, conditioning on the collider ALONE flips the sign (Berkson's paradox)
5. The naive model has genuinely good metrics (AUC >= 0.80) AND the wrong sign — the headline contract
6. The causal model recovers the true marginal ATE; the kitchen-sink naive model gets the wrong sign
7. `dagitty` returns the confounder as the adjustment set (and never the collider)
8. The E-value is computed correctly

---

## Presented At

**PositConf 2026** — "Beyond Prediction: A Causal Workflow in R for Real-World Problems" (Virtual Session: Modelling, 20 min)

---

##  References & Further Reading

### Causal Inference Foundations
- Pearl, J. (2009). *Causality: Models, Reasoning, and Inference*
- Hernán, M.A. & Robins, J.M. (2020). *Causal Inference: What If*
- Pearl, J. & Mackenzie, D. (2018). *The Book of Why*

### Methods Used Here
- VanderWeele, T.J. & Ding, P. (2017). "Sensitivity Analysis in Observational Research: Introducing the E-Value." *Annals of Internal Medicine.*
- Elwert, F. & Winship, C. (2014). "Endogenous Selection Bias: The Problem of Conditioning on a Collider Variable."
- Greenland, S., Robins, J.M., & Pearl, J. (1999). "Confounding and Collapsibility in Causal Inference."

### R Implementation
- `dagitty`: [dagitty.net](http://dagitty.net/)
- `ggdag`: [ggdag.malco.io](https://ggdag.malco.io/)
- `tidymodels`: [tidymodels.org](https://www.tidymodels.org/)

---

## 📄 License

MIT License — Feel free to use this workflow for teaching, research, or commercial applications.

---

##  Author

**Kimberley Isabel Orozco Cornejo**
Data Science and Methodology Lead
GitHub: [@isa229](https://github.com/isa229)

**Clone this repo, draw your first DAG, and start validating your causal assumptions against a ground truth you control. 🚀**
