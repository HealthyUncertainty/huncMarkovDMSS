# huncMarkovDMSS

**Diffuse Mucosal Sclerosis Syndrome Multi-Comparator Cost-Effectiveness Model**

An R package implementing an interactive multi-comparator Markov cost-effectiveness
model for Cardivex, Zonapride, Septoplasty, and Ablatherm versus Standard of Care
in patients with Diffuse Mucosal Sclerosis Syndrome (DMSS). Fictional
illustrative model for the HEPackageR skill validation exercise.

## Model Structure

| Feature | Value |
|---------|-------|
| Health states | 4 (Grade I, Grade II, Grade III, Dead) |
| Treatment arms | 5 (Cardivex, SoC, Zonapride, Septoplasty, Ablatherm) |
| Cycle length | 4/52 years (4 weeks) |
| Time horizon | 80 years (1,040 cycles) |
| Discount rate | 3% (beginning-of-cycle, no HCC) |
| Background mortality | Fictional sex-blended life table (48% female) |
| Grade distributions | Frozen after response: cycle 8 (Cardivex/SoC), cycle 1 (others) |
| Perioperative mortality | Septoplasty 1.1%, Ablatherm 0.9% |
| WTP default | CAD $150,000 |

## Installation

```r
remotes::install_github("HealthyUncertainty/huncMarkovDMSS")
```

## Quick Start

```r
library(huncMarkovDMSS)
launch_app()
res <- run_model()
res$icer_table
res_psa <- run_model(n_sim = 1000, seed = 42)
plot_ce_plane(res_psa$psa_results)
plot_ceac(res_psa$psa_results)
```

## Validation

Base-case deterministic results validated against original R script.

| Arm | Total Cost (CAD) | QALYs | Life Years |
|-----|-----------------|-------|------------|
| Cardivex | $1,464,400 | 15.39 | 17.80 |
| SoC | $417,042 | 14.40 | 17.80 |
| Zonapride | $491,781 | 14.65 | 17.80 |
| Septoplasty | $367,217 | 15.44 | 17.61 |
| Ablatherm | $307,733 | 15.45 | 17.64 |

All 15 outcomes (3 per arm) match original to < 0.01. A naming bug in
the original script (NA traces for non-Cardivex/SoC arms) was identified
and corrected. See `tests/validation/` for details.

## Development

Ian Cromwell (healthyuncertainty@gmail.com)

Developed using the [HEPackageR](https://github.com/HealthyUncertainty/hepackager) skill for Claude AI.

## Disclaimer

Fictional illustrative model. Not for clinical or policy use.

