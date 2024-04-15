# Getting Started

## Data Links

* [Website put together by the Invisible Institute visualizing their complaint data](https://cpdp.co/).
* [Github repository of the code and data available from the Invisible Institute](https://github.com/invinst/chicago-police-data/tree/master/data/unified_data).
* [Code Ocean repository for the Ba et al. 2021 paper on diversity in policing](https://codeocean.com/capsule/8907164/tree/v1).
* [Link to Ba et al. 2021 paper](https://www.science.org/doi/10.1126/science.abd8694).

## Join officers to officer-work assignments

1. Ba et al. 2021 engage in a number of data cleaning steps related to officer-work assignments:
   1. They drop any officers (and their subsequent work assignments) who are **not White, Black, or Hispanic**.
   2. They drop any officer-work assignments where the rank of the officer is not **Police Officer**.
   3. They **claimed** to have dropped **non-standard assignments** (e.g., protest detail, station supervisor, or station security). **I have not found in the code where they do this**.
   4. They **claimed** to have dropped any work assignments in which the officer is indicated as **non-present**. I assume this is because the officer was sick, on vacation, being disciplined/rewarded, or some other reason? This may have been done in an earlier processing step that is not available in their code/data repository because all officer-work assignments have at least one associated officer. There is no column indicating if the officer was present or not.
   5. They say they do not include **non-standard watches** (i.e., any officer-work assignment which did not occur between first and third shift). This must have been done in an earlier processing step that is not available in their code/data repository because all officer-work assignments already take place between the first and third shift.
   6. They say they drop **double and triple duty** work assignments (an officer working consecutive work assignments). **I have not found in the code where they do this**.
   7. One can find their discussion of these cleaning steps on pages 7-8 of the Appendix (sections S1.4 and S1.5).
2. We engage in the following data cleaning steps:
   1. We keep officers of **all races/ethnicities**.
   2. We keep officer-work assignments of **all ranks**.
   3. We keep **non-standard assignments** due to a lack of knowledge of how to identify non-standard assignments.
   4. We implicitly drop work assignments in which the officer is indicated as **non-present** (if we are correct in assuming Ba et al. have already done this).
   5. We implicitly drop **non-standard watches** (if we are correct in assuming Ba et al. have already done this).
   6. We keep **double and triple duty** work assignments.
