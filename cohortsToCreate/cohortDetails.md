# Cohort Details for EHDEN HMB

## Target Cohorts

### HMB Cohort

#### Meta

-   **LongName**: Heavy Menstrual Bleed
-   **ShortName**: HMB
-   **FilePath**: cohortsToCreate/01_target/hmb.json
-   **Id**: 1
-   **Authors**: Martin Lavallee and Asieh Golozar
-   Developed using ATLAS

#### Description

This cohort is meant to serve as the target population for the EHDEN HMB network study. This cohort captures pre-menopausal women who experience an event of heavy-menstrual bleeding. Women are excluded from this cohort if they had a prior hysterectomy or bilateral ovariectomy, menopause or other gynecological bleeding. Women exit the cohort if on of the exclusion criteria is experienced.

#### Log

**Version 4: 06/19/2023**

-   Simplification of Concept Sets:
    -   HMB: remove abnormal uterine bleeding
    -   bilateral ovariectomy: only use 4297990 Bilateral oophorectomy and descendants. remove other codes
    -   menopause: remove after menopause concept
-   Ensure that inclusion criteria counts both instances:
    -   having **all** menopause condition and menopause observation
    -   having **all** hysterectomy and bilateral ovariectomy
-   Add index start date range. Observe HMB on or after 2000-01-01

**Version 3: 06/15/2023**

-   Fix typo in inclusion rules for menopause. Exactly 0 occurrences of condition/observation of menopause between any time before and 0 days after

**Version 2: 06/01/2023**

-   Use phenotype library [HMB cohort](https://github.com/OHDSI/PhenotypeLibrary/blob/main/inst/cohorts/300.json) as a template for design
-   Simplify Concept Sets:
    -   HMB: use phenotype library concept set. Use source concepts for ICD9 and ICD10
    -   Hysterectomy: use single code with hierarchy
    -   Menopause: set of codes looking for any diagnosis of menopause (natural or medical). Include observation table
-   Ensure all inclusion criteria are retrospective
    -   other gynelogical bleed considers all time after, which was removed
-   Change censoring for age to be based on visit domain

**Version 1: Prior**

-   Developed by IQVIA, insufficient review of cohort definition and concept sets
-   Added ICD9 626.2 for excessive and frequent menstruation
-   Censoring based on age based on measurement domain (visit domain not required by IQVIA)
