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

**Version 7: 07/19/2023** - Changed inclusion criteria to all events from earliest event - Add exclusion criterion: Exactly 0 occurrences of postcoital bleeding 365 days before and 0 days after index date - Allow events outside of the observation period for bilateral ovariectomy and hysterectomy - Add exclusion criterion: Exactly 0 condition occurrence of pregnancy 180 days before and 0 days after index date - Changed menopause concept set to combine with PhenotypeLibrary expression and iVMS concept set from CHAPTER: Remove ‘atrophic vaginitis’ and ‘menopause monitoring status’, add ‘postmenopausal hormone replacement therapy’, ‘after menopause’, ‘postartificial menopausal syndrome’. Allow events outside of the observation period. - Add exclusion criterion: Exactly 0 condition occurrences of ovarian or uterine cancer all days before and 0 days after the index date. Allow events outside of the observation period. - Add censoring criteria: Uterine/ovarian cancer, pregnancy, add procedure occurrence based on the new concept set for menopause

**Version 6: 07/05/2023**

-   Remove unused concept sets from json to avoid records in cohort diagnostics (breast cancer)

**Version 5: 06/26/2023**

-   Add *First time in patient history* for HMB index

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

## Drug Exposure Cohorts

### Description

-   **FilePath**: cohortsToCreate/04_drugExposure
-   **Authors**: Martin Lavallee
-   Developed using Capr

All exposure cohort follow the same skeletal logic:

-   Index event: a drug exposure with at least 365 days of prior observation.
-   Exit: based on persistence to the index drug exposure and censor for when patients turn 55 based on a visit occurrence or have a hysterectomy or menopause
-   Era: collapse drug eras based on 30 day gaps

The purpose of these cohorts are for the treatment landscape analysis. We look at the prevalence of the drug post-index, the sequences of the drugs throughout the patient history and the time the patient remains on a particular drug line. We align these drug eras to determine therapy lines that have a combination of treatments.

List of Drug Exposure Cohorts:

-   Tranexamic acid (B02AA02)
-   Progestin only regimens:
    -   Medroxyprogesterone acetate (MPA) (G03AC06)
    -   Oral norethindrone acetate (NETA) (G03DC02)
    -   Desogestrel (G03AC09)
    -   Etonogestrel implant (G03AC08)
-   Non-steroidal anti-inflammatory drugs (NSAIDs) (M01A)
-   Combined oral hormonal contraceptives
    -   Dienogest and estradiol (G03AB08; sequential combinations)
    -   Nomegestrol and estradiol (G03AA14; fixed combinations)
-   Selective progesteron receptor modulators $$Ulipristal acetate (G03AD02, G03XB02)$$
-   Danazol (G03XA01)
-   Gonadotropin releasing hormone analogues (L02AE)
-   Intrauterine devices (G02BA)
-   Iron preparations (B03A)

#### Log

**Version 1: 07/20/2023**

-   Use Capr template to build drug eras based on 30 days of presistent exposure.
-   Drug concepts are based on ATC classifications with descendants

## Procedure Cohorts

### Description

-   **FilePath**: cohortsToCreate/05_procedures
-   **Authors**: Martin Lavallee
-   Developed using Capr

All procedure cohorts follow the same skeletal logic:

-   Index event: a procedure occurrence with at least 365 days of prior observation.
-   Exit: on the day of the procedure occurrence

The purpose of these cohorts are for the prevalence and time to event of analysis for procedures.

List of Procedure Cohorts:

-   Hysterectomy (ID: 4127886)
-   Endometrial ablation (ID: 4141940)
-   Uterine artery embolization (UAE) (ID: 4193984)
-   Myomectomy (ID: 4169931)

#### Log

**Version 1: 07/20/2023**

-   Use Capr template to build procedure cohorts were exit is on day of procedure.
-   Procedure concepts are based on SNOMED of procedure with descendants
