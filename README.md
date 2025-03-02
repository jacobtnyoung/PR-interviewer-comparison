# **[TITLE]()**

<br>

## Authors: Kevin Wright, [Jacob T.N. Young](https://jacobtnyoung.github.io/), & Alexis Klemm

### ***Abstract***

ABSTRACT.


## Overview of Repository

### Folders

There are two folders:

  * The rodeo folder-this folder contains the cleaned data and the files to render the analyses.
  
  * The wrangling folder-this contains the raw data and the scripts to prepare the data for analysis (that is, the rodeo folder).

Each folder contains a README file that describes the contents of the folder in more detail.
  
## Workflow Map

The workflow map provides a visualization of the workflow:

```mermaid
graph LR
    %% Data processing workflow
    1[Raw Data Files] -->|Processed by| A[PR-interviewer-vars-prep.R]
    A -->|Generates| B[trust.rhps.cntrls.vars.data.rds]

    %% Utility functions used by analysis scripts
    F[PR-interviewer-functions.R] -->|Used by| C[PR-interviewer-balance-var-comparison-analysis.R]
    F -->|Used by| D[PR-interviewer-demographics-analysis.R]
    F -->|Used by| E[PR-interviewer-S1Q-chisquare-analysis.R]
    F -->|Used by| G[PR-interviewer-S4Q12-analysis.R]

    %% Analysis scripts using processed data
    B -->|Used by| C[PR-interviewer-balance-var-comparison-analysis.R]
    B -->|Used by| D[PR-interviewer-demographics-analysis.R]
    B -->|Used by| E[PR-interviewer-S1Q-chisquare-analysis.R]
    B -->|Used by| G[PR-interviewer-S4Q12-analysis.R]

    %% Output generation
    C -->|Outputs| H[Figure 1 and Figure 2]
    D -->|Outputs| I[Table 1]
    E -->|Used by| J[Figure 3]

```    
