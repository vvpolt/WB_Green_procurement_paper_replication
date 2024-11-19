# WB Green Procurement Paper Replication

This repository contains the replication materials (datasets and scripts) for the **World Bank Green Procurement Paper**.

# Data Availability Statement

The following datasets could be used under the following creative commons rights: https://creativecommons.org/licenses/by-nc/4.0/
- **BG_AOP_data_20220228.json.tar**
- **BG_EOP_data_20220222**
- **bg_cri_20220317_processed**
- **bulgarian_stopwords.txt**
- **green_procurement_keywords.csv**
  
The following dataset cannot be published or shared publicly in any form due to the presence of company data information in it purchased from Orbis
- **bg_cri_processed_green_pp20220426**
  
## Datasets

The datasets used in the scripts are available at the following link: [Figshare Repository](https://figshare.com/s/2db8e7d611f73d827b49). Below is a description of the datasets:

- **BG_AOP_data_20220228.json.tar**: Dataset used in the Python script, one of the two source datasets for Bulgarian public procurement.
- **BG_EOP_data_20220222**: Dataset used in the Python script, the second of the two source datasets for Bulgarian public procurement.
- **bg_cri_20220317_processed**: Processed and cleaned public procurement dataset used in the Python script. The dataset was prepared using a data pipeline.
- **bg_cri_processed_green_pp20220426**: Enhanced version of the processed public procurement dataset. This dataset includes political connections and flags green contracts. It is used in both Python and R scripts.
- **bulgarian_stopwords.txt**: List of stop words used for identifying green procurement in text objects. This is used in the Python script.
- **green_procurement_keywords.csv**: List of keywords used to identify green procurement in text objects. This is also used in the Python script.

## Scripts

### Python Script

The script `WB_green_paper_replication_code_figure1_3.ipynb` generates **Figures 1–3** in the paper. Each figure is explicitly labeled in the code (e.g., *Figure 1: Share of GPP identified in text descriptions of procurement and CPVs*).

- **Requirements**:  
  - Update folder paths in the script to match your local setup.
  - Python 3 is required to run the code.
  - All relevant libraries are imported within the script.

### R Script

The script `WB_green_replication_code_models.R` generates variables for analysis and runs the regression models. It is also used to create **Tables 1–4** in the paper, leveraging the `stargazer` library. 

- **Dataset**: The script uses the `bg_cri_processed_green_pp20220426` dataset, with variables generated at the beginning of the script (lines 17–200).
- **Table Breakdown**:
  - **Table 1**:  
    - Model script: lines 215–252.  
    - Table generation: lines 262–348 and 392–420.
  - **Table 2**:  
    - Matching: lines 449–467.  
    - Models: lines 559–569.  
    - Table generation: lines 572–671.
  - **Table 3**:  
    - Model script: lines 202–2010.  
    - Table generation: lines 350–387 and 422–447.
  - **Table 4**:  
    - Matching: lines 449–467.  
    - Models: lines 468–498.  
    - Table generation: lines 501–557.

