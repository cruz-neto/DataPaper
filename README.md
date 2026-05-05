## Workflow overview

This repository provides the reproducible workflow used to construct the dataset described in the manuscript *Brazilian federal environmental infraction records linked to firm industry classification 1981 to 2025*.

The workflow is organized into nine main steps:

1. **IBAMA data extraction**  
   Federal environmental infraction records are retrieved from IBAMA open data services using the `Ibamam` R package. The working dataset is restricted to records involving legal entities (`CNPJ`) and to infractions dated from 1981 to 2025.

2. **Corporate identifier standardization**  
   Corporate taxpayer identifiers are converted to standardized 14-digit CNPJ strings. Non-numeric characters are removed and leading zeros are added when necessary. Additional diagnostic routines assess the structural validity of CNPJ identifiers.

3. **Corporate registry preparation**  
   Firm-level registry information is obtained from the Brazilian Federal Revenue Service CNPJ open data, accessed through Base dos Dados. The registry table is reduced to the variables required for linkage: standardized CNPJ, primary CNAE code, and secondary CNAE codes.

4. **Deterministic linkage**  
   IBAMA corporate infraction records are linked to registry-derived CNAE information using an exact match on standardized CNPJ. No probabilistic or fuzzy matching is used.

5. **CNAE hierarchy enrichment**  
   Primary CNAE codes are standardized to seven digits and linked to the official CNAE 2.0 hierarchy, adding subclass, class, group, division, and section descriptors.

6. **Municipality-level georeferencing**  
   Infraction records are joined to official Brazilian municipality identifiers and geometries using the `geobr` package. The released tabular dataset keeps municipality identifiers to allow users to reconstruct spatial layers without inflating file size.

7. **Final data assembly**  
   Variables are renamed to English, intermediate fields are removed, and a synthetic row-level identifier (`record_id`) is created to ensure deterministic identification of each observation.

8. **Technical validation**  
   Diagnostic outputs summarize CNPJ integrity, CNAE linkage coverage, temporal coverage, state-level coverage, and geographic completeness. These outputs are released alongside the primary dataset.

9. **Figures and computational metadata**  
   Scripts are provided to reproduce the manuscript figures and to export `sessionInfo.txt`, documenting the R version, operating system, and package versions used in the workflow.

