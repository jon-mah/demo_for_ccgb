# Scripts

This directory includes all scripts and code used for plotting and analysis.

* `compute_pi.py`

This script computes the pi matrix from a given `*snps_ref_freq.txt.bz2`, and requires three input arguments:

    * `input_depth`: Annotated SNPs document in the format output by Midas
    * `input_ref_freq`: Reference SNP frequency document in the format output by Midas.
    * `outprefix`: Prefix of output directory

As output this script writes a `.csv` file to the output directory  which describes summary statistics concerning pi, e.g., the site id, the average pi, and the sequencing depth.


* `compute_SNP_matrix.py`

This script computes the SNP matrix from a given `*annotated_snps.txt.bz2`, and requires two input arguments:

    * `input_snps`: path to the `*annotated_snps.txt.bz2` file.
    * `outprefix`: Prefix of output directory.

As output, this script a `*.snp_matrix.csv` file in the format required by MIDAS.  


* `dfe_comparison.R`

This file contains several scripts written in R to plot, compare, and analyze DFE's, i.e., distributions of fitness effects.

* `fit_demographic_model.py`

This script uses `Dadi` to fit demographic models, and requires three input arguments and two optional arguments:

    * `syn_input_sfs`: Text file containing an SFS of the synonymous mutations present in the species for which we are fitting a demographic model.
    * `nonsyn_input_sfs`: Dummy input argument to be inherited by other scripts
    * `outprefix`: Prefix of output directory.
    * `mask_singletons`: Optional $BOOLEAN argument for whether or not to mask singletons.
    * `mask_doubletons`: Optional $BOOLEAN argument for whether or not to mask doubletons.

* `fit_demographic_model_and_DFE.py`

    * `syn_input_sfs`: Text file containing an SFS of the synonymous mutations present in the species for which we are fitting a demographic model.
    * `nonsyn_input_sfs`: Text file containing an SFS of the nonsynonymous mutations present in the species for which we are fitting a demographic model.
    * `outprefix`: Prefix of output directory.
    * `mask_singletons`: Optional $BOOLEAN argument for whether or not to mask singletons.
    * `mask_doubletons`: Optional $BOOLEAN argument for whether or not to mask doubletons.

* `fit_one_epoch.py`

    * `syn_input_sfs`: Text file containing an SFS of the synonymous mutations present in the species for which we are fitting a demographic model.
    * `nonsyn_input_sfs`: Text file containing an SFS of the nonsynonymous mutations present in the species for which we are fitting a demographic model.
    * `outprefix`: Prefix of output directory.
    * `mask_singletons`: Optional $BOOLEAN argument for whether or not to mask singletons.
    * `mask_doubletons`: Optional $BOOLEAN argument for whether or not to mask doubletons.

* `Selection.py`

Import script for fitdadi to work.

* `summarize_pi.py`
