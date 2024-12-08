#!/bin/bash

set -e

for chr in 1 2; do
  nvcf=`grep ^chr$chr$'\t' data/DRAGEN_population_level_number_of_vcf.txt | cut -f 2`
  index=0
  while (( $index <= $nvcf )); do
    stop=$(( $index + 1999 ))
    if (( $stop > $nvcf )); then
      stop=$nvcf
    fi
    inputfilearr=()
    inputfilearrtbi=()
    for i in `seq $index $stop`; do
      inputfilearr+=("-iin=Bulk/DRAGEN WGS/DRAGEN population level WGS variants, pVCF format [500k release]/chr$chr/ukb24310_c${chr}_b${i}_v1.vcf.gz")
      inputfilearrtbi+=("-iin=Bulk/DRAGEN WGS/DRAGEN population level WGS variants, pVCF format [500k release]/chr$chr/ukb24310_c${chr}_b${i}_v1.vcf.gz.tbi")
    done
    dx run --destination PCA_variants --instance-type mem1_ssd1_v2_x2 --priority low app-swiss-army-knife --detach -y \
      -iin="UK-Biobank-Age-Specific-Fertility-Rate/data/gnomad.v3.1.pca_loadings_loc.bed" \
      "${inputfilearr[@]}" \
      "${inputfilearrtbi[@]}" \
      -icmd="bcftools view -r gnomad.v3.1.pca_loadings_loc.bed ukb24310_c${chr}_b*_v1.vcf.gz -o variants_c${chr}_from${index}.vcf" \
      -imount_inputs=true
    index=$(( $stop + 1 ))
    false
  done
done
