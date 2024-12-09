#!/bin/bash

set -e

for chr in {1..22}; do
  nvcf=`grep ^chr$chr$'\t' data/DRAGEN_population_level_number_of_vcf.txt | cut -f 2`
  index=0
  while (( $index <= $nvcf )); do
    stop=$(( $index + 499 ))
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
      -icmd="bcftools view --header-only ukb24310_c${chr}_b${index}_v1.vcf.gz -Oz >> variants_c${chr}_from${index}.vcf.gz; for f in ukb24310_c${chr}_b*_v1.vcf.gz; do bcftools view -H -R gnomad.v3.1.pca_loadings_loc.bed \$f -Oz >> variants_c${chr}_from${index}.vcf.gz; done" \
      -imount_inputs=true
    index=$(( $stop + 1 ))
  done
done
