#! /usr/bin/env bash

set -euo pipefail

LANG=(
  br_full
  bs_50k
  ca_50k
  cs_50k
  da_50k
  de_50k
  en_50k
  eo_full
  es_50k
  et_50k
  eu_50k
  fi_50k
  fr_50k
  gl_50k
  hr_50k
  hu_50k
  id_50k
  is_50k
  it_50k
  lt_50k
  lv_50k
  ms_50k
  nl_50k
  no_50k
  pl_50k
  pt_50k
  ro_50k
  sk_50k
  sl_50k
  sq_50k
  sr_50k
  sv_50k
  tl_50k
  tr_50k
  vi_50k
)

mkdir -p words
for i in ${LANG[@]}; do
  CODE="${i:0:2}"
  LINK="${CODE}_50k"
  if [ "$CODE" = "br" ] || [ "$CODE" = "eo" ] || [ "$CODE" = "tl" ]; then
    LINK="${CODE}_full"
  fi
  DEST="words/${CODE}.txt"
  if [ -f "words/${CODE}.txt" ]; then
    echo "$DEST already exists, skipping"
  else
    echo "Downloading $DEST..."
    URL="https://raw.githubusercontent.com/hermitdave/FrequencyWords/master/content/2018/${CODE}/${LINK}.txt"
    curl -s "$URL" | awk 'NF==2{print $1}NF!=2{print $0; exit 1}' > "$DEST"
    sleep 1
  fi
done

echo 'Done'
