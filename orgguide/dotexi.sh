#!/bin/sh
###############################################################################
# NAME: dotexi.sh
# DESCRIPTION: Script to make the final PDF file from .po file
# NOTES: Only work for this case
###############################################################################

po4a-translate -f texinfo -m orgguide.texi -p orgguide.es.po -l orgguide.es.texi
LIN_INI=`grep -n '@settitle' orgguide.es.texi | cut -d: -f1`
LIN_FIN=`grep -n '@finalout' orgguide.es.texi | cut -d: -f1`
LIN_TOT=`wc -l orgguide.es.texi | cut -d' ' -f1`
head -${LIN_INI} orgguide.es.texi > orgguide.es.tmp1
head -`echo "${LIN_FIN} - 1" | bc` orgguide.es.texi | tail -n`echo "${LIN_FIN} - ${LIN_INI} - 1" | bc`> orgguide.es.tmp2
echo "@documentencoding UTF-8" > orgguide.es.tmp3
echo "@documentlanguage es"  >> orgguide.es.tmp3
echo "@smallbook" > orgguide.es.tmp4
tail -n`echo "${LIN_TOT} - ${LIN_FIN} + 1" | bc` orgguide.es.texi > orgguide.es.tmp5
cat orgguide.es.tmp1 > orgguide.es.texi
cat orgguide.es.tmp2 >> orgguide.es.texi
cat orgguide.es.tmp3 >> orgguide.es.texi
cat orgguide.es.tmp4 >> orgguide.es.texi
cat orgguide.es.tmp5 >> orgguide.es.texi

