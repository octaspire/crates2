#!/bin/bash

if [ "$#" -ne 2 ]; then
    echo "Two file names expected"
    exit 2
fi

INFILE=$1
OUTFILE=$2
OUTFILE_TMP=${OUTFILE%.*}.tmp

echo "(in-package :crates2-ui)" > ${OUTFILE_TMP}
echo "(defparameter *${INFILE##*/}* #(" >> ${OUTFILE_TMP}
hexdump -v -e'1/1 " #x%02X"' ${INFILE} >> ${OUTFILE_TMP}
echo ") \"${INFILE}\")" >> ${OUTFILE_TMP}
#paste -d' ' -s ${OUTFILE_TMP} > ${OUTFILE}
cp ${OUTFILE_TMP} ${OUTFILE}
rm ${OUTFILE_TMP}