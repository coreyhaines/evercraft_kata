#! /bin/sh

CABAL_NAME=EverCraft
CABAL_VER=0.1

HPC_DIR=dist/hpc/${CABAL_NAME}-${CABAL_VER}
HPC_REPO_DIR=dist/test/test-foo

TEST_DIR=test/
TIX=test-foo

TEST_MODULES=(`find ${TEST_DIR} -name "*.hs" | sed -e "s/\.hs$//g" | sed -e "s/^src\///g" | sed -e "s/\//\./g"`)

for ITEM in ${TEST_MODULES[@]}
do
    EXCLUDES="${EXCLUDES} --exclude ${ITEM}"
done

hpc report ${TIX} ${EXCLUDES} --hpcdir=${HPC_DIR} --xml-output > ${HPC_REPO_DIR}/result.xml
hpc markup ${TIX} ${EXCLUDES} --hpcdir=${HPC_DIR} --destdir=${HPC_REPO_DIR}

open ${HPC_REPO_DIR}/hpc_index.html
