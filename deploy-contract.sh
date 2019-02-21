NODE="ws://localhost:7545"
PRIVATE_KEY="0x96ae0572826a6ec70ce2906b41c7f525d6d25a12e077c0f876e0172a768bc676"
CONTRACT_ARGS='[["Kandidat_1","Kandidat_2"],120]'

set -e

if ! [ -x "$(command -v node)" ]; then
  echo 'Error: node is not installed.' >&2
  exit 1
fi

if ! [ -x "$(command -v npm)" ]; then
  echo 'Error: npm is not installed.' >&2
  exit 1
fi

if [ ! -d ./compile-deploy-smartcontract/contracts ]; then
	mkdir -p ./compile-deploy-smartcontract/contracts
fi

cd votingsystem
find . -iname '*.sol' -exec cp {} ../compile-deploy-smartcontract/contracts \;

cd ../compile-deploy-smartcontract/contracts
npm install
npm start $NODE $PRIVATE_KEY $CONTRACT_ARGS
exit