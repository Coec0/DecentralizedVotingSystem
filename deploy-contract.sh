NODE="ws://localhost:7545"
PRIVATE_KEY="0xed75458862753c6cb536343a4e7d9cb3e3fffc5d0069720e0489c7cb27bafb9e"
CONTRACT_ARGS=""

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