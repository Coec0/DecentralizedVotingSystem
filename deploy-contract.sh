NODE="ws://localhost:7545"
PRIVATE_KEY="0xed75458862753c6cb536343a4e7d9cb3e3fffc5d0069720e0489c7cb27bafb9e"
CONTRACT_ARGS=""

cd votingsystem
find -iname '*.sol' -exec cp {} ../compile-deploy-smartcontract/contracts \;

cd ../compile-deploy-smartcontract/contracts
npm install
npm start $NODE $PRIVATE_KEY $CONTRACT_ARGS
exit