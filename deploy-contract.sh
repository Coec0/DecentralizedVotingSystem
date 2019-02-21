NODE="ws://localhost:7545"
PRIVATE_KEY=""

cd votingsystem
find -iname '*.sol' -exec cp {} ../compile-deploy-smartcontract/contracts \;

cd ../compile-deploy-smartcontract/contracts
npm install
npm start