#!/bin/bash
PORT="7545"
NODE="ws://localhost:$PORT"
PRIVATE_KEY="0xed75458862753c6cb536343a4e7d9cb3e3fffc5d0069720e0489c7cb27bafb9e"
CANDIDATES='["Reinfeldt", "någon annan"]'
BLOCKAMOUNT="120"

clear
set -e

if ps aux | grep '[g]anache-cli' >/dev/null; then
    echo "Ganache-cli already running, killing..."
    sudo kill $(ps aux | grep '[g]anache-cli' | awk '{print $2}')
fi

if ! [ -x "$(command -v node)" ]; then
  echo 'Error: node is not installed.' >&2
  exit 1
fi

if ! [ -x "$(command -v npm)" ]; then
  echo 'Error: npm is not installed.' >&2
  exit 1
fi

if ! [ -x "$(command -v ganache-cli)" ]; then
	echo 'Installing ganache-cli...'
	sudo npm install -g ganache-cli
fi

if [ ! -d ./compile-deploy-smartcontract/contracts ]; then
	mkdir -p ./compile-deploy-smartcontract/contracts
fi

echo 'Clearing old files...'
if [ "$(ls -A compile-deploy-smartcontract/build/)" ]; then
	rm compile-deploy-smartcontract/build/*
fi
if [ "$(ls -A compile-deploy-smartcontract/contracts/)" ]; then
	rm compile-deploy-smartcontract/contracts/*
fi
sleep 2

echo 'Installing compile-deploy-smartcontract'
cd compile-deploy-smartcontract
npm install
echo 'Installed'
cd ..

echo 'Starting ganache-cli'
ganache-cli --port=$PORT --account="$PRIVATE_KEY,1000000000000000000000000" --account="0x227176555b1feee67db212e8b5d313455aa0579021e858ef94d8d4fb4f5bba57,10000000000000000000" --account="0x2e4b4b39c108ac0bc5b6fe6a5667ff9f1501dfc0132da2b78a8b9150262eaa73,1000000000000000000000" --quiet &
sleep 5
echo

cp votingsystem/voterrecord.sol compile-deploy-smartcontract/contracts
cd compile-deploy-smartcontract
echo 'Deploying voterrecord.sol...'
RECORDADDRESS=$(node src/app.js --node "$NODE" --key "$PRIVATE_KEY" --output "address" --file "voterrecord")
sleep 2

CONTRACT_ARGS="[$CANDIDATES,$BLOCKAMOUNT,\"$RECORDADDRESS\"]"

cd ..
if [ "$(ls -A compile-deploy-smartcontract/build/)" ]; then
	rm compile-deploy-smartcontract/build/*
fi
if [ "$(ls -A compile-deploy-smartcontract/contracts/)" ]; then
	rm compile-deploy-smartcontract/contracts/*
fi
sleep 2
cp votingsystem/votingsystem.sol compile-deploy-smartcontract/contracts
cd compile-deploy-smartcontract
echo 'Deploying votingsystem.sol...'
echo
VOTING=$(node src/app.js --node "$NODE" --key "$PRIVATE_KEY" --args "$CONTRACT_ARGS" --output "abi/address" --file "votingsystem")

votingarray=($VOTING)
VOTINGADDRESS=${votingarray[0]}
VOTINGABI=${votingarray[1]}
echo $VOTINGADDRESS
echo $VOTINGABI

exit