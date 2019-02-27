#!/bin/bash
reset
set -e

PORT="7545"
NODE="ws://localhost:$PORT"
PRIVATE_KEY="0xed75458862753c6cb536343a4e7d9cb3e3fffc5d0069720e0489c7cb27bafb9e"
CANDIDATES='["Reinfeldt", "någon annan"]'
BLOCKAMOUNT="120"

PLATFORM=""

if [ "$(uname)" == "Darwin" ]; then
	# Do something under Mac OS X platform  
	echo "Mac OS X detected"
	PLATFORM="OSX"
elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
	# Do something under GNU/Linux platform
	echo "Linux detected"
	PLATFORM="Linux"
elif [ "$(expr substr $(uname -s) 1 5)" == "MINGW" ]; then
	# Do something under Windows NT platform
	echo "Windows detected"
	PLATFORM="Windows"
fi

if ! [ -x "$(command -v node)" ]; then
  echo 'Error: node is not installed.' >&2
  exit 1
fi

if ! [ -x "$(command -v npm)" ]; then
  echo 'Error: npm is not installed.' >&2
  exit 1
fi

if ps aux | grep '[g]anache-cli' >/dev/null; then
	if [ $PLATFORM = "Windows" ]; then
		echo "Found ganache-cli running but can't terminate it! Do it manually."
		exit
	else
		echo "Ganache-cli already running, killing..."
		sudo kill $(ps aux | grep '[g]anache-cli' | awk '{print $2}')
	fi
fi

if ! [ -x "$(command -v ganache-cli)" ]; then
	echo 'Ganache-cli not detected. Installing...'
	if [ $PLATFORM = "Windows" ]; then
		npm install -g ganache-cli &> /dev/null
	else
		sudo npm install -g ganache-cli &> /dev/null
	fi
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
npm install  &> /dev/null
cd ..

echo 'Starting ganache-cli'
ganache-cli --port=$PORT --account="$PRIVATE_KEY,1000000000000000000000000" --account="0x227176555b1feee67db212e8b5d313455aa0579021e858ef94d8d4fb4f5bba57,10000000000000000000" --account="0x2e4b4b39c108ac0bc5b6fe6a5667ff9f1501dfc0132da2b78a8b9150262eaa73,1000000000000000000000" --quiet &
sleep 5

cp votingsystem/voterrecord.sol compile-deploy-smartcontract/contracts
cd compile-deploy-smartcontract
echo 'Deploying voterrecord.sol...'
echo
RECORDADDRESS=$(node src/app.js --node "$NODE" --key "$PRIVATE_KEY" --output "address" --file "voterrecord")
sleep 1

CONTRACT_ARGS="[$CANDIDATES,$BLOCKAMOUNT,\"$RECORDADDRESS\"]"

cd ..
if [ "$(ls -A compile-deploy-smartcontract/build/)" ]; then
	rm compile-deploy-smartcontract/build/*
fi
if [ "$(ls -A compile-deploy-smartcontract/contracts/)" ]; then
	rm compile-deploy-smartcontract/contracts/*
fi
sleep 1
cp votingsystem/votingsystem.sol compile-deploy-smartcontract/contracts
cd compile-deploy-smartcontract
echo 'Deploying votingsystem.sol...'
echo
VOTING=$(node src/app.js --node "$NODE" --key "$PRIVATE_KEY" --args "$CONTRACT_ARGS" --output "abi/address" --file "votingsystem")

votingarray=($VOTING)
SCADDRESS=${votingarray[0]}
ABI=${votingarray[1]}
echo $SCADDRESS
echo $ABI

exit