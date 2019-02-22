NODE="ws://localhost:7545"
PRIVATE_KEY="0xed75458862753c6cb536343a4e7d9cb3e3fffc5d0069720e0489c7cb27bafb9e"
CONTRACT_ARGS='[["Kandidat 1", "Kandidat 2"],120]'

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

printf "Please select file:\n"
select FILENAME in *.sol;
do
     echo "Compiling $FILENAME ($REPLY)."
     cp $FILENAME ../compile-deploy-smartcontract/contracts
     break
done

cd ../compile-deploy-smartcontract/contracts
npm install
npm start -- --node "$NODE" --key "$PRIVATE_KEY" --args "$CONTRACT_ARGS"
exit