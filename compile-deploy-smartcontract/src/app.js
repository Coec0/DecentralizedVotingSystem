const path = require('path');
const fs = require('fs-extra');
const solc = require('solc');
const Web3 = require('web3');

const buildPath = path.resolve('build');
const contractsPath = path.resolve('contracts');

var web3 = null;
var account = null;
var smartcontractArgs = null;

const checkPrerequisits = function() {
	return new Promise(async (resolve, reject) => {
		//Check args
		let errors = [];
		if(!process.argv[2]) {
			errors.push('No ethereum node specified in first argument');
		} else {
			try {
				console.log('Connecting to node...');
				web3 = new Web3(process.argv[2]);
				let timeout = new Promise((resolve, reject) => {
					let wait = setTimeout(() => {
						resolve('timeout');
					}, 4000)
				});
				let type = web3.eth.net.getNetworkType();
				let result = await Promise.race([timeout, type]);

				if(result == 'timeout') {
					errors.push('Timed out trying to connect to node ' + process.argv[2]);
					reject(errors);
					return;
				}

				console.log(`Successfully connected to node ${process.argv[2]} of type ${result}`);
			} catch(err) {
				errors.push(err.message);
			}
		}
		if(!process.argv[3]) {
			errors.push('No private key specified as second argument');
		} else {
			try {
				if(!process.argv[3].startsWith('0x')) {
					errors.push('Invalid private key (Must be begin with "0x"): ' + process.argv[3]);
				} else {
					account = web3.eth.accounts.privateKeyToAccount(process.argv[3]);
					console.log(`Account retrieved: ${account.address}`);
					let balance = await web3.eth.getBalance(account.address);
					console.log(`Account balance: ${balance} ETH`);
					resolve();
				}	
			} catch (err) {
				errors.push(err.message);
			}
		}
		if(!process.argv[4]) {
			console.log('NOTE: No deploy arguments given');
		} else {
			smartcontractArgs = JSON.parse(process.argv[4]);

			//Convert all text found in input to hex
			let recursiveAsciiToHex = function(obj) {
				for (var key in obj)
				{
					if (!obj.hasOwnProperty(key))
						continue;
					if(typeof obj[key] == 'string') {
						obj[key] = web3.utils.asciiToHex(obj[key]);
					}
					if(typeof obj[key] == 'object') {
						recursiveAsciiToHex(smartcontractArgs[key]);
					}
				}
			}

			recursiveAsciiToHex(smartcontractArgs);
		}
		if(errors.length > 0) {
			reject(errors);
		}
	});
}

const createBuildFolder = () => {
	fs.emptyDirSync(buildPath);
}

const contractSources = function() {
	const sources = {};
	const contractsFiles = fs.readdirSync(contractsPath);

	contractsFiles.forEach(file => {
		const contractFullPath = path.resolve(contractsPath, file);
		sources[file] = {
			content: fs.readFileSync(contractFullPath, 'utf8')
		};
	});
	
	return sources;
}

const input = {
	language: 'Solidity',
	sources: contractSources(),
	settings: {
		outputSelection: {
			'*': {
				'*': [ 'abi', 'evm.bytecode' ]
			}
		}
	}
}

const compileContracts = function() {
	if(input.sources.length == 0) {
		console.log()
		console.log('No contracts found in folder');
		return;
	}

	console.time('Compiled in');
	const compiledContracts = JSON.parse(solc.compile(JSON.stringify(input))).contracts;

	for (let contract in compiledContracts) {
		for(let contractName in compiledContracts[contract]) {
			fs.outputJsonSync(path.resolve(buildPath, `${contractName}.json`), compiledContracts[contract][contractName], { spaces: '\t' });
			console.log(`Compiled ${contractName}.sol to ${contractName}.json`);
		}
	}

	console.timeEnd('Compiled in');
}

const deploy = function() {
	return new Promise(async (resolve, reject) => {
		const compiledFiles = fs.readdirSync(buildPath);


		for (var i = 0; i < compiledFiles.length; i++) {
			let file = compiledFiles[i];
			console.log(`Attempting to deploy contract ${file} from account: ${account.address}`);
			
			const filePath = path.resolve(buildPath, file);
			const contract = require(filePath);

			try  {
				const deployedContract = await new web3.eth.Contract(contract.abi)
				.deploy({
					data: '0x' + contract.evm.bytecode.object,
					arguments: [[web3.utils.asciiToHex('Test'),web3.utils.asciiToHex('1234')], 120]
				})
				.send({
					from: account.address,
					gas: '2000000'
				});

				console.log(`Contract ${file} deployed at address: ${deployedContract.options.address}`);
			} catch (err) {
				console.log(`Error deploying contract ${file}. ${err.message}`);
			}
		}

		resolve();
	});
};

checkPrerequisits().then(() => {
	createBuildFolder();
	compileContracts();
	deploy().then(() => {
		process.exit(0);
	});
}).catch((errors) => {
	console.error('Error(s) detected!');
	errors.forEach(e => console.error('> ' + e));
	process.exit(1);
});
