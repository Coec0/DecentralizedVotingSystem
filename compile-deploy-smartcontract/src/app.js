const path = require('path');
const fs = require('fs-extra');
const solc = require('solc');
const Web3 = require('web3');
const ArgumentParser = require('argparse').ArgumentParser;
const parser = new ArgumentParser({
	version: '0.0.1',
	addHelp: true,
	description: ''
});

parser.addArgument(
	[ '-n', '--node' ],
	{
		help: 'URL to node, ex: ws://localhost:7545'
	}
);
parser.addArgument(
	[ '-k', '--key' ],
	{
		help: 'Private key for wallet that deploys. Must begin with 0x'
	}
);
parser.addArgument(
	[ '-a', '--args' ],
	{
		help: 'The arguments to use for the contract when deploying, ex: "["1234", ["Kandidat 1", "Kandidat 2"], 120]"'
	}
);
parser.addArgument(
	'--debug',
	{
		help: 'Disables debug logging and only outputs address of contract'
	}
);
parser.addArgument(
	'--output',
	{
		help: 'Can be set to abi, address or abi/address'
	}
);

parser.addArgument(
	'--file',
	{
		help: 'Specify filename in contracts to compile and deploy'
	}
);

parser.addArgument(
	'--fo',
	{
		help: 'Specify filename for output file'
	}
);

let args = parser.parseArgs();

const buildPath = path.resolve('build');
const contractsPath = path.resolve('contracts');

var web3 = null;
var account = null;
var smartcontractArgs = null;

const checkPrerequisits = function() {
	return new Promise(async (resolve, reject) => {
		//Check args
		let errors = [];
		if(!args.node) {
			errors.push('No ethereum node specified in first argument');
		} else {
			try {
				if(args.debug) console.log('Connecting to node...');
				web3 = new Web3(args.node);
				let timeout = new Promise((resolve, reject) => {
					let wait = setTimeout(() => {
						resolve('timeout');
					}, 15*1000)
				});
				let type = web3.eth.net.getNetworkType();
				let result = await Promise.race([timeout, type]);

				if(result === 'timeout') {
					errors.push('Timed out trying to connect to node ' + args.node);
					return reject(errors);
				}

				if(args.debug) console.log(`Successfully connected to node ${args.node} of type ${result}`);
			} catch(err) {
				errors.push(err.message);
			}
		}
		if(!args.key) {
			errors.push('No private key specified as second argument');
		} else if(web3) {
			try {
				if(!args.key.startsWith('0x')) {
					errors.push('Invalid private key (Must be begin with "0x"): ' + args.key);
				} else {
					account = web3.eth.accounts.privateKeyToAccount(args.key);
					if(args.debug) console.log(`Account retrieved: ${account.address}`);
					let balance = await web3.eth.getBalance(account.address);
					if(args.debug) console.log(`Account balance: ${balance} ETH`);
					resolve();
				}	
			} catch (err) {
				errors.push(err.message);
			}
		}
		if(!args.args) {
			if(args.debug) console.log('NOTE: No deploy arguments given');
		} else if(web3) {
			smartcontractArgs = JSON.parse(args.args);

			//Convert all text found in input to hex
			let recursiveAsciiToHex = function(obj) {
				for (var key in obj)
				{
					if (!obj.hasOwnProperty(key))
						continue;
					if(typeof obj[key] == 'string') {
						if(obj[key].startsWith('0x'))
							continue;
						obj[key] = web3.utils.asciiToHex(obj[key].replace('_', ' '));
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
		if(args.file && !file.toLowerCase().includes(args.file))
			return;

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
		console.error('No contracts found in folder');
		return;
	}

	if(args.debug) console.time('Compiled in');
	const compiledContracts = JSON.parse(solc.compile(JSON.stringify(input))).contracts;

	for (let contract in compiledContracts) {
		for(let contractName in compiledContracts[contract]) {
			fs.outputJsonSync(path.resolve(buildPath, `${contractName}.json`), compiledContracts[contract][contractName], { spaces: '\t' });
			if(args.debug) console.log(`Compiled ${contractName}.sol to ${contractName}.json`);
		}
	}

	if(args.debug) console.timeEnd('Compiled in');
}

const deploy = function() {
	return new Promise(async (resolve, reject) => {
		const compiledFiles = fs.readdirSync(buildPath);


		for (var i = 0; i < compiledFiles.length; i++) {
			let file = compiledFiles[i];
			if(args.file && !file.toLowerCase().includes(args.file)) {
				continue;
			}
			if(args.debug) console.log(`Attempting to deploy contract ${file} from account: ${account.address}`);
			
			const filePath = path.resolve(buildPath, file);
			const contract = require(filePath);

			try  {
				if(args.debug) console.log(smartcontractArgs)
				const deployedContract = await new web3.eth.Contract(contract.abi)
				.deploy({
					data: '0x' + contract.evm.bytecode.object,
					arguments: smartcontractArgs
				})
				.send({
					from: account.address,
					gas: '6721975'
				});

				if(args.debug) {
					console.log(`Contract ${file} deployed at address: ${deployedContract.options.address}`);
					console.log(`ABI: \n\n ${JSON.stringify(contract.abi)} \n\n`);
				}
				if(args.output.includes('address')) console.log(`${deployedContract.options.address}`);
				if(args.output.includes('abi')) console.log(`${JSON.stringify(contract.abi)}`);

				if(args.fo) {	// File output
					let output = {
						port: 8080,
						contracts: [
							{
								id: 1,
								name: 'Kommunalval',
								nodeAddr: null,
								bcAddr: null,
								abi: null
							},
							{
								id: 2,
								name: 'Landstingsval',
								nodeAddr: null,
								bcAddr: null,
								abi: null
							},
							{
								id: 3,
								name: 'Riksdagsval',
								nodeAddr: args.node,
								bcAddr: deployedContract.options.address,
								abi: contract.abi
							}
						]
					};

					fs.writeFileSync(args.fo, JSON.stringify(output));
				}
			
			} catch (err) {
				console.error(err.message);
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
	}).catch(err => console.error);
}).catch((errors) => {
	console.error('Error(s) detected!');
	errors.forEach(e => console.error('> ' + e));
	process.exit(1);
});
