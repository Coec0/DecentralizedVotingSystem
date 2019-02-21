const path = require('path');
const fs = require('fs-extra');
const solc = require('solc');
const web3 = require('web3');

const buildPath = path.resolve('build');
const contractsPath = path.resolve('contracts');

//Check args
let errors = [];
if(!process.argv[2]) {
	errors.push('> No ethereum node specified in first argument');
}
if(!process.argv[3]) {
	errors.push('> No private key specified as second argument');
}
if(errors.length > 0) {
	console.error('Error(s) detected!');
	errors.forEach(e => console.error(e));
	return;
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

	console.time('Compilation');
	const compiledContracts = JSON.parse(solc.compile(JSON.stringify(input))).contracts;

	for (let contract in compiledContracts) {
		for(let contractName in compiledContracts[contract]) {
			fs.outputJsonSync(path.resolve(buildPath, `${contractName}.json`), compiledContracts[contract][contractName], { spaces: '\t' });
			console.log(`Compiled ${contractName}.json`);
		}
	}

	console.timeEnd('Compilation');
}

createBuildFolder();
compileContracts();
