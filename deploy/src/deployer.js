const solc = require('solc');
const Web3 = require('web3');

const io = require('./io.js');

let web3;

async function init(config) {
	web3 = new Web3(`ws://localhost:${config.ganache.port}`);
}

async function deployRecord(config) {
	console.time(`Compiled ${config.record.name} in`);
	const rawContract = io.getContract(config.record.filename);

	const input = {
		language: 'Solidity',
		sources: { },
		settings: {
			outputSelection: {
				'*': {
					'*': [ 'abi', 'evm.bytecode' ]
				}
			}
		}
	}
	input.sources[config.record.name] = { content: rawContract };
	const compiledContract = JSON.parse(solc.compile(JSON.stringify(input))).contracts[config.record.name][config.record.name];
	console.timeEnd(`Compiled ${config.record.name} in`);

	const account = web3.eth.accounts.privateKeyToAccount(config.ganache.accounts[0].secretKey);

	// Get contract parameters from config
	let arguments = [];
	arguments.push(config.record.args.admins);

	console.log(`Attempting to deploy ${config.record.name} from account: ${account.address}`);
	const contract = await new web3.eth.Contract(compiledContract.abi)
	.deploy({
		data: '0x' + compiledContract.evm.bytecode.object,
		arguments
	})
	.send({
		from: account.address,
		gas: '6721975'
	});
	console.log(`Contract ${config.record.name} successfully deployed at address: ${contract.options.address}`);

	return { address: contract.options.address, abi: compiledContract.abi };
}

async function deploySystem(recordAddress, config) {
	console.time(`Compiled ${config.system.name} in`);
	const rawContract = io.getContract(config.system.filename);

	const input = {
		language: 'Solidity',
		sources: { },
		settings: {
			outputSelection: {
				'*': {
					'*': [ 'abi', 'evm.bytecode' ]
				}
			}
		}
	}
	input.sources[config.system.name] = { content: rawContract };
	const compiledContract = JSON.parse(solc.compile(JSON.stringify(input))).contracts[config.system.name][config.system.name];
	console.timeEnd(`Compiled ${config.system.name} in`);

	const account = web3.eth.accounts.privateKeyToAccount(config.ganache.accounts[0].secretKey);

	// Make input compatible first
	let arguments = [];

	// Candidates
	arguments.push(config.system.args.candidates);
	arguments[0].forEach((candidate, index) => {
		arguments[0][index] = web3.utils.asciiToHex(candidate);
	});

	// Blocks until start
	arguments.push(config.system.args.blocksUntilStart);

	// Blocks until end
	arguments.push(config.system.args.blocksUntilEnd);

	// Voterecord address
	arguments.push(recordAddress);

	// Admins
	arguments.push(config.system.args.admins);

	// a
	arguments.push(config.system.args.a);

	// b
	arguments.push(config.system.args.b);
	
	// p
	arguments.push(config.system.args.p);
	
	// q
	arguments.push(config.system.args.q);
	
	// gx
	arguments.push(config.system.args.gx);
	
	// gy
	arguments.push(config.system.args.gy);
	
	// bx
	arguments.push(config.system.args.bx);
	
	// by
	arguments.push(config.system.args.by);

	console.log(`Attempting to deploy ${config.system.name} from account: ${account.address}`);
	const contract = await new web3.eth.Contract(compiledContract.abi)
	.deploy({
		data: '0x' + compiledContract.evm.bytecode.object,
		arguments: arguments
	})
	.send({
		from: account.address,
		gas: '6721975'
	});
	console.log(`Contract ${config.system.name} successfully deployed at address: ${contract.options.address}`);

	return { address: contract.options.address, abi: compiledContract.abi };
}

module.exports = { init, deployRecord, deploySystem };
