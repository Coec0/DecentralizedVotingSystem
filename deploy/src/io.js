const fs = require('fs');
const path = require('path');
const { exec, execSync } = require('child_process');

const contractsPath = './../votingsystem';
const backendSettingsPath = './../elections.json';

function getConfig() {
	try {
		console.log('Reading config.js...');
		let config = fs.readFileSync('./config.js');
		return JSON.parse(config);
	} catch (err) {
		console.log('config.js not found, creating new config from default');
		fs.copyFileSync('./default-config.js', './config.js', fs.constants.COPYFILE_EXCL);
		return JSON.parse(fs.readFileSync('./config.js'));
	}
	
}

function getContract(contractName) {
	const path = `${contractsPath}/${contractName}`;

	console.log(`Reading ${contractName}...`);
	return fs.readFileSync(path, { encoding: 'utf8' });
}

function createBackendConfig(voterecord, votesystem, config) {
	let backendConfig = config.backend;

	backendConfig.elections.forEach(election => {
		election.nodeAddr = `ws://localhost:${config.ganache.port}`;

		election.contracts.voterecord.bcAddr = voterecord.address;
		election.contracts.voterecord.abi = voterecord.abi;

		election.contracts.votesystem.bcAddr = votesystem.address;
		election.contracts.votesystem.abi = votesystem.abi;
	});

	fs.writeFileSync(backendSettingsPath, JSON.stringify(backendConfig));
	console.log('Backend configuration file created');
}

function startBackend() {
	return new Promise((resolve, reject) =>{
		process.stdout.write('Compiling server code... ');
		const log = execSync('mvn clean install', { cwd: './../server' }).toString('utf8');
		if(!log.includes('BUILD SUCCESS')) {
			return reject(log);
		}
		console.log('Success');

		console.log('Starting server');
		const child = exec(`java -jar Server-0.6-jar-with-dependencies.jar --settings="./.${backendSettingsPath}"`, { cwd: './../server/target' });
		child.stdout.on('data', (data) => {
			process.stdout.write(data);

			if (data.includes('Started') || data.includes('Running Server')) {
				resolve();
			}
		});

		process.on('exit', () => {
			console.log('\n\nExit called, killing backend process\n\n');
			child.kill();
		});
	});
}



module.exports = { getConfig, getContract, createBackendConfig, startBackend };