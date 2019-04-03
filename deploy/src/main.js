const ganache = require('./ganache.js');
const deployer = require('./deployer.js');
const io = require('./io.js');
const { startDotting } = require('./utils.js');

const config = io.getConfig();

async function main() {
	console.time('Deployed in');

	await ganache.start(config);

	await deployer.init(config);
	let votingrecord = await deployer.deployRecord(config);
	let votingsystem = await deployer.deploySystem(votingrecord.address, config);

	await io.createBackendConfig(votingsystem, config);
	await io.startBackend();
	console.timeEnd('Deployed in');
	console.log();
	console.log('Running until Ctrl + C is pressed');

	startDotting();
}

try {
	main();
} catch (error) {
	console.error(error);
}

const cleanExit = function() { process.exit() };
process.on('SIGINT', cleanExit); // catch ctrl-c
process.on('SIGTERM', cleanExit); // catch kill