const ganache = require("ganache-cli");
const Web3 = new require('web3');

function start(config) {
	return new Promise((resolve, reject) => {
		console.log(`Starting ganache server...`);

		let server = ganache.server(config.ganache);

		server.on('error', (err) => {
			if (err.message.includes('EADDRINUSE')) {
				console.error(`Port ${config.ganache.port} already in use, end other process first (possibly another instance running?)`);
			} else {
				console.error(err);
			}
		});

		server.listen(config.ganache.port, (err, blockchain) => {
			if (err) return reject(err);

			const web3 = new Web3(`ws://localhost:${config.ganache.port}`);

			console.log(`Ganache server up and running at ws://localhost:${config.ganache.port}`);
			console.log('---------------------------- Accounts ---------------------------------');
			Object.values(blockchain.unlocked_accounts).forEach((account, index) => {
				console.log('                             Account ' + index);
				console.log('Address:    ' + account.address);
				console.log('Secret key: 0x' + account.secretKey.toString('hex'));
				console.log('Balance:    ' + web3.utils.fromWei(web3.utils.toBN(account.account.balance.toString('hex'))) + ' Ether');
			});
			console.log('-----------------------------------------------------------------------');
			resolve();
		});
		
	})
}


module.exports = { start };