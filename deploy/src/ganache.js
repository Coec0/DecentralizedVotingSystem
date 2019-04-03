const ganache = require("ganache-cli");

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

			console.log(`Ganache server up and running at ws://localhost:${config.ganache.port}`);
			resolve();
		});
		
	})
}


module.exports = { start };