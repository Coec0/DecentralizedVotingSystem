const readline = require('readline');

let interval = null;

const dotsCount = 5;

function startDotting() {
	let i = 0;

	interval = setInterval(() => {
		process.stdout.write('.');

		if (i % (dotsCount + 1) === 0) {
			readline.clearLine(process.stdout, 0);
			readline.moveCursor(process.stdout, -1*(dotsCount+1), 0);
		} 

		i++;
		i %= (dotsCount + 1);
	}, 1000);

	process.on('exit', () => {
		if (interval) clearInterval(interval);
	});
}

module.exports = { startDotting };