const Web3 = require('web3');

class Utils {
	removeTrailingZeroes(str) {
		while(str.charAt(str.length - 1) === '0') {
			str = str.slice(0, -1);
		}

		return str;
	}

	// Makes the user wait X ms before continuing (if using await)
	sleep(ms) {
		return new Promise(resolve => {
			setTimeout(() => {
				resolve();
			}, ms);
		});
	}

	// Checks if a privatekey resolves into a wallet with funds
	async isValidPK(web3, pk) {
		// Format checking
		if (!pk) return false;
		if (!pk.startsWith('0x')) return false;
		if (pk.length !== 66) return false;
	
		// Get balance
		const wallet = web3.eth.accounts.privateKeyToAccount(pk);
		const balance = await web3.eth.getBalance(wallet.address);

		return balance > 0;
	}

	// Does XOR between password hash and given data
	XOR(data, password) {
		// Error checking
		if (!data) throw new Error('data is not given!');
		if (!password) throw new Error('password is not given!');
		if (!data.startsWith('0x')) throw new Error('data is not hex');
		if (data.length !== 66) throw new Error(`invalid data length: ${data.length} (length must be 66)`);

		// Remove '0x' because we can't have it when doing bitwise operations
		data = data.replace('0x', '');
		// Do keccak-256 on password
		const pwHash = Web3.utils.sha3(password).replace('0x', '');

		// Create buffers so we can do bitwise operation
		let dataBuffer = Buffer.from(data, 'hex');
		let pwBuffer = Buffer.from(pwHash, 'hex');

		// More error checking
		if (dataBuffer.length !== 32) throw new Error(`dataBuffer length is ${dataBuffer.length} (should be 32)`);
		if (pwBuffer.length !== 32) throw new Error(`pwBuffer length is ${pwBuffer.length} (should be 32)`);

		// Create buffer where we store result
		let resultBuffer = new Buffer.alloc(32);

		// Where the magic happens
		for (var i = 0; i < 32; i++) {
			resultBuffer[i] = pwBuffer[i] ^ dataBuffer[i];
		}

		return '0x' + resultBuffer.toString('hex');
	}
}

export default new Utils();