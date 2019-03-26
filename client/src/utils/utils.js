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

	// Decrypts using One Time Pad and password padding
	decryptPK(pk, pw) {
		console.warn('THIS METHOD HASN\'T IMPLEMENTED YET');

		// This is where the actual decryption happens
		const decrypt = function decrypt(pk, pw) {
			console.log(`utils::decrypt Decrypting ${pk} with password ${pw}`);

			return null;
		}


		// Do password padding if neccessary
		const pkLength = pk.length;
		const pwLength = pw.length;

		if (pkLength > pwLength) {
			// Do password padding
			// Repeat password until it is same length as pk
			let paddedPW = pw;
			let fills = Math.floor(pkLength / paddedPW.length);

			for (let i = 0; i < fills - 1; i++) {
				paddedPW += pw;
			}

			let diff = pk.length - paddedPW.length;

			paddedPW += pw.slice(0, diff);

			return decrypt(pk, paddedPW);
		} else if (pkLength < pwLength) {
			// Slice password
			let slicedPW = pw.slice(0, pkLength);

			return decrypt(pk, slicedPW);
		} else {
			// Same length, just decrypt
			return decrypt(pk, pw);
		}
	}
}

export default new Utils();