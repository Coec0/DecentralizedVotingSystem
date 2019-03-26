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
}

export default new Utils();