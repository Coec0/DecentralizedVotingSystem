class Utils {
	removeTrailingZeroes(str) {
		while(str.charAt(str.length - 1) === '0') {
			str = str.slice(0, -1);
		}

		return str;
	}
}

export default new Utils();