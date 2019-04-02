const web3 = require('web3');
const bigInt = require("big-integer");

function encrypt(message, ordG, G, B) {
	const k = web3.utils.randomHex(ordG);




}


// Calculates n * P, where P is a point
function doubleAndAdd(P, n) {
	if (!k) return 0;
	if (k === 1) return P;

	if (mod(n, 2) === 1) return 
	return
}

function pointAdd(P, Q, m) {
	const hasSameX = P.x === Q.x
	const hasOppositeY = Q.y === mod(-1*P.y, m);

	if (hasSameX && hasOppositeY) {
		// Is inverse => no point intersects => return Infinity
		return Infinity;
	}

	let s = add(P, Q, m);

	const x = mod((Math.pow(s, 2) - P.x - Q.x), m);
	const y = mod((s * (P.x - x) - P.y), m);

	return { x, y };
}

function pointDouble(P, Q, m) {
	const hasSameX = P.x === Q.x
	const hasOppositeY = Q.y === mod(-1*P.y, m);

	if (hasSameX && hasOppositeY) {
		// Is inverse => no point intersects => return Infinity
		return Infinity;
	}

	let s = double(P, Q, m);

	const x = mod((Math.pow(s, 2) - P.x - Q.x), m);
	const y = mod((s * (P.x - x) - P.y), m);

	return { x, y };
}

function findNextPoint(P, Q, m) {
	// Check if same point
	if (P.x === Q.x && P.y === Q.y) {
		// Do multiplication
		return pointDouble(P, Q, m);
	} else {
		// Do addition
		return pointAdd(P, Q, m);
	}
}

/* Helper Functions */

// Point addition
function add(P, Q, m) {
	const a = (Q.y - P.y) * modInv((Q.x - P.x), m);
	return mod(a, m);
}

// Point doubling
function double(P, Q, m) {
	const a = 3 * Math.pow(P.x, 2) + 1 * modInv(2 * P.y, m);
	return mod(a, m);
}

// Real modulo function which supports negative numbers
function mod(x, n) {
	// Error check
	if (x === null || x === undefined || n === null || n === undefined) throw new ReferenceError('missing arguments');
	// Check if both parameters are of same type
	if (typeof x !== typeof n) {
		throw new TypeError('x and n must be same type');
	}

	// They are both objects because of previous check
	if (typeof x === 'object') {
		// Check if both are bigInt
		if (!(x instanceof bigInt) || !(n instanceof bigInt)) throw new TypeError('x and n are not bigInt');

		return x.mod(n).plus(n).mod(n);
	} else {
		return (x % n + n) % n;
	}
} 

function modInv(x, n) {
	// Error check
	if (x === null || x === undefined || n === null || n === undefined) throw new ReferenceError('missing arguments');
	// Type checking
	if (!(x instanceof bigInt)) console.warn('Warning! x is not a bigInt, but it probably should be?');
	if (!(n instanceof bigInt)) console.warn('Warning! n is not a bigInt, but it probably should be?');

	return bigInt(x).modInv(bigInt(n));
}

module.exports = { findNextPoint, mod, modInv }