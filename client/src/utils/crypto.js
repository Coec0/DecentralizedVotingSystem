const web3 = require('web3');
const bigInt = require("big-integer");

const IDENTITY = bigInt.minusOne;

/**
 * @param  {Integer} 	message		The message to be encrypted
 * @param  {Integer} 	ordG		The order of the generator
 * @param  {Point}		G 			The generator for the encryption
 * @param  {Point}		B 			The public key
 * @param  {Integer}	p 			The prime used as modulo
 * @param  {Integer}	A 			The a-value of the curve
 * @return {Point}					The point represnting the encrypted data.
 */
function encrypt(message, ordG, G, B, p, A) {
	// Check all parameters exist
	if (message === null || message === undefined || ordG === null || ordG === undefined || G === null || G === undefined || B === null || B === undefined || p === null || p === undefined || A === null || A === undefined) {
		throw new ReferenceError('missing argument');
	}
	// Check so that everything is bigInts
	checkBigInts(message, ordG, G, B, p, A);

	// Check message is smaller than ordG
	if (!message.lesser(ordG)) throw new RangeError(`message is not less than ord (m=${message} ordG=${ordG})`);

	// .plus(1).mod(ordG) to avoid getting zero
	const random = bigInt(web3.utils.randomHex(32).replace('0x', ''), 16).mod(ordG).plus(1).mod(ordG);

	const B1 = doubleAndAdd(G, random, A, p);
	const B2_1 = doubleAndAdd(G, message, A, p);
	const B2_2 = doubleAndAdd(B, random, A, p);
	const B2 = findNextPoint(B2_1, B2_2, A, p);

	return [ B1, B2, random ];
}

/**
 * Calculates n * P using double-and-add method
 * https://en.wikipedia.org/wiki/Elliptic_curve_point_multiplication#Double-and-add
 * @param  {Point} P   The point to be mutlipled
 * @param  {Integer} n The scalar value
 * @param  {Integer} m The modulo of the current curve
 * @return {Point}     The resulting point
 */
function doubleAndAdd(P, n, a, m) {
	checkBigInts(P, n, a, m);
	if (n.equals(bigInt.zero)) return IDENTITY;
	if (n.equals(bigInt.one)) return P;

	if (mod(n, bigInt(2)).equals(bigInt.one)) return pointAdd(P, doubleAndAdd(P, n.minus(1), a, m), m);

	return doubleAndAdd(pointDouble(P, P, a, m), n.divide(2), a, m);
}

/**
 * Finds next point given two arbitrary points using either point addition or point doubling
 * @param  {Point} P   The first point
 * @param  {Point} Q   The second point
 * @param  {Integer} m The modulo of the current curve
 * @return {Point}     The resulting point
 */
function findNextPoint(P, Q, a, m) {
	checkBigInts(P, Q, a, m);

	if (P === IDENTITY && Q === IDENTITY) return IDENTITY;
	if (P === IDENTITY) return Q;
	if (Q === IDENTITY) return P;

	// Check if same point
	if (P.x.equals(Q.x) && P.y.equals(Q.y)) {
		// Do multiplication
		return pointDouble(P, Q, a, m);
	} else {
		// Do addition
		return pointAdd(P, Q, m);
	}
}

/**
 * Finds next point from two distinguished points (including reflection)
 * @param  {Point} P The first point
 * @param  {Point} Q The second point
 * @param  {Integer} m The modulo of the current curve
 * @return {Point} Resulting point
 */
function pointAdd(P, Q, m) {
	const hasSameX = P.x.equals(Q.x);
	const hasOppositeY = Q.y.equals(mod(P.y.times(-1), m));

	if (hasSameX && hasOppositeY) {
		// Is inverse => no point intersects => return Infinity
		return IDENTITY;
	}

	const lambda = calcAddLambda(P, Q, m);
	const x = mod(lambda.pow(2).minus(P.x).minus(Q.x), m);
	const y = mod(lambda.times(P.x.minus(x)).minus(P.y), m);

	// const x = mod((Math.pow(lamba, 2) - P.x - Q.x), m);
	// const y = mod((lambda * (P.x - x) - P.y), m);

	return { x, y };
}

/**
 * Finds the next point from two identical points point (including reflection)
 * @param  {Point} P The only point which is doubled to get the next point
 * @param  {Point} Q  This is only included for some testing support. Should be identical to P
 * @param  {Integer} a The a-value of the current curve
 * @param  {Integer} m The modulo of the current curve
 * @return {Point} Resulting point
 */
function pointDouble(P, Q, a, m) {
	// Do some minor parameter checking first
	if (!(P.x.equals(Q.x) && P.y.equals(Q.y))) console.warn('pointDouble called with two NOT IDENTICAL points, is this really intended?');

	const lambda = calcDoubleLambda(P, a, m);
	const x = mod(lambda.pow(2).minus(P.x).minus(Q.x), m);
	const y = mod(lambda.times(P.x.minus(x)).minus(P.y), m);

	// const x = mod((Math.pow(lamba, 2) - P.x - Q.x), m);
	// const y = mod((lambda * (P.x - x) - P.y), m);

	return { x, y };
}


/*
	-----------------------------------------------------
					Helper Functions
	-----------------------------------------------------
 */


/**
 * Calculates the lambda used in calculating point addition
 * @param  {Point} P   The first point
 * @param  {Point} Q   The second point
 * @param  {Integer} m The modulo of the current curve
 * @return {Integer}   The lambda value for the two points
 */
function calcAddLambda(P, Q, m) {
	const lambda = Q.y.minus(P.y).times(Q.x.minus(P.x).modInv(m));
	return mod(lambda, m);
}

/**
 * Calculates the lambda used for point doubling
 * @param  {Point} P   The first point
 * @param  {Integer} a The 'a'-value for the curve used
 * @param  {Integer} m The modulo of the current curve
 * @return {Integer}   The lambda value for the point
 */
function calcDoubleLambda(P, a, m) {
	const lambda = P.x.pow(2).times(3).plus(a).times(P.y.times(2).modInv(m))
	return mod(lambda, m);
}

/**
 * Real modulo function which supports negative numbers (Supports both bigInts and Integers)
 * @param  {Integer} x The value to be modulo-ed
 * @param  {Integer} n The modulo value
 * @return {Integer}   The result
 */
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
		if (!(x instanceof bigInt) || !(n instanceof bigInt)) throw new TypeError('x and n are not bigInts');

		return x.mod(n).plus(n).mod(n);
	} else {
		return (x % n + n) % n;
	}
} 

// This functions takes any amount of arguments and check if they are either bigInts or objects which contain bigInts
// Will throw an error if something that is not a bigInt is found
// Not fully tested for now
function checkBigInts(...args) {
	for (var i = args.length - 1; i >= 0; i--) {
		let arg = args[i];

		if (arg instanceof bigInt) {
			continue;
		}

		if (typeof arg === 'object') {
			checkBigInts(...Object.values(arg));
			continue;
		}

		throw new TypeError(`object contains something that is not a bigInt (${arg} is of type ${typeof arg})`);
	}
}

// Functions added here are exposed to the outside
module.exports = { encrypt, doubleAndAdd, findNextPoint, mod }