const web3 = require('web3');
const bigInt = require("big-integer");

function encrypt(message, ordG, G, B) {
	const k = web3.utils.randomHex(ordG);

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
	if (!n) return 0;
	if (n === 1) return P;

	if (mod(n, 2) === 1) return pointAdd(P, doubleAndAdd(P, n-1, a, m), m);

	return doubleAndAdd(pointDouble(P, P, a, m), n/2, a, m);
}

/**
 * Finds next point given two arbitrary points using either point addition or point doubling
 * @param  {Point} P   The first point
 * @param  {Point} Q   The second point
 * @param  {Integer} m The modulo of the current curve
 * @return {Point}     The resulting point
 */
function findNextPoint(P, Q, a, m) {
	// Check if same point
	if (P.x === Q.x && P.y === Q.y) {
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
	const hasSameX = P.x === Q.x
	const hasOppositeY = Q.y === mod(-1*P.y, m);

	if (hasSameX && hasOppositeY) {
		// Is inverse => no point intersects => return Infinity
		return Infinity;
	}

	const lamba = calcAddLambda(P, Q, m);
	const x = mod((Math.pow(lamba, 2) - P.x - Q.x), m);
	const y = mod((lamba * (P.x - x) - P.y), m);

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
	if (!(P.x === Q.x && P.y === Q.y)) console.warn('pointDouble called with two NOT IDENTICAL points, is this really intended?');

	const lamba = calcDoubleLambda(P, a, m);
	const x = mod((Math.pow(lamba, 2) - P.x - Q.x), m);
	const y = mod((lamba * (P.x - x) - P.y), m);

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
	const a = (Q.y - P.y) * modInv((Q.x - P.x), m);
	return mod(a, m);
}

/**
 * Calculates the lambda used for point doubling
 * @param  {Point} P   The first point
 * @param  {Integer} a The 'a'-value for the curve used
 * @param  {Integer} m The modulo of the current curve
 * @return {Integer}   The lambda value for the point
 */
function calcDoubleLambda(P, a, m) {
	const lambda = (3 * Math.pow(P.x, 2) + a) * modInv(2 * P.y, m);
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
		if (!(x instanceof bigInt) || !(n instanceof bigInt)) throw new TypeError('x and n are not bigInt');

		return x.mod(n).plus(n).mod(n);
	} else {
		return (x % n + n) % n;
	}
} 

/**
 * Finds the modulo inverse for x (x^-1 mod n)
 * @param  {Integer} x The value to be inverse-modulo-ed
 * @param  {Integer} n The modulo value
 * @return {Integer}   The modulo inverse
 */
function modInv(x, n) {
	// Error check
	if (x === null || x === undefined || n === null || n === undefined) throw new ReferenceError('missing arguments');
	// Type checking
	if (!(x instanceof bigInt)) console.warn('Warning! x is not a bigInt, but it probably should be?');
	if (!(n instanceof bigInt)) console.warn('Warning! n is not a bigInt, but it probably should be?');

	return bigInt(x).modInv(n);
}

// This functions takes any amount of arguments and check if they are either bigInts or objects which contain bigInts
// Will throw an error if something that is not a bigInt is found
// Not fully tested for now
function checkBigInts(...args) {
	console.log(args)

	for (var i = args.length - 1; i >= 0; i--) {
		let arg = args[i];

		if (arg instanceof bigInt) {
			continue;
		}

		if (typeof arg === 'object') {
			checkBigInts(...Object.values(arg));
			continue;
		}

		throw new TypeError('Object contains something that is not a bigInt');
	}
}

// Functions added here are exposed to the outside
module.exports = { doubleAndAdd, findNextPoint, mod, modInv }