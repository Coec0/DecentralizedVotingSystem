const Web3 = require('web3');

let P = { x: 1, y: 60 }
let Q = { x: 6, y: 21 }
let m = 109;

console.log(pointAddition(P, Q, m));

function pointAddition(P, Q, m) {
	const add = function add(P, Q) {
		const a = (Q.y - P.y) * modularInverse((Q.x - P.x), m);
		return mod(a, m);
	}

	const double = function double(P, Q) {
		
	}

	// Check if identity
	const firstThing = P.x === Q.x
	const otherThing = Q.y === mod(-1*P.y, m);

	if (firstThing && otherThing) {
		return Infinity;
	}

	// Check if same point
	let s;
	if (P.x === Q.x && P.y === Q.y) {
		// Do multiplication
		s = double(P, Q);
	} else {
		// Do addition
		s = add(P, Q);
	}

	const x = mod((Math.pow(s, 2) - P.x - Q.x), m);
	const y = mod((s * (P.x - x) - P.y), m);

	return { x, y };
}

/* Helper Functions */

// Real modulo function which supports negative numbers
function mod(x, n) {
	return (x % n + n) % n;
} 

// This uses eulers theorem to compute the inverse
// https://en.wikipedia.org/wiki/Modular_multiplicative_inverse#Using_Euler's_theorem
function modularInverse(a, p) {
	return mod(Math.pow(a, p - 2), p);
}

module.exports = { pointAddition }