const Web3 = require('web3');

let P = { x: 1, y: 60 }
let Q = { x: 6, y: 21 }
let m = 109;

console.time('EEA');
console.log(modularInverse(3453453453453451, 12312312));
console.timeEnd('EEA');

function pointAddition(P, Q, m) {
	const add = function add(P, Q) {
		const a = (Q.y - P.y) * modularInverse((Q.x - P.x), m);
		return mod(a, m);
	}

	const double = function double(P, Q) {
		
	}

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

// Use EEA to compute modular inverse
// https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm
function modularInverse(a, m) {
	// Validate input
	[a, m] = [Number(a), Number(m)];
	if (Number.isNaN(a) || Number.isNaN(m)) {
		return NaN;
	}
	a = mod(a, m);
	if (!a || m < 2) {
		return NaN;
	}

	// Find GCD
	const s = [];
	let b = m;
	while(b) {
		[a, b] = [b, a % b];
		s.push({a, b});
	}
	if (a !== 1) {
		return NaN; // inverse does not exists
	}
	// find the inverse
	let x = 1;
	let y = 0;
	for(let i = s.length - 2; i >= 0; --i) {
		[x, y] = [y,  x - y * Math.floor(s[i].a / s[i].b)];
	}

	return mod(y, m);
}

module.exports = { pointAddition }