const BigNumber = require('bignumber.js');
const Big = require('big.js');
const BigInteger = require("big-integer");

const P = '115792089237316195423570985008687907853269984665640564039457584007908834671663';
const G = '286650441496909734516720688912544350032790572785058722254415355376215376009112';

const iterations = parseInt(process.argv[2]);

console.log('Starting test');
console.log(`Performing ${iterations} iterations\n`);


console.log('Testing bignumber.js');
// Curve variables
const BN_P = BigNumber(P);
const BN_G = BigNumber(G);
let start = Date.now();
for(let i = 0; i < iterations; i++) {
	let temp = BN_G.pow(2).times(3).times(BN_G.times(2)).plus(BN_P).mod(BN_P);
}
const BigNumberResult = Date.now() - start;


console.log('Testing big.js');
// Curve variables
const Big_P = Big(P);
const Big_G = Big(G);
start = Date.now();
for(let i = 0; i < iterations; i++) {
	let temp = Big_G.pow(2).times(3).times(Big_G.times(2)).plus(Big_P).mod(Big_P);
}
const BigResult = Date.now() - start;


console.log('Testing big-integer');
// Curve variables
const BI_P = BigInteger(P);
const BI_G = BigInteger(G);
start = Date.now();
for(let i = 0; i < iterations; i++) {
	let temp = BI_G.pow(2).times(3).times(BI_G.times(2)).plus(BI_P).mod(BI_P);
}
const BigIntegerResult = Date.now() - start;



console.log();
console.log('Results:');
console.log('BigNumber:   ' + BigNumberResult + ' milliseconds');
console.log('big.js:      ' + BigResult + ' milliseconds');
console.log('big-integer: ' + BigIntegerResult + ' milliseconds');