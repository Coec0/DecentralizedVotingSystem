/* test/sum.js */

const expect = require('chai').expect;
const bigInt = require("big-integer");

const crypto = require('./../src/utils/crypto.js');

describe('#mod()', function() {
	context('with missing argument', () => {
		it('should throw error', () => {
			expect(() => {
				crypto.mod(null);
			}).to.throw(ReferenceError, 'missing arguments');

			expect(() => {
				crypto.mod(undefined, null);
			}).to.throw(ReferenceError, 'missing arguments');

			expect(() => {
				crypto.mod(bigInt(12), null);
			}).to.throw(ReferenceError, 'missing arguments');

			expect(() => {
				crypto.mod(bigInt(12));
			}).to.throw(ReferenceError, 'missing arguments');

			expect(() => {
				crypto.mod(12);
			}).to.throw(ReferenceError, 'missing arguments');
		});
	});	
	
	context('with bigInt arguments', () =>  {
		it('should return correct bigInt', () => {
			expect(crypto.mod(bigInt('-48979784270817801437890312478094231789213487'), bigInt('7826462382673867867896914719627763489674237896')).toString()).to.equal('7777482598403050066459024407149669257885024409');

			expect(crypto.mod(bigInt('57236798436257679586275627938536784568537468796435267896576978253678934567523'), bigInt('56225638756876536579246947586794583675932672394673549867345267354963745678439257698')).toString()).to.equal('57236798436257679586275627938536784568537468796435267896576978253678934567523');

			expect(crypto.mod(bigInt('349208356729856982764'), bigInt('1')).toString()).to.equal('0');

			expect(crypto.mod(bigInt('34920835672985698276412312312312325363485639287652398756293845623897562398451398718937129873912873123123'), bigInt('89713123123')).toString()).to.equal('84306511631');

			expect(crypto.mod(bigInt('1'), bigInt(2)).toString()).to.equal('1');
		});
	});
});

describe('#findNextPoint()', function() {	
	context('with two different points in argument', () => {
		it('should return correct next point', () => {
			let P = { x: bigInt(3), y: bigInt(24) };
			let Q = { x: bigInt(5), y: bigInt(83) };
			let a = bigInt(1);
			let m = bigInt(109);
			let R = { x: bigInt(72), y: bigInt(66) };

			expect(crypto.findNextPoint(P, Q, a, m)).to.deep.equal(R);
		});

		it('should return correct next point', () => {
			let P = { x: bigInt(11), y: bigInt(12) };
			let Q = { x: bigInt(17), y: bigInt(74) };
			let a = bigInt(1);
			let m = bigInt(109);
			let R = { x: bigInt(103), y: bigInt(91) };

			expect(crypto.findNextPoint(P, Q, a, m)).to.deep.equal(R);
		});
	});

	context('with two identical points in argument', () => {
		it('should return correct next point', () => {
			let P = { x: bigInt(0), y: bigInt(108) };
			let Q = { x: bigInt(0), y: bigInt(108) 	};
			let a = bigInt(1);
			let m = bigInt(109);
			let R = { x: bigInt(82), y: bigInt(42) };

			expect(crypto.findNextPoint(P, Q, a, m)).to.deep.equal(R);
		});

		it('should return correct next point', () => {
			let P = { x: bigInt(1), y: bigInt(60) };
			let Q = { x: bigInt(1), y: bigInt(60) };
			let a = bigInt(1);
			let m = bigInt(109);
			let R = { x: bigInt(72), y: bigInt(43) };

			expect(crypto.findNextPoint(P, Q, a, m)).to.deep.equal(R);
		});
	});
});

describe('#doubleAndAdd()', function() {	
	context('with valid arguments', () => {
		it('should return correct point', () => {
			let P = { x: bigInt(18), y: bigInt(40) };
			let n = bigInt(10);
			let a = bigInt(1);
			let m = bigInt(109);
			let R = { x: bigInt(75), y: bigInt(11) };

			expect(crypto.doubleAndAdd(P, n, a, m)).to.deep.equal(R);
		});

		it('should return correct point', () => {
			let P = { x: bigInt(3), y: bigInt(24) };
			let n = bigInt(400);
			let a = bigInt(1);
			let m = bigInt(109);
			let R = { x: bigInt(50), y: bigInt(79) };

			expect(crypto.doubleAndAdd(P, n, a, m)).to.deep.equal(R);
		});

		it('should return correct point', () => {
			let P = { x: bigInt(3), y: bigInt(24) };
			let n = bigInt(1298370031);
			let a = bigInt(1);
			let m = bigInt(109);
			let R = { x: bigInt(33), y: bigInt(1) };

			expect(crypto.doubleAndAdd(P, n, a, m)).to.deep.equal(R);
		});
	});
});