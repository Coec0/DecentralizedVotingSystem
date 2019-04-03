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

	context('with different argument types', () => {
		it('should throw error', () => {
			expect(() => {
				crypto.mod(123, bigInt(123));
			}).to.throw(TypeError, 'x and n must be same type');

			expect(() => {
				crypto.mod({}, 123);
			}).to.throw(TypeError, 'x and n must be same type');

			expect(() => {
				crypto.mod({}, bigInt(123));
			}).to.throw(TypeError, 'x and n are not bigInt');

			expect(() => {
				crypto.mod(bigInt(123), {});
			}).to.throw(TypeError, 'x and n are not bigInt');

			expect(() => {
				crypto.mod(123, bigInt(123));
			}).to.throw(TypeError, 'x and n must be same type');
		});
	});
	
	context('with number arguments', () => {
		it('should return correct number', () => {
			expect(crypto.mod(98712367, 78321)).to.equal(27907);

			expect(crypto.mod(74238, 12)).to.equal(6);

			expect(crypto.mod(74231238798, 2321789)).to.equal(1322679);

			expect(crypto.mod(0, 123)).to.equal(0);

			expect(crypto.mod(321789, 1)).to.equal(0);
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

describe('#modInv()', function() {
	context('with missing argument', () => {
		it('should throw error', () => {
			expect(() => {
				crypto.modInv(null, undefined);
			}).to.throw(ReferenceError, 'missing arguments');

			expect(() => {
				crypto.modInv(null);
			}).to.throw(ReferenceError, 'missing arguments');

			expect(() => {
				crypto.modInv();
			}).to.throw(ReferenceError, 'missing arguments');
		});
	});	

	context('with different argument types', () => {
		it('should return correct bigInt', () => {
			expect(crypto.modInv(bigInt(98712367), 78321).toString()).to.equal('68995');
		});
	});
	
	context('with number arguments', () => {
		it('should return correct bigInt', () => {
			expect(crypto.modInv(98712367, 78321).toString()).to.equal('68995');
		});
	});

	context('with bigInt arguments', () =>  {
		it('should return correct bigInt', () => {
			expect(crypto.modInv(bigInt('987123673122487'), bigInt('718321')).toString()).to.equal('430863');
		});
	});
	
	context('with not co-prime arguments', () =>  {
		it('should throw error', () => {
			expect(() => {
				crypto.modInv(bigInt('123872498'), bigInt('213798'));
			}).to.throw(Error, 'are not co-prime');
		});
	});
});

describe('#findNextPoint()', function() {	
	context('with two different points in argument', () => {
		it('should return correct next point', () => {
			let P = { x: 3, y: 24 };
			let Q = { x: 5, y: 83 };
			let m = 109;
			let R = { x: 72, y: 66 };

			expect(crypto.findNextPoint(P, Q, m)).to.deep.equal(R);
		});

		it('should return correct next point', () => {
			let P = { x: 11, y: 12 };
			let Q = { x: 17, y: 74 };
			let m = 109;
			let R = { x: 103, y: 91 };

			expect(crypto.findNextPoint(P, Q, m)).to.deep.equal(R);
		});
	});

	context('with two identical points in argument', () => {
		it('should return correct next point', () => {
			let P = { x: 0, y: 108 };
			let Q = { x: 0, y: 108 	};
			let m = 109;
			let R = { x: 82, y: 42 };

			expect(crypto.findNextPoint(P, Q, m)).to.deep.equal(R);
		});

		it('should return correct next point', () => {
			let P = { x: 1, y: 60 };
			let Q = { x: 1, y: 60 };
			let m = 109;
			let R = { x: 72, y: 43 };

			expect(crypto.findNextPoint(P, Q, m)).to.deep.equal(R);
		});
	});
});

describe('#doubleAndAdd()', function() {	
	context('with valid arguments', () => {
		it('should return correct point', () => {
			let P = { x: 18, y: 40 };
			let n = 10;
			let m = 109;
			let R = { x: 75, y: 11 };

			expect(crypto.doubleAndAdd(P, n, m)).to.deep.equal(R);
		});

		it('should return correct point', () => {
			let P = { x: 3, y: 24 };
			let n = 400;
			let m = 109;
			let R = { x: 50, y: 79 };

			expect(crypto.doubleAndAdd(P, n, m)).to.deep.equal(R);
		});

		it('should return correct point', () => {
			let P = { x: 3, y: 24 };
			let n = 1298370031;
			let m = 109;
			let R = { x: 33, y: 1 };

			expect(crypto.doubleAndAdd(P, n, m)).to.deep.equal(R);
		});
	});
});