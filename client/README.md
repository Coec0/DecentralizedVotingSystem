# client

## Node setup
```
Download and install Node.js LTS from https://nodejs.org/en/download/
```

## Project setup
```
npm install
```

### Compiles and hot-reloads for development
```
npm run dev
```

### Compiles and minifies for production
```
npm run build
```

### Run your tests
```
npm run test
```

### Lints and fixes files
```
npm run lint
```

### Customize configuration
See [Configuration Reference](https://cli.vuejs.org/config/).


## Crypto functions (utils/crypto.js)

<dl>
<dt><a href="#encrypt">encrypt(message, ordG, G, B, p, A)</a> ⇒ <code>Point</code></dt>
<dd></dd>
<dt><a href="#doubleAndAdd">doubleAndAdd(P, n, m)</a> ⇒ <code>Point</code></dt>
<dd><p>Calculates n * P using double-and-add method
<a href="https://en.wikipedia.org/wiki/Elliptic_curve_point_multiplication#Double-and-add">https://en.wikipedia.org/wiki/Elliptic_curve_point_multiplication#Double-and-add</a></p>
</dd>
<dt><a href="#findNextPoint">findNextPoint(P, Q, m)</a> ⇒ <code>Point</code></dt>
<dd><p>Finds next point given two arbitrary points using either point addition or point doubling</p>
</dd>
<dt><a href="#pointAdd">pointAdd(P, Q, m)</a> ⇒ <code>Point</code></dt>
<dd><p>Finds next point from two distinguished points (including reflection)</p>
</dd>
<dt><a href="#pointDouble">pointDouble(P, Q, a, m)</a> ⇒ <code>Point</code></dt>
<dd><p>Finds the next point from two identical points point (including reflection)</p>
</dd>
<dt><a href="#calcAddLambda">calcAddLambda(P, Q, m)</a> ⇒ <code>Integer</code></dt>
<dd><p>Calculates the lambda used in calculating point addition</p>
</dd>
<dt><a href="#calcDoubleLambda">calcDoubleLambda(P, a, m)</a> ⇒ <code>Integer</code></dt>
<dd><p>Calculates the lambda used for point doubling</p>
</dd>
<dt><a href="#mod">mod(x, n)</a> ⇒ <code>Integer</code></dt>
<dd><p>Real modulo function which supports negative numbers (Supports both bigInts and Integers)</p>
</dd>
</dl>

<a name="encrypt"></a>

## encrypt(message, ordG, G, B, p, A) ⇒ <code>Point</code>
**Kind**: global function  
**Returns**: <code>Point</code> - The point represnting the encrypted data.  

| Param | Type | Description |
| --- | --- | --- |
| message | <code>Integer</code> | The message to be encrypted |
| ordG | <code>Integer</code> | The order of the generator |
| G | <code>Point</code> | The generator for the encryption |
| B | <code>Point</code> | The public key |
| p | <code>Integer</code> | The prime used as modulo |
| A | <code>Integer</code> | The a-value of the curve |

<a name="doubleAndAdd"></a>

## doubleAndAdd(P, n, m) ⇒ <code>Point</code>
Calculates n * P using double-and-add method
https://en.wikipedia.org/wiki/Elliptic_curve_point_multiplication#Double-and-add

**Kind**: global function  
**Returns**: <code>Point</code> - The resulting point  

| Param | Type | Description |
| --- | --- | --- |
| P | <code>Point</code> | The point to be mutlipled |
| n | <code>Integer</code> | The scalar value |
| m | <code>Integer</code> | The modulo of the current curve |

<a name="findNextPoint"></a>

## findNextPoint(P, Q, m) ⇒ <code>Point</code>
Finds next point given two arbitrary points using either point addition or point doubling

**Kind**: global function  
**Returns**: <code>Point</code> - The resulting point  

| Param | Type | Description |
| --- | --- | --- |
| P | <code>Point</code> | The first point |
| Q | <code>Point</code> | The second point |
| m | <code>Integer</code> | The modulo of the current curve |

<a name="pointAdd"></a>

## pointAdd(P, Q, m) ⇒ <code>Point</code>
Finds next point from two distinguished points (including reflection)

**Kind**: global function  
**Returns**: <code>Point</code> - Resulting point  

| Param | Type | Description |
| --- | --- | --- |
| P | <code>Point</code> | The first point |
| Q | <code>Point</code> | The second point |
| m | <code>Integer</code> | The modulo of the current curve |

<a name="pointDouble"></a>

## pointDouble(P, Q, a, m) ⇒ <code>Point</code>
Finds the next point from two identical points point (including reflection)

**Kind**: global function  
**Returns**: <code>Point</code> - Resulting point  

| Param | Type | Description |
| --- | --- | --- |
| P | <code>Point</code> | The only point which is doubled to get the next point |
| Q | <code>Point</code> | This is only included for some testing support. Should be identical to P |
| a | <code>Integer</code> | The a-value of the current curve |
| m | <code>Integer</code> | The modulo of the current curve |

<a name="calcAddLambda"></a>

## calcAddLambda(P, Q, m) ⇒ <code>Integer</code>
Calculates the lambda used in calculating point addition

**Kind**: global function  
**Returns**: <code>Integer</code> - The lambda value for the two points  

| Param | Type | Description |
| --- | --- | --- |
| P | <code>Point</code> | The first point |
| Q | <code>Point</code> | The second point |
| m | <code>Integer</code> | The modulo of the current curve |

<a name="calcDoubleLambda"></a>

## calcDoubleLambda(P, a, m) ⇒ <code>Integer</code>
Calculates the lambda used for point doubling

**Kind**: global function  
**Returns**: <code>Integer</code> - The lambda value for the point  

| Param | Type | Description |
| --- | --- | --- |
| P | <code>Point</code> | The first point |
| a | <code>Integer</code> | The 'a'-value for the curve used |
| m | <code>Integer</code> | The modulo of the current curve |

<a name="mod"></a>

## mod(x, n) ⇒ <code>Integer</code>
Real modulo function which supports negative numbers (Supports both bigInts and Integers)

**Kind**: global function  
**Returns**: <code>Integer</code> - The result  

| Param | Type | Description |
| --- | --- | --- |
| x | <code>Integer</code> | The value to be modulo-ed |
| n | <code>Integer</code> | The modulo value |

