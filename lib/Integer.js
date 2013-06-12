/**
 * Constructs a two's-complement integer an array containing bits of the
 * integer in 32-bit (signed) pieces, given in little-endian order (i.e.,
 * lowest-order bits in the first piece), and the sign of -1 or 0.
 *
 * See the from* functions below for other convenient ways of constructing
 * Integers.
 *
 * The internal representation of an integer is an array of 32-bit signed
 * pieces, along with a sign (0 or -1) that indicates the contents of all the
 * other 32-bit pieces out to infinity.  We use 32-bit pieces because these are
 * the size of integers on which Javascript performs bit-operations.  For
 * operations like addition and multiplication, we split each number into 16-bit
 * pieces, which can easily be multiplied within Javascript's floating-point
 * representation without overflow or change in sign.
 *
 * @constructor
 * @param {Array.<number>} bits Array containing the bits of the number.
 * @param {number} sign The sign of the number: -1 for negative and 0 positive.
 */
var Integer = function(bits, sign) {
  /**
   * @type {!Array.<number>}
   * @private
   */
  this.bits_ = [];

  /**
   * @type {number}
   * @private
   */
  this.sign_ = sign;

  // Copy the 32-bit signed integer values passed in.  We prune out those at the
  // top that equal the sign since they are redundant.
  var top = true;
  for (var i = bits.length - 1; i >= 0; i--) {
    var val = bits[i] | 0;
    if (!top || val != sign) {
      this.bits_[i] = val;
      top = false;
    }
  }
};


// NOTE: Common constant values ZERO, ONE, NEG_ONE, etc. are defined below the
// from* methods on which they depend.


/**
 * A cache of the Integer representations of small integer values.
 * @type {!Object}
 * @private
 */
Integer.IntCache_ = {};


/**
 * Returns an Integer representing the given (32-bit) integer value.
 * @param {number} value A 32-bit integer value.
 * @return {!Integer} The corresponding Integer value.
 */
var I_fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Integer.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Integer([value | 0], value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Integer.IntCache_[value] = obj;
  }
  return obj;
};


/**
 * Returns an Integer representing the given value, provided that it is a finite
 * number.  Otherwise, zero is returned.
 * @param {number} value The value in question.
 * @return {!Integer} The corresponding Integer value.
 */
var I_fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Integer.ZERO;
  } else if (value < 0) {
    return I_negate(I_fromNumber(-value));
  } else {
    var bits = [];
    var pow = 1;
    for (var i = 0; value >= pow; i++) {
      bits[i] = (value / pow) | 0;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return new Integer(bits, 0);
  }
};


/**
 * Returns a Integer representing the value that comes by concatenating the
 * given entries, each is assumed to be 32 signed bits, given in little-endian
 * order (lowest order bits in the lowest index), and sign-extending the highest
 * order 32-bit value.
 * @param {Array.<number>} bits The bits of the number, in 32-bit signed pieces,
 *     in little-endian order.
 * @return {!Integer} The corresponding Integer value.
 */
Integer.fromBits = function(bits) {
  var high = bits[bits.length - 1];
  return new Integer(bits, high & (1 << 31) ? -1 : 0);
};


/**
 * Returns an Integer representation of the given string, written using the
 * given radix.
 * @param {string} str The textual representation of the Integer.
 * @param {number=} opt_radix The radix in which the text is written.
 * @return {!Integer} The corresponding Integer value.
 */
I_fromString = function(str, opt_radix) {
  if (str.length == 0) {
    throw Error('number format error: empty string');
  }

  var radix = opt_radix || 10;
  if (radix < 2 || 36 < radix) {
    throw Error('radix out of range: ' + radix);
  }

  if (str.charAt(0) == '-') {
    return I_negate(I_fromString(str.substring(1), radix));
  } else if (str.indexOf('-') >= 0) {
    throw Error('number format error: interior "-" character');
  }

  // Do several (8) digits each time through the loop, so as to
  // minimize the calls to the very expensive emulated div.
  var radixToPower = I_fromNumber(Math.pow(radix, 8));

  var result = Integer.ZERO;
  for (var i = 0; i < str.length; i += 8) {
    var size = Math.min(8, str.length - i);
    var value = parseInt(str.substring(i, i + size), radix);
    if (size < 8) {
      var power = I_fromNumber(Math.pow(radix, size));
      result = I_add(I_mul(result, power), I_fromNumber(value));
    } else {
      result = I_mul(result, radixToPower);
      result = I_add(add, I_fromNumber(value));
    }
  }
  return result;
};


/**
 * A number used repeatedly in calculations.  Self must appear before the first
 * call to the from* functions below.
 * @type {number}
 * @private
 */
Integer.TWO_PWR_32_DBL_ = (1 << 16) * (1 << 16);


/** @type {!Integer} */
Integer.ZERO = I_fromInt(0);


/** @type {!Integer} */
Integer.ONE = I_fromInt(1);


/**
 * @type {!Integer}
 * @private
 */
Integer.TWO_PWR_24_ = I_fromInt(1 << 24);


/**
 * Returns the value, assuming it is a 32-bit integer.
 * @return {number} The corresponding int value.
 */
var I_toInt = function(self) {
  return self.bits_.length > 0 ? self.bits_[0] : self.sign_;
};

var I_toWord = function(self) {
  return I_toInt(self) >>> 0;
};

/** @return {number} The closest floating-point representation to self value. */
var I_toNumber = function(self) {
  if (isNegative(self)) {
    return -I_toNumber(I_negate(self));
  } else {
    var val = 0;
    var pow = 1;
    for (var i = 0; i < self.bits_.length; i++) {
      val += getBitsUnsigned(self, i) * pow;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return val;
  }
};


/**
 * Returns the index-th 32-bit (signed) piece of the Integer according to
 * little-endian order (i.e., index 0 contains the smallest bits).
 * @param {number} index The index in question.
 * @return {number} The requested 32-bits as a signed number.
 */
var getBits = function(self, index) {
  if (index < 0) {
    return 0;  // Allowing self simplifies bit shifting operations below...
  } else if (index < self.bits_.length) {
    return self.bits_[index];
  } else {
    return self.sign_;
  }
};


/**
 * Returns the index-th 32-bit piece as an unsigned number.
 * @param {number} index The index in question.
 * @return {number} The requested 32-bits as an unsigned number.
 */
var getBitsUnsigned = function(self, index) {
  var val = getBits(self, index);
  return val >= 0 ? val : Integer.TWO_PWR_32_DBL_ + val;
};


/** @return {number} The sign bit of self number, -1 or 0. */
var getSign = function(self) {
  return self.sign_;
};


/** @return {boolean} Whether self value is zero. */
var isZero = function(self) {
  if (self.sign_ != 0) {
    return false;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    if (self.bits_[i] != 0) {
      return false;
    }
  }
  return true;
};


/** @return {boolean} Whether self value is negative. */
var isNegative = function(self) {
  return self.sign_ == -1;
};


/** @return {boolean} Whether self value is odd. */
var isOdd = function(self) {
  return (self.bits_.length == 0) && (self.sign_ == -1) ||
         (self.bits_.length > 0) && ((self.bits_[0] & 1) != 0);
};


/**
 * @param {Integer} other Integer to compare against.
 * @return {boolean} Whether self Integer equals the other.
 */
var I_equals = function(self, other) {
  if (self.sign_ != other.sign_) {
    return false;
  }
  var len = Math.max(self.bits_.length, other.bits_.length);
  for (var i = 0; i < len; i++) {
    if (getBits(self, i) != getBits(other, i)) {
      return false;
    }
  }
  return true;
};


/**
 * @param {Integer} other Integer to compare against.
 * @return {boolean} Whether self Integer does not equal the other.
 */
var I_notEquals = function(self, other) {
  return !I_equals(self, other);
};


/**
 * @param {Integer} other Integer to compare against.
 * @return {boolean} Whether self Integer is greater than the other.
 */
var greaterThan = function(self, other) {
  return I_compare(self, other) > 0;
};


/**
 * @param {Integer} other Integer to compare against.
 * @return {boolean} Whether self Integer is greater than or equal to the other.
 */
var greaterThanOrEqual = function(self, other) {
  return I_compare(self, other) >= 0;
};


/**
 * @param {Integer} other Integer to compare against.
 * @return {boolean} Whether self Integer is less than the other.
 */
var lessThan = function(self, other) {
  return I_compare(self, other) < 0;
};


/**
 * @param {Integer} other Integer to compare against.
 * @return {boolean} Whether self Integer is less than or equal to the other.
 */
var lessThanOrEqual = function(self, other) {
  return I_compare(self, other) <= 0;
};


/**
 * Compares self Integer with the given one.
 * @param {Integer} other Integer to compare against.
 * @return {number} 0 if they are the same, 1 if the self is greater, and -1
 *     if the given one is greater.
 */
var I_compare = function(self, other) {
  var diff = I_sub(self, other);
  if (isNegative(diff)) {
    return -1;
  } else if (isZero(diff)) {
    return 0;
  } else {
    return +1;
  }
};

var I_compareInt = function(self, other) {
  return I_compare(self, I_fromInt(other));
}


/**
 * Returns an integer with only the first numBits bits of self value, sign
 * extended from the final bit.
 * @param {number} numBits The number of bits by which to shift.
 * @return {!Integer} The shorted integer value.
 */
var shorten = function(self, numBits) {
  var arr_index = (numBits - 1) >> 5;
  var bit_index = (numBits - 1) % 32;
  var bits = [];
  for (var i = 0; i < arr_index; i++) {
    bits[i] = getBits(self, i);
  }
  var sigBits = bit_index == 31 ? 0xFFFFFFFF : (1 << (bit_index + 1)) - 1;
  var val = getBits(self, arr_index) & sigBits;
  if (val & (1 << bit_index)) {
    val |= 0xFFFFFFFF - sigBits;
    bits[arr_index] = val;
    return new Integer(bits, -1);
  } else {
    bits[arr_index] = val;
    return new Integer(bits, 0);
  }
};


/** @return {!Integer} The negation of self value. */
var I_negate = function(self) {
  return I_add(not(self), Integer.ONE);
};


/**
 * Returns the sum of self and the given Integer.
 * @param {Integer} other The Integer to add to self.
 * @return {!Integer} The Integer result.
 */
var I_add = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  var carry = 0;

  for (var i = 0; i <= len; i++) {
    var a1 = getBits(self, i) >>> 16;
    var a0 = getBits(self, i) & 0xFFFF;

    var b1 = getBits(other, i) >>> 16;
    var b0 = getBits(other, i) & 0xFFFF;

    var c0 = carry + a0 + b0;
    var c1 = (c0 >>> 16) + a1 + b1;
    carry = c1 >>> 16;
    c0 &= 0xFFFF;
    c1 &= 0xFFFF;
    arr[i] = (c1 << 16) | c0;
  }
  return Integer.fromBits(arr);
};


/**
 * Returns the difference of self and the given Integer.
 * @param {Integer} other The Integer to subtract from self.
 * @return {!Integer} The Integer result.
 */
var I_sub = function(self, other) {
  return I_add(self, I_negate(other));
};


/**
 * Returns the product of self and the given Integer.
 * @param {Integer} other The Integer to multiply against self.
 * @return {!Integer} The product of self and the other.
 */
var I_mul = function(self, other) {
  if (isZero(self)) {
    return Integer.ZERO;
  } else if (isZero(other)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_mul(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_mul(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_mul(self, I_negate(other)));
  }

  // If both numbers are small, use float multiplication
  if (lessThan(self, Integer.TWO_PWR_24_) &&
      lessThan(other, Integer.TWO_PWR_24_)) {
    return I_fromNumber(I_toNumber(self) * I_toNumber(other));
  }

  // Fill in an array of 16-bit products.
  var len = self.bits_.length + other.bits_.length;
  var arr = [];
  for (var i = 0; i < 2 * len; i++) {
    arr[i] = 0;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    for (var j = 0; j < other.bits_.length; j++) {
      var a1 = getBits(self, i) >>> 16;
      var a0 = getBits(self, i) & 0xFFFF;

      var b1 = getBits(other, j) >>> 16;
      var b0 = getBits(other, j) & 0xFFFF;

      arr[2 * i + 2 * j] += a0 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j);
      arr[2 * i + 2 * j + 1] += a1 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 1] += a0 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 2] += a1 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 2);
    }
  }

  // Combine the 16-bit values into 32-bit values.
  for (var i = 0; i < len; i++) {
    arr[i] = (arr[2 * i + 1] << 16) | arr[2 * i];
  }
  for (var i = len; i < 2 * len; i++) {
    arr[i] = 0;
  }
  return new Integer(arr, 0);
};


/**
 * Carries any overflow from the given index into later entries.
 * @param {Array.<number>} bits Array of 16-bit values in little-endian order.
 * @param {number} index The index in question.
 * @private
 */
Integer.carry16_ = function(bits, index) {
  while ((bits[index] & 0xFFFF) != bits[index]) {
    bits[index + 1] += bits[index] >>> 16;
    bits[index] &= 0xFFFF;
  }
};


var I_mod = function(self, other) {
  return I_rem(I_add(other, I_rem(self, other)), other);
}

var I_div = function(self, other) {
  if(greaterThan(self, Integer.ZERO) != greaterThan(other, Integer.ZERO)) {
    if(I_rem(self, other) != Integer.ZERO) {
      return I_sub(I_quot(self, other), Integer.ONE);
    }
  }
  return I_quot(self, other);
}

var I_quotRem = function(self, other) {
  return [1, I_quot(self, other), I_rem(self, other)];
}

var I_divMod = function(self, other) {
  return [1, I_div(self, other), I_mod(self, other)];
}

/**
 * Returns self Integer divided by the given one.
 * @param {Integer} other Th Integer to divide self by.
 * @return {!Integer} Self value divided by the given one.
 */
var I_quot = function(self, other) {
  if (isZero(other)) {
    throw Error('division by zero');
  } else if (isZero(self)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_quot(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_quot(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_quot(self, I_negate(other)));
  }

  // Repeat the following until the remainder is less than other:  find a
  // floating-point that approximates remainder / other *from below*, add self
  // into the result, and subtract it from the remainder.  It is critical that
  // the approximate value is less than or equal to the real value so that the
  // remainder never becomes negative.
  var res = Integer.ZERO;
  var rem = self;
  while (greaterThanOrEqual(rem, other)) {
    // Approximate the result of division. Self may be a little greater or
    // smaller than the actual value.
    var approx = Math.max(1, Math.floor(I_toNumber(rem) / I_toNumber(other)));

    // We will tweak the approximate result by changing it in the 48-th digit or
    // the smallest non-fractional digit, whichever is larger.
    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);

    // Decrease the approximation until it is smaller than the remainder.  Note
    // that if it is too large, the product overflows and is negative.
    var approxRes = I_fromNumber(approx);
    var approxRem = I_mul(approxRes, other);
    while (isNegative(approxRem) || greaterThan(approxRem, rem)) {
      approx -= delta;
      approxRes = I_fromNumber(approx);
      approxRem = I_mul(approxRes, other);
    }

    // We know the answer can't be zero... and actually, zero would cause
    // infinite recursion since we would make no progress.
    if (isZero(approxRes)) {
      approxRes = Integer.ONE;
    }

    res = I_add(res, approxRes);
    rem = I_sub(rem, approxRem);
  }
  return res;
};


/**
 * Returns self Integer modulo the given one.
 * @param {Integer} other The Integer by which to mod.
 * @return {!Integer} Self value modulo the given one.
 */
var I_rem = function(self, other) {
  return I_sub(self, I_mul(I_quot(self, other), other));
};


/** @return {!Integer} The bitwise-NOT of self value. */
var not = function(self) {
  var len = self.bits_.length;
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = ~self.bits_[i];
  }
  return new Integer(arr, ~self.sign_);
};


/**
 * Returns the bitwise-AND of self Integer and the given one.
 * @param {Integer} other The Integer to AND with self.
 * @return {!Integer} The bitwise-AND of self and the other.
 */
var I_and = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = getBits(self, i) & getBits(other, i);
  }
  return new Integer(arr, self.sign_ & other.sign_);
};


/**
 * Returns the bitwise-OR of self Integer and the given one.
 * @param {Integer} other The Integer to OR with self.
 * @return {!Integer} The bitwise-OR of self and the other.
 */
var I_or = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = getBits(self, i) | getBits(other, i);
  }
  return new Integer(arr, self.sign_ | other.sign_);
};


/**
 * Returns the bitwise-XOR of self Integer and the given one.
 * @param {Integer} other The Integer to XOR with self.
 * @return {!Integer} The bitwise-XOR of self and the other.
 */
var I_xor = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = getBits(self, i) ^ getBits(other, i);
  }
  return new Integer(arr, self.sign_ ^ other.sign_);
};


/**
 * Returns self value with bits shifted to the left by the given amount.
 * @param {number} numBits The number of bits by which to shift.
 * @return {!Integer} Self shifted to the left by the given amount.
 */
var I_shiftLeft = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length + arr_delta + (bit_delta > 0 ? 1 : 0);
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (getBits(self, i - arr_delta) << bit_delta) |
               (getBits(self, i - arr_delta - 1) >>> (32 - bit_delta));
    } else {
      arr[i] = getBits(self, i - arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};


/**
 * Returns self value with bits shifted to the right by the given amount.
 * @param {number} numBits The number of bits by which to shift.
 * @return {!Integer} Self shifted to the right by the given amount.
 */
var I_shiftRight = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length - arr_delta;
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (getBits(self, i + arr_delta) >>> bit_delta) |
               (getBits(self, i + arr_delta + 1) << (32 - bit_delta));
    } else {
      arr[i] = getBits(self, i + arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_signum = function(self) {
  var cmp = I_compare(self, Integer.ZERO);
  if(cmp > 0) {
    return Integer.ONE
  }
  if(cmp < 0) {
    return I_sub(Integer.ZERO, Integer.ONE);
  }
  return Integer.ZERO;
};

var I_abs = function(self) {
  if(I_compare(self, Integer.ZERO) < 0) {
    return I_sub(Integer.ZERO, self);
  }
  return self;
};

var I_decodeDouble = function(x) {
  if(isNaN(x)) {
      return [1, 972, I_fromString('-6755399441055744')];
  }
  var sig = x > 0 ? 1 : -1;
  if(!isFinite(x)) {
      return [1, 972, I_fromNumber(sig) * I_fromString('4503599627370496')];
  }
  x = Math.abs(x);
  var exp = log2(x)-52; // log2 is defined in rts.js
  var man = x/Math.pow(2, exp)
  return [1, exp, I_fromNumber(sig*man)];
}

var I_toString = function(self) {
  var radix = 10;

  if (isZero(self)) {
    return '0';
  } else if (isNegative(self)) {
    return '-' + toString(negate(self));
  }

  // Do several (6) digits each time through the loop, so as to
  // minimize the calls to the very expensive emulated div.
  var radixToPower = I_fromNumber(Math.pow(radix, 6));

  var rem = self;
  var result = '';
  while (true) {
    var remDiv = I_div(rem, radixToPower);
    var intval = I_toInt(I_sub(rem, I_mul(remDiv, radixToPower)));
    var digits = intval.toString();

    rem = remDiv;
    if (isZero(rem)) {
      return digits + result;
    } else {
      while (digits.length < 6) {
        digits = '0' + digits;
      }
      result = '' + digits + result;
    }
  }
};

var I_fromRat = function(a, b) {
    return I_toNumber(a) / I_toNumber(b);
}
