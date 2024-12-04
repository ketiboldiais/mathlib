// § Utility Functions
/** Returns a pretty-print tree of the given Object `Obj`. */
export function treestring<T extends Object>(
  Obj: T,
  cbfn?: (node: any) => void
) {
  const prefix = (key: keyof T, last: boolean) => {
    let str = last ? "└" : "├";
    if (key) str += "─ ";
    else str += "──┐";
    return str;
  };
  const getKeys = (obj: T) => {
    const keys: (keyof T)[] = [];
    for (const branch in obj) {
      if (!obj.hasOwnProperty(branch) || typeof obj[branch] === "function") {
        continue;
      }
      keys.push(branch);
    }
    return keys;
  };
  const grow = (
    key: keyof T,
    root: any,
    last: boolean,
    prevstack: [T, boolean][],
    cb: (str: string) => any
  ) => {
    cbfn && cbfn(root);
    let line = "";
    let index = 0;
    let lastKey = false;
    let circ = false;
    let stack = prevstack.slice(0);
    if (stack.push([root, last]) && stack.length > 0) {
      prevstack.forEach(function (lastState, idx) {
        if (idx > 0) line += (lastState[1] ? " " : "│") + "  ";
        if (!circ && lastState[0] === root) circ = true;
      });
      line += prefix(key, last) + key.toString();
      if (typeof root !== "object") line += ": " + root;
      circ && (line += " (circular ref.)");
      cb(line);
    }
    if (!circ && typeof root === "object") {
      const keys = getKeys(root);
      keys.forEach((branch) => {
        lastKey = ++index === keys.length;
        grow(branch, root[branch], lastKey, stack, cb);
      });
    }
  };
  let output = "";
  const obj = Object.assign({}, Obj);
  grow(
    "." as keyof T,
    obj,
    false,
    [],
    (line: string) => (output += line + "\n")
  );
  return output;
}

class None {
  _tag: "None" = "None";
  constructor() {}
  map(f: (a: never) => unknown): None {
    return new None();
  }
}

class Some<T> {
  readonly value: T;
  _tag: "Some" = "Some";
  constructor(value: T) {
    this.value = value;
  }
  map<S>(f: (a: T) => S): Some<S> {
    return new Some(f(this.value));
  }
}

function some<T>(value: T) {
  return new Some<T>(value);
}

function none() {
  return new None();
}

type Option<T> = None | Some<T>;

/** A data structure with two pointers, left and right. */
class Binode<T> {
  _data: T | null;
  private _R: Option<Binode<T>>;
  private _L: Option<Binode<T>>;
  constructor(data: T | null) {
    this._data = data;
    this._R = none();
    this._L = none();
  }
  /**
   * Returns a copy of this bnode.
   */
  copy() {
    const out = new Binode(this._data);
    const left = this._L;
    const right = this._R;
    out._L = left;
    out._R = right;
    return out;
  }
  /**
   * Flattens this bnode.
   */
  flatten(): Binode<T> | T {
    return this._data === null ? Binode.none<T>() : this._data;
  }
  map<K>(callback: (data: T) => K) {
    if (this._data) {
      return Binode.some<K>(callback(this._data));
    } else return Binode.none<K>();
  }
  /**
   * Sets the value of this bnode.
   */
  set value(data: T) {
    this._data = data;
  }

  do<K>(f: (d: T) => K) {
    if (this._data !== null) {
      f(this._data);
    }
    return this;
  }
  isSomething() {
    return this._data !== null;
  }
  isNothing() {
    return this._data === null;
  }
  static none<T>() {
    return new Binode<T>(null);
  }
  static some<T>(data: T) {
    return new Binode(data);
  }
  get _prev() {
    if (this._L._tag === "None") {
      return new Binode<T>(null);
    } else {
      return this._L.value;
    }
  }
  set _prev(node: Binode<T>) {
    this._L = some(node);
  }
  get _next() {
    if (this._R._tag === "None") {
      return new Binode<T>(null);
    } else {
      return this._R.value;
    }
  }
  set _next(node: Binode<T>) {
    this._R = some(node);
  }
  get _left() {
    return this._prev;
  }
  set _left(node: Binode<T>) {
    this._prev = node;
  }
  get _right() {
    return this._next;
  }
  set _right(node: Binode<T>) {
    this._next = node;
  }
}

function binode<T>(data: T | null = null) {
  return new Binode(data);
}

class LinkedList<T> {
  private head: Binode<T>;
  private tail: Binode<T>;
  private count: number;
  cdr() {
    const list = this.clone();
    if (list.isEmpty) return list;
    let previousHead = list.head;
    if (list.count === 1) {
      list.head = binode();
      list.tail = binode();
    } else {
      list.head = previousHead._right!;
      list.head._left = binode();
      previousHead._right = binode();
    }
    list.count--;
    return list;
  }
  car() {
    if (this.isEmpty) return this;
    const head = this.head._data!;
    return new LinkedList<T>().push(head);
  }
  clear() {
    this.head = binode();
  }
  get length() {
    return this.count;
  }
  get isEmpty() {
    return this.count === 0 || this.head.isNothing();
  }
  constructor() {
    this.count = 0;
    this.head = binode();
    this.tail = binode();
  }
  *[Symbol.iterator](): IterableIterator<T> {
    let node = this.head;
    while (node._data !== null) {
      yield node._data;
      node = node._right;
    }
  }
  toArray(): T[] {
    return [...this];
  }
  safeIdx(i: number) {
    return 0 <= i && i < this.count;
  }
  set(element: T, at: number) {
    const node = this.at(at);
    node._data = element;
    return this;
  }
  private at(index: number) {
    if (!this.safeIdx(index)) {
      return binode<T>();
    } else {
      let count = 0;
      let current = this.head;
      while (count !== index) {
        let k = current._right;
        if (k.isNothing()) break;
        current = k;
        count++;
      }
      return current;
    }
  }

  map<K>(f: (data: T, index: number, list: LinkedList<T>) => K) {
    const list = new LinkedList<K>();
    this.forEach((d, i, l) => list.push(f(d, i, l)));
    return list;
  }

  forEach(f: (data: T, index: number, list: LinkedList<T>) => void) {
    if (this.isEmpty) return this;
    let node = this.head;
    let i = 0;
    while (i < this.count) {
      node.do((d) => f(d, i, this));
      node = node._right;
      i++;
    }
  }

  clone() {
    const list = new LinkedList<T>();
    this.forEach((d) => list.push(d));
    return list;
  }
  #reduce<X>(
    from: 0 | 1,
    reducer: (
      accumulator: X,
      currentValue: T,
      index: number,
      list: LinkedList<T>
    ) => X,
    initialValue: X
  ) {
    let i = 0;
    const fn = (list: LinkedList<T>, init: X): X => {
      if (list.isEmpty) return init;
      else {
        const popped = list[from === 0 ? "shift" : "pop"]();
        if (popped._tag === "None") return init;
        const updatedValue = reducer(init, popped.value, i++, list);
        return fn(list, updatedValue);
      }
    };
    return fn(this.clone(), initialValue);
  }
  reduceRight<X>(
    reducer: (
      accumulator: X,
      currentValue: T,
      index: number,
      list: LinkedList<T>
    ) => X,
    initialValue: X
  ): X {
    return this.#reduce(1, reducer, initialValue);
  }
  reduce<X>(
    reducer: (
      accumulator: X,
      currentValue: T,
      index: number,
      list: LinkedList<T>
    ) => X,
    initialValue: X
  ): X {
    return this.#reduce(0, reducer, initialValue);
  }

  /** Returns the string representation of this list, with each element jointed by the given separator (defaults to the empty string). */
  join(separator: string = "") {
    return [...this].join(separator);
  }

  /** Returns th string representation of this list. */
  toString(f?: (x: T, index: number) => string) {
    const out = this.clone();
    const g = f ? f : (x: T, index: number) => x;
    return out.map((d, i) => g(d, i)).join();
  }

  /** Returns a new list whose elements satisfy the given predicate. */
  filter(predicate: (value: T, index: number, list: LinkedList<T>) => boolean) {
    const out = new LinkedList<T>();
    this.forEach((n, i, list) => predicate(n, i, list) && out.push(n));
    return out;
  }

  /** Reverses this list. */
  reverse() {
    let current = this.head;
    let i = 0;
    while (current.isSomething() && i < this.count) {
      const right = current._right;
      current._right = current._left;
      current._left = right;
      let k = current._left;
      if (k.isNothing() || i > this.count) break;
      current = k;
      i++;
    }
    const tail = this.tail;
    this.tail = this.head;
    this.head = tail;
    return this;
  }

  /** Returns the element at the given index. */
  item(index: number) {
    return this.at(index)._data;
  }

  zip<K>(list: LinkedList<K>) {
    const out = new LinkedList<[T, K]>();
    this.forEach((d, i) => {
      const x = list.item(i);
      if (x !== null) {
        const element: [T, K] = [d, x] as [T, K];
        out.push(element);
      }
    });
    return out;
  }

  /** Removes the last element of this list. */
  pop(): Option<T> {
    if (this.isEmpty) return none();
    let popped = this.tail;
    if (this.length === 1) {
      this.head = binode();
      this.tail = binode();
    } else {
      this.tail = popped._left;
      this.tail._right = binode();
      popped._left = binode();
    }
    this.count--;
    return popped._data === null ? none() : some(popped._data);
  }

  /** Inserts the given element at the head of this list.*/
  unshift(element: T) {
    const node = binode(element);
    if (this.isEmpty) {
      this.head = node;
      this.tail = node;
    } else {
      this.head._prev = node;
      node._next = this.head;
      this.head = node;
    }
    this.count++;
    return this;
  }

  /** Removes the first element of this list. */
  shift() {
    if (this.isEmpty) return none();
    const previousHead = this.head;
    if (this.length === 1) {
      this.head = binode();
      this.tail = binode();
    } else {
      this.head = previousHead._next;
      this.head._prev = binode();
      previousHead._prev = binode();
    }
    this.count--;
    return previousHead._data === null ? none() : some(previousHead._data);
  }

  /** Inserts the given element to this list. */
  push(element: T) {
    const node = binode(element);
    if (this.head.isNothing()) {
      this.head = node;
      this.tail = node;
    } else {
      this.tail._next = node;
      node._prev = this.tail;
      this.tail = node;
    }
    this.count++;
    return this;
  }

  /** Inserts the given elements to this list. */
  append(...elements: T[]) {
    elements.forEach((e) => this.push(e));
    return this;
  }
}

function linkedList<T>(...elements: T[]) {
  return new LinkedList<T>().append(...elements);
}

type Either<A, B> = Left<A> | Right<B>;

/** An object corresponding to failure. */
class Left<T> {
  private value: T;
  constructor(value: T) {
    this.value = value;
  }
  map<A>(): Either<T, never> {
    return this as any;
  }
  isLeft(): this is Left<T> {
    return true;
  }
  isRight(): this is never {
    return false;
  }
  unwrap() {
    return this.value;
  }
  chain<X, S>(): Left<T> {
    return this;
  }
}

/** Returns a new Left with value `x` of type `T`. */
const left = <T>(x: T) => new Left(x);

/** An object corresponding to success. */
class Right<T> {
  private value: T;
  constructor(value: T) {
    this.value = value;
  }
  map<X>(f: (x: T) => X): Either<never, X> {
    return new Right(f(this.value));
  }
  isLeft(): this is never {
    return false;
  }
  isRight(): this is Right<T> {
    return true;
  }
  unwrap() {
    return this.value;
  }
  chain<N, X>(f: (x: T) => Either<N, X>): Either<never, X> {
    return f(this.value) as Either<never, X>;
  }
}

/** Returns a new Right with value `x` of type `T`. */
const right = <T>(x: T) => new Right(x);

type NumberTokenType =
  | token_type.integer
  | token_type.float
  | token_type.big_number
  | token_type.scientific
  | token_type.fraction;

type ErrorType =
  | "lexical-error"
  | "syntax-error"
  | "type-error"
  | "runtime-error"
  | "environment-error"
  | "resolver-error";

// § Error Handling
/** An object corresponding to an error in Winnow. */
class Err extends Error {
  /** This error's message. */
  message: string;
  /** This error's type. */
  $type: ErrorType;
  /** The line where this error occurred. */
  $line: number;
  /** The column where this error occurred. */
  $column: number;
  constructor(message: string, type: ErrorType, line: number, column: number) {
    super(message);
    this.message = message;
    this.$type = type;
    this.$line = line;
    this.$column = column;
  }
  toString() {
    return this.message;
  }
}

const errorFactory =
  (type: ErrorType) => (message: string, line: number, column: number) =>
    new Err(message, type, line, column);
const lexicalError = errorFactory("lexical-error");

// § Primitives

/** An object corresponding to a number of the form `n/d`, where `n` and `d` are integers. */
class Fraction {
  /** The numerator of this fraction. */
  $n: number;
  /** The denominator of this fraction. */
  $d: number;
  constructor(n: number, d: number) {
    this.$n = n;
    this.$d = d;
  }
  toString() {
    return `${this.$n}/${this.$d}`;
  }
}

/**
 * Returns a new Fraction. Both arguments must be integers.
 * If the arguments are not integers, they will be floored.
 * @param n The numerator of this fraction (must be an integer).
 * @param d The denominator of this fraction (must be an integer).
 * @returns A new instance of a fraction.
 */
function fraction(n: number, d: number) {
  return new Fraction(Math.floor(n), Math.floor(d));
}

/** An object corresponding to a number of the form `m x 10^n`. */
class Scientific_Number {
  /** The mantissa in `m x 10^n`. */
  $m: number;
  /** The exponent (an integer) in `m x 10^n` */
  $n: number;
  constructor(m: number, n: number) {
    this.$m = m;
    this.$n = Math.floor(n);
  }
}

/**
 * Returns a new instance of a Scientific_Number.
 * @param m The mantissa in `m x 10^n`.
 * @param n The exponent (an integer) in `m x 10^n`.
 * @returns A new scientific number (m x 10^n).
 */
function scinum(m: number, n: number) {
  return new Scientific_Number(m, Math.floor(n));
}

/** A value native to Winnow. */
type PRIMITIVE =
  | number
  | string
  | null
  | boolean
  | bigint
  | Scientific_Number
  | Fraction
  | Err;

/** Returns true iff `x` is null. */
function isNull(x: any): x is null {
  return x === null;
}

// § Token Types

enum token_type {
  // Utility tokens
  end,
  error,
  empty,
  // Paired Delimiters
  left_paren,
  right_paren,
  left_brace,
  right_brace,
  left_bracket,
  right_bracket,
  // Single Delimiters
  semicolon,
  colon,
  dot,
  comma,
  // Operator Delimiters
  plus,
  minus,
  star,
  slash,
  caret,
  percent,
  bang,
  ampersand,
  tilde,
  vbar,
  equal,
  less,
  greater,
  less_equal,
  greater_equal,
  bang_equal,
  equal_equal,
  plus_plus,
  minus_minus,
  star_star,
  // Vector Operators
  dot_add,
  dot_star,
  dot_minus,
  dot_caret,
  at,
  // Matrix Operators
  pound_plus, // '#+'
  pound_minus, // '#-
  pound_star, // '#*'
  // Literals
  symbol,
  string,
  // BOOLEAN,
  integer,
  float,
  fraction,
  scientific,
  big_number,
  false,
  true,
  nan,
  inf,
  nil,
  numeric_constant,
  algebraic,
  // Keyword Tokens
  and,
  or,
  not,
  nand,
  xor,
  xnor,
  nor,
  if,
  else,
  fn,
  let,
  var,
  return,
  while,
  for,
  class,
  print,
  super,
  this,
  rem,
  mod,
  div,
  native,
  // algebra strings
  algebra_string,
}

// § Token Object

/** An object corresponding to a token in Winnow. */
class Token<
  T extends token_type = token_type,
  L extends PRIMITIVE = PRIMITIVE
> {
  /** This token's type. */
  $type: T;

  /** This tokene's lexeme. */
  $lexeme: string;

  /** This token's literal value, if any (defaults to null). */
  $literal: L = null as any;

  /** The line where this token was first encountered. */
  $line: number;

  /** The column where this token was first encountered. */
  $column: number;

  constructor(type: T, lexeme: string, line: number, column: number) {
    this.$type = type;
    this.$lexeme = lexeme;
    this.$line = line;
    this.$column = column;
  }

  /** Returns a copy of this token. */
  copy() {
    return new Token(this.$type, this.$lexeme, this.$line, this.$column);
  }

  /**
   * Returns a copy of this token with a new token type.
   * @param tokenType The new token type.
   * @returns A copy of this token with the given token type.
   */
  withType<X extends token_type>(tokenType: X) {
    const out = this.copy();
    out.$type = tokenType as any;
    return out as any as Token<X, L>;
  }

  /**
   * Returns a copy of this token with a new lexeme.
   * @param lexeme The new lexeme, a string value.
   * @returns A copy of this token with the given string as its lexeme.
   */
  withLexeme(lexeme: string) {
    const out = this.copy();
    out.$lexeme = lexeme;
    return out;
  }

  /**
   * Returns a copy of this token with a new literal value.
   * @param primitive The new literal value, a Primitive.
   * @returns A copy of this token with the given literal as its literal.
   */
  withLiteral<L2 extends PRIMITIVE>(primitive: L2) {
    const out = this.copy();
    out.$literal = primitive;
    return out as any as Token<T, L2>;
  }

  /**
   * Returns a copy of this token with a new line number.
   * @param line The new line number.
   * @returns A copy of this token with the given line as its line number.
   */
  withLine(line: number) {
    const out = this.copy();
    out.$line = line;
    return out;
  }

  /**
   * Returns a copy of this token with a new column number.
   * @param column The new column number.
   * @returns A copy of this token with the given column as its column number.
   */
  withColumn(column: number) {
    const out = this.copy();
    out.$column = column;
    return out;
  }

  /**
   * Returns true, and asserts, if this token
   * is of the given type `K`.
   */
  isType<K extends T>(type: K): this is Token<K> {
    return this.$type === type;
  }

  /**
   * Returns true if this token is an error token.
   * If this token is an error token, then there must
   * be an accompanying Err object in its $literal
   * field.
   */
  isErrorToken(): this is Token<token_type.error, Err> {
    return this.$type === token_type.error;
  }

  /**
   * Returns true if this token is a number token.
   * If this token is a number token, then there
   * must be an accompanying JavaScript literal number
   * (either a float or an int).
   */
  isNumber(): this is Token<T, number> {
    return typeof this.$literal === "number";
  }

  /**
   * Returns true if and only if this token is a
   * right-delimiter token. That is, either a `)`,
   * `]`, or `}`.
   * @returns a boolean
   */
  isRightDelimiter() {
    return (
      this.$type === token_type.right_paren ||
      this.$type === token_type.right_brace ||
      this.$type === token_type.right_bracket
    );
  }

  /** The empty token, used as a placeholder. */
  static empty: Token<token_type, any> = new Token(
    token_type.empty,
    "",
    -1,
    -1
  );

  /** The end token, marking the end of input. */
  static end: Token<token_type, any> = new Token(token_type.end, "END", -1, -1);

  /** Returns a string form of this token. */
  toString() {
    return `{token: ${token_type[this.$type]}, lexeme: ${this.$lexeme}, line: ${
      this.$line
    }, column: ${this.$column}, literal: ${this.$literal}}`;
  }
}

/**
 * Returns a new Winnow token.
 * @param type The new token's type.
 * @param lexeme The new token's lexeme.
 * @param line The line where this token was first encountered.
 * @param column The column where this token was first encountered.
 * @returns A new instance of Token.
 */
function token<X extends token_type>(
  type: X,
  lexeme: string,
  line: number,
  column: number
): Token<X> {
  return new Token(type, lexeme, line, column);
}

// § Native Function Types

type NativeUnary =
  | "ceil"
  | "floor"
  | "sin"
  | "cos"
  | "cosh"
  | "tan"
  | "lg"
  | "ln"
  | "!"
  | "log"
  | "arcsin"
  | "arccos"
  | "arcsinh"
  | "arctan"
  | "exp"
  | "sinh"
  | "sqrt"
  | "tanh"
  | "gcd"
  | "avg"
  | "deriv"
  | "simplify"
  | "subex"
  | "arccosh";

/** A native function that takes more than 1 argument. */
type NativePolyAry = "max" | "min";

type NativeFn = NativeUnary | NativePolyAry;

// § Lexical

export function lexical(code: string) {
  /**
   * A variable corresponding to the
   * current line the scanner's on.
   */
  let $line: number = 1;

  /**
   * A variable corresponding to the
   * current column the scanner's on.
   */
  let $column: number = 1;

  /**
   * A pointer to the first character
   * of the lexeme currently being
   * scanned.
   */
  let $start: number = 0;

  /**
   * A pointer to the character currently
   * being scanned.
   */
  let $current: number = 0;

  /**
   * Error indicator defaulting to null.
   * If initialized, then the scanning will cease.
   */
  let $error: null | Err = null;

  /**
   * Returns true if the scanner has reached the end
   * of code.
   */
  const atEnd = (): boolean => $current >= code.length || !isNull($error);

  /**
   * Consumes and returns the next character
   * in the code.
   */
  const tick = (): string => code[$current++];

  /**
   * Returns the code substring from $start to $current.
   */
  const slice = (): string => code.slice($start, $current);

  /**
   * Returns a new token.
   * @param type The token type.
   * @param lexeme The token's lexeme.
   * @returns A new Token.
   */
  const tkn = (type: token_type, lexeme: string = ""): Token => {
    lexeme = lexeme ? lexeme : slice();
    return token(type, lexeme, $line, $column);
  };

  /**
   * Returns an error token. If called, sets the
   * mutable $error variable, causing scanning to
   * cease.
   * @param message The error message to accompany
   * the Err object.
   * @returns A new Token of type token_type.ERROR.
   */
  const errorToken = (message: string): Token<token_type.error, Err> => {
    $error = lexicalError(message, $line, $column);
    return token(token_type.error, "", $line, $column).withLiteral($error);
  };

  /**
   * Returns the current character being scanned WITHOUT
   * moving the scanner forward.
   * @returns A 1-character string.
   */
  const peek = (): string => (atEnd() ? "" : code[$current]);

  /**
   * Returns the character just ahead of the current character
   * WITHOUT moving the scanner forward.
   * @returns A 1-character string.
   */
  const peekNext = (): string => (atEnd() ? "" : code[$current + 1]);

  /**
   * Returns the character `by` places
   * ahead of the current character
   * WITHOUT moving the scanner forward.
   * @param by The number of places to look ahead.
   * @returns A 1-character string.
   */
  const lookup = (by: number): string => (atEnd() ? "" : code[$current + by]);

  /**
   * If the provided expected string
   * matches, increments $current (moving
   * the scanner forward) and returns true.
   * Otherwise returns false without increment (
   * scanner doesn't move forward).
   * @param expectedChar A 1-character string corresponding
   * to the expected character.
   * @returns A boolean.
   */
  const match = (expectedChar: string): boolean => {
    if (atEnd()) return false;
    if (code[$current] !== expectedChar) return false;
    $current++;
    return true;
  };

  /**
   * Returns true if the current peek (the character
   * pointed at by `$current`) matches the provided
   * number. Otherwise, returns false.
   * @param char A 1-char string corresponding
   * to the expected character.
   * @returns A boolean.
   */
  const peekIs = (char: string): boolean => peek() === char;

  /**
   * Consumes all whitespace while moving
   * the scanner's `$current` pointer
   * forward.
   * @returns Nothing.
   */
  const skipWhitespace = (): void => {
    while (!atEnd()) {
      const char: string = peek();
      switch (char) {
        case " ":
        case "\r":
        case "\t":
          tick();
          $column++;
          break;
        case "\n":
          $line++;
          $column = 0;
          tick();
          break;
        default:
          return;
      }
    }
  };

  /**
   * Returns true if the given character is a Latin or
   * Greek character, false otherwise.
   * @param char A 1-character string.
   * @returns A boolean.
   */
  const isLatinGreekChar = (char: string): boolean =>
    /^[a-zA-Z_$\u00C0-\u02AF\u0370-\u03FF\u2100-\u214F]$/.test(char);

  /**
   * Returns true if the given character is a Unicode
   * math symbol, false otherwise.
   * @param char A 1-character string.
   * @returns A boolean.
   */
  const isMathSymbol = (char: string): boolean => /^[∀-⋿]/u.test(char);

  /**
   * Returns true if the given character is a valid
   * character to the start of a name, false
   * otherwise.
   * @param char A 1-char string.
   * @returns A boolean.
   */
  const isValidNameChar = (char: string) =>
    isLatinGreekChar(char) || isMathSymbol(char);

  /**
   * Returns true if the given `char` is a digit.
   * @param char A 1-char string.
   * @returns A boolean.
   */
  const isDigit = (char: string) => "0" <= char && char <= "9";

  /** Dictionary of keywords to tokens. */
  const dictionary: Record<string, () => Token> = {
    this: () => tkn(token_type.this),
    super: () => tkn(token_type.super),
    class: () => tkn(token_type.class),
    false: () => tkn(token_type.false).withLiteral(false),
    true: () => tkn(token_type.true).withLiteral(true),
    NaN: () => tkn(token_type.nan).withLiteral(NaN),
    Inf: () => tkn(token_type.inf).withLiteral(Infinity),
    return: () => tkn(token_type.return),
    while: () => tkn(token_type.while),
    for: () => tkn(token_type.for),
    let: () => tkn(token_type.let),
    var: () => tkn(token_type.var),
    fn: () => tkn(token_type.fn),
    if: () => tkn(token_type.if),
    else: () => tkn(token_type.else),
    print: () => tkn(token_type.print),
    rem: () => tkn(token_type.rem),
    mod: () => tkn(token_type.mod),
    div: () => tkn(token_type.div),
    nil: () => tkn(token_type.nil),
    and: () => tkn(token_type.and),
    or: () => tkn(token_type.or),
    nor: () => tkn(token_type.nor),
    xor: () => tkn(token_type.xor),
    xnor: () => tkn(token_type.xnor),
    not: () => tkn(token_type.not),
    nand: () => tkn(token_type.nand),
  };

  /**
   * Record of native functions. Each key corresponds
   * to the native function name. The number mapped to
   * by the key is the function’s arity (the number
   * of arguments the function takes).
   */
  const nativeFunctions: Record<NativeFn, number> = {
    deriv: 1,
    avg: 1,
    gcd: 1,
    simplify: 1,
    subex: 1,
    sqrt: 1,
    exp: 1,
    ceil: 1,
    tanh: 1,
    floor: 1,
    sinh: 1,
    cosh: 1,
    sin: 1,
    cos: 1,
    tan: 1,
    lg: 1,
    ln: 1,
    log: 1,
    arctan: 1,
    arccos: 1,
    arccosh: 1,
    arcsin: 1,
    arcsinh: 1,
    "!": 1,
    max: 1,
    min: 1,
  };

  /** Generates a word token. */
  const wordToken = () => {
    while ((isValidNameChar(peek()) || isDigit(peek())) && !atEnd()) {
      tick();
    }
    const word = slice();
    const native = nativeFunctions[word as NativeFn];
    if (native) {
      return tkn(token_type.native);
    } else if (dictionary[word]) {
      return dictionary[word]();
    } else {
      return tkn(token_type.symbol);
    }
  };

  const isHexDigit = (char: string) =>
    ("0" <= char && char <= "9") ||
    ("a" <= char && char <= "f") ||
    ("A" <= char && char <= "F");

  const isOctalDigit = (char: string) => "0" <= char && char <= "7";

  /**
   * Scans and returns a BIG_NUMBER token.
   * @returns A Token of type BIG_NUMBER.
   */
  const bigNumberToken = () => {
    while (isDigit(peek()) && !atEnd()) tick();
    const n = slice().replace("#", "");
    return tkn(token_type.big_number).withLiteral(BigInt(n));
  };

  /**
   * Scans a binary number token.
   * @returns A Token of type INTEGER.
   */
  const binaryNumberToken = () => {
    if (!(peekIs("0") || peekIs("1"))) {
      return errorToken(`Expected binary digits after “0b”`);
    }
    while ((peekIs("0") || peekIs("1")) && !atEnd()) {
      tick();
    }
    const numberString = slice().replace("0b", "");
    const integerValue = Number.parseInt(numberString, 2);
    return tkn(token_type.integer).withLiteral(integerValue);
  };

  /**
   * Scans an octal number token.
   * @returns A Token of type INTEGER.
   */
  const octalNumberToken = () => {
    if (!isOctalDigit(peek())) {
      return errorToken("Expected octal digits after");
    }
    while (isOctalDigit(peek()) && !atEnd()) {
      tick();
    }
    const numberString = slice().replace("0o", "");
    const integerValue = Number.parseInt(numberString, 8);
    return tkn(token_type.integer).withLiteral(integerValue);
  };

  /**
   * Scans a hexadecimal number token.
   * @returns A Token of type INTEGER.
   */
  const hexNumberToken = () => {
    if (!isHexDigit(peek())) {
      return errorToken("Expected hexadecimals after 0x");
    }
    while (isHexDigit(peek()) && !atEnd()) {
      tick();
    }
    const numberString = slice().replace("0x", "");
    const integerValue = Number.parseInt(numberString, 16);
    return tkn(token_type.integer).withLiteral(integerValue);
  };

  /** Generates number token. */
  const numTkn = (
    numberString: string,
    type: NumberTokenType,
    hasSeparators: boolean
  ) => {
    const n = hasSeparators ? numberString.replaceAll("_", "") : numberString;
    switch (type) {
      // handle integers
      case token_type.integer: {
        const num = Number.parseInt(n);
        if (num > Number.MAX_SAFE_INTEGER) {
          return errorToken(
            `Encountered an integer overflow. Consider rewriting “${numberString}” as a bignumber: “#${numberString}”. If “${numberString}” is to be used symbolically, consider rewriting “${numberString}” as a scientific number.`
          );
        } else {
          return tkn(type).withLiteral(num);
        }
      }
      // handle floats
      case token_type.float: {
        const num = Number.parseFloat(n);
        if (num > Number.MAX_VALUE) {
          return errorToken(
            `Encountered a floating point overflow. Consider rewriting "${n}" as a fraction or bigfraction. If "${n}" is to be used symbolically, consider rewriting "${n}" as a scientific number.`
          );
        }
      }
      // handle fractions
      case token_type.fraction: {
        const [a, b] = n.split("|");
        const N = Number.parseInt(a);
        const D = Number.parseInt(b);
        return tkn(type).withLiteral(fraction(N, D));
      }
      // handle scientific numbers
      case token_type.scientific: {
        const [a, b] = n.split("E");
        const base = Number.parseFloat(a);
        const exponent = Number.parseInt(b);
        return tkn(type).withLiteral(scinum(base, exponent));
      }
    }
    return errorToken(`Unrecognized number: "${n}".`);
  };

  const numberToken = (initialType: NumberTokenType): Token => {
    let type = initialType;
    let hasSeparators = false;
    while (isDigit(peek()) && !atEnd()) {
      tick();
    }

    // handle number with separators
    if (peekIs("_") && isDigit(peekNext())) {
      tick(); // eat the '_'
      hasSeparators = true;
      let digits = 0;
      while (isDigit(peek()) && !atEnd()) {
        tick();
        digits++;
        if (peekIs("_") && isDigit(peekNext())) {
          if (digits === 3) {
            tick();
            digits = 0;
          } else {
            return errorToken(
              'Expected 3 ASCII digits after the separator "_"'
            );
          }
        }
      }
      // there must be 3 ASCII digits after the "_"
      if (digits !== 3) {
        return errorToken('Expected 3 ASCII digits after the separator "_"');
      }
    }

    // handle floating point numbers
    if (peekIs(".") && isDigit(peekNext())) {
      tick();
      type = token_type.float;
      while (isDigit(peek()) && !atEnd()) {
        tick();
      }
    }

    // handle fractions
    if (peekIs("|")) {
      if (type !== token_type.integer) {
        return errorToken('Expected an integer before "|"');
      }
      type = token_type.fraction;
      tick();
      while (isDigit(peek()) && !atEnd()) {
        tick();
      }
      return numTkn(slice(), type, hasSeparators);
    }

    if (peekIs("E")) {
      if (isDigit(peekNext())) {
        type = token_type.scientific;
        tick();
        while (isDigit(peek())) tick();
      } else if (
        (peekNext() === "+" || peekNext() === "-") &&
        isDigit(lookup(2))
      ) {
        type = token_type.scientific;
        tick();
        tick();
        while (isDigit(peek())) tick();
      }
    }
    return numTkn(slice(), type, hasSeparators);
  };

  /**
   * Scans for a string token.
   * @returns A Token.
   */
  const stringToken = () => {
    while (peek() !== '"' && !atEnd()) {
      if (peek() === "\n") {
        $line++;
        $column = 0;
      } else {
        $column++;
      }
      tick();
    }
    if (atEnd()) return errorToken("Unterminated string");
    tick();
    const lex = slice().slice(1, -1);
    return tkn(token_type.string, lex);
  };

  const algebraStringToken = () => {
    while (peek() !== `'` && !atEnd()) {
      if (peek() === `\n`) {
        $line++;
        $column = 0;
      } else {
        $column++;
      }
      tick();
    }
    if (atEnd()) {
      return errorToken(`Unterminated algebraic string`);
    }
    tick(); // eat the ':'
    const s = slice().replaceAll(`'`, "");
    return tkn(token_type.algebra_string).withLiteral(s);
  };

  const scan = (): Token => {
    // Start by skipping whitespace.
    skipWhitespace();

    // Set the $start and $current pointers
    // to the same characters.
    $start = $current;

    // If we've reached the end of the source code,
    // immediately return an END token.
    if (atEnd()) return tkn(token_type.end, "END");

    // Now get the current character and move the
    // scanner forward.
    const char = tick();

    // If the character is a valid name starter (a Latin
    // or Greek character, a unicode math symbol,
    // an underscore, or a `$`), returns a word token.
    if (isValidNameChar(char)) return wordToken();

    // If the character is '#' then we have either
    // a BIG_NUMBER token, or a matrix operator.
    if (char === "#") {
      if (isDigit(peek())) {
        return bigNumberToken();
      } else if (match("+")) {
        return tkn(token_type.pound_plus);
      } else if (match("-")) {
        return tkn(token_type.pound_minus);
      } else if (match("*")) {
        return tkn(token_type.pound_star);
      } else {
        return errorToken('Expected digits after "#".');
      }
    }
    // If the character is a digit, then we have
    // a number token.
    if (isDigit(char)) {
      if (char === "0" && match("b")) {
        return binaryNumberToken();
      } else if (char === "0" && match("o")) {
        return octalNumberToken();
      } else if (char === "0" && match("x")) {
        return hexNumberToken();
      } else {
        return numberToken(token_type.integer);
      }
    }
    switch (char) {
      case ":":
        return tkn(token_type.colon);
      case "&":
        return tkn(token_type.ampersand);
      case "~":
        return tkn(token_type.tilde);
      case "|":
        return tkn(token_type.vbar);
      case "(":
        return tkn(token_type.left_paren);
      case ")":
        return tkn(token_type.right_paren);
      case "[":
        return tkn(token_type.left_bracket);
      case "]":
        return tkn(token_type.right_bracket);
      case "{":
        return tkn(token_type.left_brace);
      case "}":
        return tkn(token_type.right_brace);
      case ",":
        return tkn(token_type.comma);
      case "*":
        return tkn(token_type.star);
      case ";":
        return tkn(token_type.semicolon);
      case "%":
        return tkn(token_type.percent);
      case "/":
        return tkn(token_type.slash);
      case "^":
        return tkn(token_type.caret);
      case "!":
        return tkn(match("=") ? token_type.bang_equal : token_type.bang);
      case "<":
        return tkn(match("=") ? token_type.less_equal : token_type.less);
      case ">":
        return tkn(match("=") ? token_type.greater_equal : token_type.greater);
      case '"':
        return stringToken();
      case "+":
        return tkn(match("+") ? token_type.plus_plus : token_type.plus);
      case "'":
        return algebraStringToken();
      // Special handling of dot for vector operators.
      case ".": {
        if (match("+")) {
          return tkn(token_type.dot_add);
        } else if (match("-")) {
          return tkn(token_type.dot_minus);
        } else if (match("*")) {
          return tkn(token_type.dot_star);
        } else if (match("^")) {
          return tkn(token_type.dot_caret);
        } else {
          return tkn(token_type.dot);
        }
      }

      // Special handling of dash for inline comments.
      case "-": {
        if (peek() === "-" && peekNext() === "-") {
          while (peek() !== "\n" && !atEnd()) {
            tick();
          }
          return Token.empty;
        } else {
          return tkn(match("-") ? token_type.minus_minus : token_type.minus);
        }
      }

      // special handling of '=' for block comments.
      case "=": {
        if (peek() === "=" && peekNext() === "=") {
          while (peek() === "=") tick();
          while (!atEnd()) {
            tick();
            if (peek() === "=" && peekNext() === "=" && lookup(2) === "=") {
              break;
            }
          }
          if (atEnd()) {
            return errorToken("Unterminated block comment");
          }
          while (peek() === "=") tick();
          return Token.empty;
        } else {
          return tkn(match("=") ? token_type.equal_equal : token_type.equal);
        }
      }
    }
    return errorToken(`Unknown token: ${char}`);
  };

  const stream = () => {
    const out: Token[] = [];
    let prev = Token.empty;
    let now = scan();
    if (!now.isType(token_type.empty)) {
      out.push(now);
    } else if ($error !== null) {
      return left($error);
    }
    let peek = scan();
    if ($error !== null) {
      return left($error);
    }
    while (!atEnd()) {
      prev = now;
      now = peek;
      const k = scan();
      if ($error !== null) {
        return left($error);
      }
      if (k.isType(token_type.empty)) {
        continue;
      } else {
        peek = k;
      }
      // remove trailing commas
      if (
        prev.isRightDelimiter() &&
        now.isType(token_type.comma) &&
        peek.isRightDelimiter()
      ) {
        continue;
      }
      out.push(now);
    }
    out.push(peek);
    return right(out);
  };

  return { stream, scan, atEnd };
}

// § Nodekind Enum
enum nodekind {
  class_statement,
  block_statement,
  string_binex,
  grouped_expression,
  expression_statement,
  function_declaration,
  if_statement,
  print_statement,
  return_statement,
  variable_declaration,
  vector_binex,
  while_statement,
  algebra_string,
  tuple_expression,
  vector_expression,
  matrix_expression,
  assignment_expression,
  native_call,
  algebraic_unary,
  algebraic_infix,
  logical_unary,
  call,
  nil,
  fraction_expression,
  numeric_constant,
  integer,
  float,
  bool,
  string,
  symbol,
  logical_infix,
  let_expression,
  get_expression,
  set_expression,
  super_expression,
  this_expression,
  relation,
  index_expression,
  big_number,
  big_rational,
}

interface Visitor<T> {
  blockStmt(node: BlockStatement): T;
  exprStmt(node: ExprStmt): T;
  fnStmt(node: FnStmt): T;
  ifStmt(node: IfStmt): T;
  printStmt(node: PrintStmt): T;
  returnStmt(node: ReturnStmt): T;
  variableStmt(node: VariableStmt): T;
  whileStmt(node: WhileStmt): T;
  // expressions
  indexExpr(node: IndexExpr): T;
  algebraString(node: AlgebraString): T;
  tupleExpr(node: TupleExpr): T;
  vectorExpr(node: VectorExpr): T;
  matrixExpr(node: MatrixExpr): T;
}

/** A node corresponding to some syntax tree node. */
abstract class TreeNode {
  abstract kind(): nodekind;
}

/** A node corresponding to an abstract syntax tree node. */
abstract class ASTNode extends TreeNode {
  abstract accept<T>(visitor: Visitor<T>): T;
  abstract toString(): string;
  abstract isStatement(): this is Statement;
  abstract isExpr(): this is Expr;
}

/** A node corresponding to a statement node. */
abstract class Statement extends ASTNode {
  isStatement(): this is Statement {
    return true;
  }
  isExpr(): this is Expr {
    return false;
  }
}

// TODO - Class Statement Node

/** A node corresponding to a block statement node. */
class BlockStatement extends Statement {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.blockStmt(this);
  }
  kind(): nodekind {
    return nodekind.block_statement;
  }
  toString(): string {
    return "block-statement";
  }
  /** The statements comprising this block. */
  $statements: Statement[];
  constructor(statements: Statement[]) {
    super();
    this.$statements = statements;
  }
}

/** Returns true, and asserts, iff the given node is a block statement node. */
function isBlockStmt(node: ASTNode): node is BlockStatement {
  return node.kind() === nodekind.block_statement;
}

/** Returns a new block statement node. */
function blockStmt(statements: Statement[]) {
  return new BlockStatement(statements);
}

/** A node corresponding to an expression statement. */
class ExprStmt extends Statement {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.exprStmt(this);
  }
  kind(): nodekind {
    return nodekind.expression_statement;
  }
  toString(): string {
    return "expression-statement";
  }
  $expression: Expr;
  constructor(expression: Expr) {
    super();
    this.$expression = expression;
  }
}

/** Returns a new Expression Statement. */
function exprStmt(expression: Expr) {
  return new ExprStmt(expression);
}

/** A node corresponding to a function declaration statement. */
class FnStmt extends Statement {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.fnStmt(this);
  }
  kind(): nodekind {
    return nodekind.function_declaration;
  }
  toString(): string {
    return "fn-declaration";
  }
  $name: Token<token_type.symbol>;
  $params: Sym[];
  $body: Statement[];
  constructor(
    name: Token<token_type.symbol>,
    params: Sym[],
    body: Statement[]
  ) {
    super();
    this.$name = name;
    this.$params = params;
    this.$body = body;
  }
}

/** Returns a new function declaration statement. */
function fnStmt(
  name: Token<token_type.symbol>,
  params: Sym[],
  body: Statement[]
) {
  return new FnStmt(name, params, body);
}

/** An AST node corresponding to an if-then statement. */
class IfStmt extends Statement {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.ifStmt(this);
  }
  kind(): nodekind {
    return nodekind.if_statement;
  }
  toString(): string {
    return "if-statement";
  }
  $keyword: Token;
  $condition: Expr;
  $then: Statement;
  $alt: Statement;
  constructor(
    keyword: Token,
    condition: Expr,
    then: Statement,
    alt: Statement
  ) {
    super();
    this.$keyword = keyword;
    this.$condition = condition;
    this.$then = then;
    this.$alt = alt;
  }
}

/** Returns a new if-then statement. */
function ifStmt(
  keyword: Token,
  condition: Expr,
  then: Statement,
  alt: Statement
) {
  return new IfStmt(keyword, condition, then, alt);
}

/** An AST node corresponding to a print statement. */
class PrintStmt extends Statement {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.printStmt(this);
  }
  kind(): nodekind {
    return nodekind.print_statement;
  }
  toString(): string {
    return "print-statement";
  }
  $keyword: Token;
  $expression: Expr;
  constructor(keyword: Token, expression: Expr) {
    super();
    this.$keyword = keyword;
    this.$expression = expression;
  }
}

/** Returns a new print statement node. */
function printStmt(keyword: Token, expression: Expr) {
  return new PrintStmt(keyword, expression);
}

/** A node corresponding to a return statement. */
class ReturnStmt extends Statement {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.returnStmt(this);
  }
  kind(): nodekind {
    return nodekind.return_statement;
  }
  toString(): string {
    return "return-statement";
  }
  $keyword: Token;
  $value: Expr;
  constructor(keyword: Token, value: Expr) {
    super();
    this.$keyword = keyword;
    this.$value = value;
  }
}

/** Returns a new return statement node. */
function returnStmt(keyword: Token, expression: Expr) {
  return new ReturnStmt(keyword, expression);
}

class VariableStmt extends Statement {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.variableStmt(this);
  }
  kind(): nodekind {
    return nodekind.variable_declaration;
  }
  toString(): string {
    return "variable-declaration";
  }
  $variable: Sym;
  $value: Expr;
  $mutable: boolean;
  constructor(variable: Sym, value: Expr, mutable: boolean) {
    super();
    this.$variable = variable;
    this.$value = value;
    this.$mutable = mutable;
  }
}

/** Returns a new 'var' statement node. */
function varStmt(symbol: Sym, value: Expr) {
  return new VariableStmt(symbol, value, true);
}

/** Returns a new 'let' statement node. */
function letStmt(symbol: Sym, value: Expr) {
  return new VariableStmt(symbol, value, false);
}

/** An AST node corresponding to a while statement. */
class WhileStmt extends Statement {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.whileStmt(this);
  }
  kind(): nodekind {
    return nodekind.while_statement;
  }
  toString(): string {
    return "while-statement";
  }
  $keyword: Token;
  $condition: Expr;
  $body: Statement;
  constructor(keyword: Token, condition: Expr, body: Statement) {
    super();
    this.$keyword = keyword;
    this.$condition = condition;
    this.$body = body;
  }
}

/** Returns a new while statement node. */
function whileStmt(keyword: Token, condition: Expr, body: Statement) {
  return new WhileStmt(keyword, condition, body);
}

/** A node corresponding to an expression node. */
abstract class Expr extends ASTNode {
  isStatement(): this is Statement {
    return false;
  }
  isExpr(): this is Expr {
    return true;
  }
}

/** An AST node corresponding to an indexing expression. */
class IndexExpr extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.indexExpr(this);
  }
  kind(): nodekind {
    return nodekind.index_expression;
  }
  toString(): string {
    return `${this.$list.toString()}[${this.$index.toString()}]`;
  }
  $list: Expr;
  $index: Expr;
  $op: Token;
  constructor(list: Expr, index: Expr, op: Token) {
    super();
    this.$list = list;
    this.$index = index;
    this.$op = op;
  }
}

/** Returns a new indexing expression node. */
function indexExpr(list: Expr, index: Expr, op: Token) {
  return new IndexExpr(list, index, op);
}

/** An AST node corresponding to an algebra string. */
class AlgebraString extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.algebraString(this);
  }
  kind(): nodekind {
    return nodekind.algebra_string;
  }
  toString(): string {
    return this.$expression.toString();
  }
  $expression: Expr;
  $op: Token;
  constructor(expression: Expr, op: Token) {
    super();
    this.$expression = expression;
    this.$op = op;
  }
}

/** Returns a new algebra string node. */
function algebraString(expression: Expr, op: Token) {
  return new AlgebraString(expression, op);
}

/** A node corresponding to a tuple expression. */
class TupleExpr extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.tupleExpr(this);
  }
  kind(): nodekind {
    return nodekind.tuple_expression;
  }
  toString(): string {
    const elems = this.$elements.map((e) => e.toString()).join(",");
    return `(${elems})`;
  }
  $elements: LinkedList<Expr>;
  constructor(elements: Expr[]) {
    super();
    this.$elements = linkedList(...elements);
  }
}

/** Returns a new tuple expression. */
function tupleExpr(elements: Expr[]) {
  return new TupleExpr(elements);
}

class VectorExpr extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.vectorExpr(this);
  }
  kind(): nodekind {
    return nodekind.vector_expression;
  }
  toString(): string {
    const elems = this.$elements.map((e) => e.toString()).join(",");
    return `[${elems}]`;
  }
  $op: Token;
  $elements: Expr[];
  constructor(op: Token, elements: Expr[]) {
    super();
    this.$op = op;
    this.$elements = elements;
  }
}

/** Returns a new vector expression node. */
function vectorExpr(op: Token, elements: Expr[]) {
  return new VectorExpr(op, elements);
}

/** A node corresponding to a matrix expression. */
class MatrixExpr extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.matrixExpr(this);
  }
  kind(): nodekind {
    return nodekind.matrix_expression;
  }
  toString(): string {
    const vectors = this.$vectors.map((v = v.toString())).join(",");
    return `[${vectors}]`;
  }
  $vectors: VectorExpr[];
  $rowCount: number;
  $colCount: number;
  constructor(vectors: VectorExpr[], rows: number, columns: number) {
    super();
    this.$vectors = vectors;
    this.$rowCount = rows;
    this.$colCount = columns;
  }
}

/** Returns a new matrix expression. */
function matrixExpr(vectors: VectorExpr[], rowCount: number, colCount: number) {
  return new MatrixExpr(vectors, rowCount, colCount);
}
