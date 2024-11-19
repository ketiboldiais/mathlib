type Either<A, B> = Left<A> | Right<B>;

/** A box type corresponding to failure. */
class Left<T> {
  private value: T;
  constructor(value: T) {
    this.value = value;
  }
  map<A>(f: (x: never) => A): Either<T, never> {
    return this as any;
  }
  isLeft(): this is Left<T> {
    return true;
  }
  isRight(): this is never {
    return false;
  }
  chain<X, S>(f: (x: never) => Either<X, S>): Left<T> {
    return this;
  }
  read<K>(value: K): K {
    return value;
  }
  flatten(): Left<T> {
    return this;
  }
  unwrap() {
    return this.value;
  }
  ap<B, E>(f: Either<T, E>): Either<never, B> {
    return this as any;
  }
}

/** A box type corresponding success. */
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
  chain<N, X>(f: (x: T) => Either<N, X>): Either<never, X> {
    return f(this.value) as Either<never, X>;
  }
  flatten(): Right<T extends Right<infer T> ? T : never> {
    return (
      this.value instanceof Right || this.value instanceof Left
        ? this.value
        : this
    ) as Right<T extends Right<infer T> ? T : never>;
  }
  read<K>(_: K): T {
    return this.value;
  }
  unwrap() {
    return this.value;
  }
  ap<B, E>(f: Either<E, (x: T) => B>): Either<never, B> {
    if (f.isLeft()) return f as any as Right<B>;
    return this.map(f.value);
  }
}

/** Returns a new left. */
const left = <T>(x: T): Left<T> => new Left(x);

/** Returns a new right. */
const right = <T>(x: T): Right<T> => new Right(x);

type ErrorType =
  | "lexical-error"
  | "syntax-error"
  | "type-error"
  | "runtime-error"
  | "resolver-error"
  | "environment-error"
  | "algebraic-error";

class Erratum extends Error {
  message: string;
  errorType: ErrorType;
  phase: string;
  line: number;
  column: number;
  fix: string;
  constructor(
    message: string,
    errorType: ErrorType,
    phase: string,
    line: number,
    column: number,
    fix: string = "none"
  ) {
    super(message);
    this.message = message;
    this.errorType = errorType;
    this.phase = phase;
    this.line = line;
    this.column = column;
    this.fix = fix;
  }
  report() {
    return formattedError(
      this.message,
      this.phase,
      this.errorType,
      this.line,
      this.column,
      this.fix
    );
  }
}

/** Ensures all errors have the same format. */
function formattedError(
  message: string,
  phase: string,
  errorType: ErrorType,
  line: number,
  column: number,
  recommendation: string
) {
  let moduleName = "module";
  switch (errorType) {
    case "lexical-error":
      moduleName = "scanner";
      break;
    case "algebraic-error":
      moduleName = "algebraic tree transfomer";
      break;
    case "environment-error":
      moduleName = "environment";
      break;
    case "resolver-error":
      moduleName = "resolver";
      break;
    case "runtime-error":
      moduleName = "interpreter";
      break;
    case "syntax-error":
      moduleName = "parser";
      break;
    case "type-error":
      moduleName = "typechecker";
  }
  return `${errorType.toUpperCase()}.\nWhile ${phase}, an error occurred on\nline ${line}, column ${column}.\nReporting from the ${moduleName}:\n${message} \nRecommendation: ${recommendation}`;
}

/** Returns a new error making function. */
const errorFactory =
  (errtype: ErrorType) =>
  (message: string, phase: string, token: Token, fix: string = "none") =>
    new Erratum(message, errtype, phase, token.line, token.column);

/** Returns a new lexical error. A lexical error is raised if an error occured during scanning. */
const lexicalError = errorFactory("lexical-error");

/** Returns a new syntax error. A syntax error is raised if an error occured during parsing. */
const syntaxError = errorFactory("syntax-error");

/** Returns a new runtime error. A runtime error is raised if an error occured during interpretation. */
const runtimeError = errorFactory("runtime-error");

/** Returns a new environment error. An environment error is raised if an error occured during an environment lookup. */
const envError = errorFactory("environment-error");

/** Returns a new resolver error. A resolver error is raised if an error occured during resolution. */
const resolverError = errorFactory("resolver-error");

/** Returns a new algebra error. An algebra error is raised if an error occured during a symbolic operation. Because symbolic operations are handled by the symbol engine, these errors must be handled separately. */
const algebraError = errorFactory("algebraic-error");

enum TokenType {
  // single-character tokens
  left_paren,
  right_paren,
  left_brace,
  right_brace,
  left_bracket,
  right_bracket,
  comma,
  dot,
  minus,
  plus,
  semicolon,
  slash,
  star,
  colon,
  ampersand,
  caret,
  percent,

  // one or two character tokens
  bang,
  bang_equal,
  equal,
  equal_equal,
  greater,
  greater_equal,
  less,
  less_equal,

  // literals
  identifier,
  string,
  integer,
  float,

  // keywords
  and,
  class,
  else,
  false,
  for,
  if,
  nil,
  or,
  print,
  return,
  super,
  this,
  true,
  var,
  while,
  nan,
  inf,
  pi,
  e,
  let,
  fn,
  rem,
  mod,
  div,
  nor,
  xor,
  xnor,
  not,
  nand,

  // utility tokens
  error,
  eof,
  empty,
}

type NumberTokenType = TokenType.integer | TokenType.float;

type Primitive = string | number | boolean | null;

/** An object corresponding to a token in Winnow. */
class Token {
  /** This token's type. */
  tokenType: TokenType;
  /** This token's lexeme. */
  lexeme: string;
  /** The line where this token was recognized. */
  line: number;
  column: number;
  literal: Primitive = null;
  constructor(
    tokenType: TokenType,
    lexeme: string,
    line: number,
    column: number
  ) {
    this.tokenType = tokenType;
    this.lexeme = lexeme;
    this.line = line;
    this.column = column;
  }
  /** Sets this token's literal value to the given primitive, and returns this. */
  lit(value: Primitive) {
    this.literal = value;
    return this;
  }
  /** Returns true if this token is of the given type. */
  isType(type: TokenType) {
    return this.tokenType === type;
  }
  /** Returns a string representation of this token. */
  toString() {
    const type = TokenType[this.tokenType];
    const lexeme = this.lexeme;
    const line = this.line;
    return `[${type} | ${lexeme} | ${line}]`;
  }
}

/** Returns a new token. */
const newToken = (
  type: TokenType,
  lexeme: string,
  line: number,
  column: number
) => new Token(type, lexeme, line, column);

/** Returns an empty token. */
const emptyToken = () => newToken(TokenType.empty, "", -1, -1);

/** Returns true if the string `char` is a Latin or Greek character. */
function isLatinGreek(char: string) {
  return /^[a-zA-Z_$\u00C0-\u02AF\u0370-\u03FF\u2100-\u214F]$/.test(char);
}

/** Returns true if the given string `char` is within the unicode range `∀-⋿`. Else, returns false. */
function isMathSymbol(char: string) {
  return /^[∀-⋿]/u.test(char);
}

/** Returns true if the given string `char` is a digit. Else, returns false. */
function isDigit(char: string) {
  return "0" <= char && char <= "9";
}

/** Returns true if the given string char is a hex digit. Else, returns false. */
function isHexDigit(char: string) {
  return (
    ("0" <= char && char <= "9") ||
    ("a" <= char && char <= "f") ||
    ("A" <= char && char <= "F")
  );
}

/** Returns true if the given string char is an octal digit. Else, returns false. */
function isOctalDigit(char: string) {
  return "0" <= char && char <= "7";
}

/** Returns true if the given string char is a binary digit. Else, returns false. */
function isBinaryDigit(char: string) {
  return char === "0" || char === "1";
}

/** Returns true if the given string `char` is a Latin/Greek character or a math symbol. Else, returns false. */
function isValidName(char: string) {
  return isLatinGreek(char) || isMathSymbol(char);
}

export function lexical(source: string) {
  let $line = 1;
  let $column = 1;
  let $start = 0;
  let $current = 0;
  let $error: Erratum | null = null;
  const peek = () => source[$current];
  const peekNext = () => (isAtEnd() ? "" : source[$current + 1]);
  const slice = () => source.slice($start, $current);
  const isAtEnd = () => $current >= source.length;
  const makeToken = (tokenType: TokenType, lexeme: string = "") => {
    const lex = lexeme ? lexeme : slice();
    const out = newToken(tokenType, lex, $line, $column);
    return out;
  };
  const errorToken = (message: string) => {
    const err = newToken(TokenType.error, message, $line, $column);
    $error = lexicalError(message, "scanning", err);
    return err;
  };
  const advance = () => source[$current++];
  const skipWhitespace = () => {
    while (true) {
      const c = peek();
      switch (c) {
        case " ":
        case "\r":
        case "\t":
          advance();
          $column++;
          break;
        case "\n":
          $line++;
          $column = 0;
          advance();
          break;
        default:
          return;
      }
    }
  };

  const match = (expectedChar: string) => {
    if (isAtEnd()) {
      return false;
    } else if (source[$current] !== expectedChar) {
      return false;
    } else {
      $current++;
      return true;
    }
  };

  const binaryNumber = () => {
    if (!isBinaryDigit(peek())) {
      return errorToken("Expected binary digits after 0b");
    }
    while ((peek() === "0" || peek() === "1") && !isAtEnd()) {
      advance();
    }
    const s = slice().replace("0b", "");
    const n = Number.parseInt(s, 2);
    return makeToken(TokenType.integer, `${n}`).lit(n);
  };

  const octalNumber = () => {
    if (!isOctalDigit(peek())) {
      return errorToken("Expected octal digits after 0o");
    }
    while (isOctalDigit(peek()) && !isAtEnd()) {
      advance();
    }
    const s = slice().replace("0o", "");
    const n = Number.parseInt(s, 8);
    return makeToken(TokenType.integer).lit(n);
  };

  const hexNumber = () => {
    if (!isHexDigit(peek())) {
      return errorToken("Expected hexadecimals after 0x");
    }
    while (isHexDigit(peek()) && !isAtEnd()) {
      advance();
    }
    const s = slice().replace("0x", "");
    const n = Number.parseInt(s, 16);
    return makeToken(TokenType.integer).lit(n);
  };

  const stringLiteral = () => {
    while (peek() !== `"` && !isAtEnd()) {
      if (peek() === `\n`) {
        $line++;
        $column = 0;
      } else {
        $column++;
      }
      advance();
    }
    if (isAtEnd()) {
      return errorToken("Unterminated string");
    }
    advance();
    const lex = slice().slice(1, -1);
    return makeToken(TokenType.string, lex);
  };

  const word = () => {
    while ((isValidName(peek()) || isDigit(peek())) && !isAtEnd()) {
      advance();
    }
    const string = slice();
    switch (string) {
      case "this":
        return makeToken(TokenType.this);
      case "super":
        return makeToken(TokenType.super);
      case "class":
        return makeToken(TokenType.class);
      case "false":
        return makeToken(TokenType.false);
      case "true":
        return makeToken(TokenType.true);
      case "nan":
        return makeToken(TokenType.nan);
      case "inf":
        return makeToken(TokenType.inf);
      case "pi":
        return makeToken(TokenType.pi);
      case "e":
        return makeToken(TokenType.e);
      case "return":
        return makeToken(TokenType.return);
      case "while":
        return makeToken(TokenType.while);
      case "for":
        return makeToken(TokenType.for);
      case "let":
        return makeToken(TokenType.let);
      case "var":
        return makeToken(TokenType.var);
      case "fn":
        return makeToken(TokenType.fn);
      case "if":
        return makeToken(TokenType.if);
      case "else":
        return makeToken(TokenType.else);
      case "print":
        return makeToken(TokenType.print);
      case "rem":
        return makeToken(TokenType.rem);
      case "mod":
        return makeToken(TokenType.mod);
      case "div":
        return makeToken(TokenType.div);
      case "nil":
        return makeToken(TokenType.nil);
      case "and":
        return makeToken(TokenType.and);
      case "or":
        return makeToken(TokenType.or);
      case "nor":
        return makeToken(TokenType.nor);
      case "xor":
        return makeToken(TokenType.xor);
      case "xnor":
        return makeToken(TokenType.xnor);
      case "not":
        return makeToken(TokenType.not);
      case "nand":
        return makeToken(TokenType.nand);
    }
    return makeToken(TokenType.identifier);
  };

  const ntoken = (
    numberString: string,
    type: NumberTokenType,
    hasSeparators: boolean
  ) => {
    const n = hasSeparators ? numberString.replaceAll("_", "") : numberString;
    switch (type) {
      case TokenType.integer: {
        const num = Number.parseInt(n);
        return makeToken(TokenType.integer).lit(num);
      }
      case TokenType.float: {
        const num = Number.parseFloat(n);
        return makeToken(TokenType.float).lit(num);
      }
    }
  };

  const numberToken = (initialType: NumberTokenType) => {
    let type = initialType;
    let scannedSeparators = false;
    while (isDigit(peek()) && !isAtEnd()) {
      advance();
    }
    if (peek() === "_" && isDigit(peekNext())) {
      advance();
      scannedSeparators = true;
      let digits = 0;
      while (isDigit(peek()) && !isAtEnd()) {
        advance();
        digits++;
        if (peek() === "_" && isDigit(peekNext())) {
          if (digits === 3) {
            advance();
            digits = 0;
          } else {
            return errorToken("There must be 3 digits before the separator _");
          }
        }
      }
      if (digits !== 3) {
        return errorToken("There must be 3 digits before the separator _");
      }
    }
    if (peek() === "." && isDigit(peekNext())) {
      advance();
      type = TokenType.float;
      while (isDigit(peek()) && !isAtEnd()) {
        advance();
      }
    }
    return ntoken(slice(), type, scannedSeparators);
  };

  const scan = () => {
    skipWhitespace();
    $start = $current;
    if (isAtEnd()) {
      return makeToken(TokenType.eof);
    }
    const c = advance();
    if (isValidName(c)) {
      return word();
    }
    if (isDigit(c)) {
      if (c === "0" && match("b")) {
        return binaryNumber();
      } else if (c === "0" && match("o")) {
        return octalNumber();
      } else if (c === "0" && match("x")) {
        return hexNumber();
      } else {
        return numberToken(TokenType.integer);
      }
    }
    switch (c) {
      case "(":
        return makeToken(TokenType.left_paren);
      case ")":
        return makeToken(TokenType.right_paren);
      case "{":
        return makeToken(TokenType.left_brace);
      case "}":
        return makeToken(TokenType.right_brace);
      case "[":
        return makeToken(TokenType.left_bracket);
      case "]":
        return makeToken(TokenType.right_bracket);
      case ":":
        return makeToken(TokenType.colon);
      case "&":
        return makeToken(TokenType.ampersand);
      case ";":
        return makeToken(TokenType.semicolon);
      case ",":
        return makeToken(TokenType.comma);
      case ".":
        return makeToken(TokenType.dot);
      case "-":
        return makeToken(TokenType.minus);
      case "+":
        return makeToken(TokenType.plus);
      case "/":
        return makeToken(TokenType.slash);
      case "*":
        return makeToken(TokenType.star);
      case "^":
        return makeToken(TokenType.caret);
      case "%":
        return makeToken(TokenType.percent);
      case "!":
        return makeToken(match("=") ? TokenType.equal_equal : TokenType.equal);
      case "=":
        return makeToken(match("=") ? TokenType.equal_equal : TokenType.equal);
      case "<":
        return makeToken(match("=") ? TokenType.less_equal : TokenType.less);
      case ">":
        return makeToken(
          match("=") ? TokenType.greater_equal : TokenType.greater
        );
      case `"`:
        return stringLiteral();
    }

    return errorToken(`Unexpected character: ${c}`);
  };

  const stream = () => {
    const out: Token[] = [];
    let prev = emptyToken();
    let now = scan();
    if (!now.isType(TokenType.empty)) {
      out.push(now);
    }
    let peek = scan();
    while (!isAtEnd()) {
      prev = now;
      now = peek;
      const k = scan();
      if (k.isType(TokenType.empty)) {
        continue;
      } else {
        peek = k;
      }
      out.push(now);
    }
    out.push(peek);
    return out;
  };

  return {
    stream,
    scan,
  };
}

/** The binding power of a given operator. Values of type `bp` are used the parsers to determinate operator precedence (both the Twine and CAM parsers use Pratt parsing for expressions). */
enum BP {
  nil,
  lowest,
  stringop,
  assign,
  atom,
  or,
  nor,
  and,
  nand,
  xor,
  xnor,
  not,
  eq,
  rel,
  sum,
  difference,
  product,
  quotient,
  imul,
  power,
  postfix,
  call,
}

enum nodekind {
  class_statement,
  block_statement,
  string_binex,
  grouped_expression,
  expression_statement,
  function_declaration,
  branching_statement,
  print_statement,
  return_statement,
  variable_declaration,
  vector_binex,
  loop_statement,
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
  indexing_expression,
  big_number,
  big_rational,
}

abstract class TREENODE {
  abstract get kind(): nodekind;
}

interface Visitor<T> {}

abstract class ASTNode extends TREENODE {
  abstract accept<T>(visitor: Visitor<T>): T;
}

/** @internal A Pratt parsing function. */
type Parslet<T> = (current: Token, lastNode: T) => Either<Erratum, T>;

/** @internal An entry within parser’s BP table. The first element is a prefix parslet, the second element is an infix parslet, and the last element is the binding power of the operator. */
type ParsletEntry<T> = [Parslet<T>, Parslet<T>, BP];

/** @internal A record of parslet entries, where each key is a token type (`tt`). */
type BPTable<T> = Record<TokenType, ParsletEntry<T>>;
