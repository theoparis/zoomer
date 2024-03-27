import { transformSync } from "esbuild";
import { createHash } from "node:crypto";
import * as fs from "node:fs/promises";

export type TokenKind =
	| "word" // any word
	| "username" // any valid username (starting with `@`)
	| (typeof keywords)[number] // the keywords
	| "eof"
	| "number"
	| "string";

export const keywords = [
	"bro really", // function declaration
	"fr", // end of statement or block
	"L", // like a (
	"ratio", // like a )
	"bet", // start of block
	"dox", // ptr reference
	"swat", // ptr dereference
	"cap", // false constant
	"no", // not operator
	"you dropped this:", // exit the program
	"0.0.0.0",
] as const;

const usernameRegex = /^@[a-zA-Z0-9_\.-]+/;
const wordRegex = /^[^@\s][^\s]+/;
const numberRegex = /^[+-]?(\d+(\.\d*)?|\.\d+)([eE][+-]?\d+)?/;

export interface Token {
	kind: TokenKind;
	text: string;
}

export class Lexer {
	buffer: string;
	current: number;

	extraTokens: Token[] = [];

	constructor(buffer: string) {
		this.buffer = buffer;
		this.current = 0;
	}

	next(): Token {
		if (this.extraTokens.length > 0) {
			return this.extraTokens.shift()!;
		}

		// skip whitespace
		while (
			this.buffer[this.current] === " " ||
			this.buffer[this.current] === "\n" ||
			this.buffer[this.current] === "\t"
		) {
			this.current++;
		}

		if (this.current >= this.buffer.length) {
			return { kind: "eof", text: "" };
		}

		const current = this.buffer.slice(this.current);

		let match = /^(fr)+/.exec(current);
		if (match) {
			// for each fr we add a fr token
			const frCount = match[0].length / 2;
			for (let i = 0; i < frCount - 1; i++) {
				this.extraTokens.push({ kind: "fr", text: "fr" });
			}

			this.current += match[0].length;
			return { kind: "fr", text: "fr" };
		}

		for (const keyword of keywords) {
			if (current.startsWith(keyword)) {
				this.current += keyword.length;
				return { kind: keyword, text: keyword };
			}
		}

		match = usernameRegex.exec(current);
		if (match) {
			this.current += match[0].length;
			return { kind: "username", text: match[0] };
		}

		match = numberRegex.exec(current);
		if (match) {
			this.current += match[0].length;
			return { kind: "number", text: match[0] };
		}

		if (current.startsWith('"')) {
			let i = 1;
			while (current[i] !== '"') {
				i++;
			}
			const text = JSON.parse(current.slice(0, i + 1));
			this.current += i + 1;
			return { kind: "string", text };
		}

		match = wordRegex.exec(current);
		if (match) {
			this.current += match[0].length;
			return { kind: "word", text: match[0] };
		}

		throw new Error(`Lexer Error: what the fuck is \"${current[0]}\"`);
	}

	peek(): Token {
		if (this.extraTokens.length > 0) {
			return this.extraTokens[0];
		}
		const nextToken = this.next();
		this.extraTokens.unshift(nextToken);
		return nextToken;
	}

	*[Symbol.iterator]() {
		let token = this.next();
		while (token.kind !== "eof") {
			yield token;
			token = this.next();
		}
		yield token;
	}
}

/// Parsing

export interface Node {
	type: NodeType;
}

export interface FnDeclNode {
	type: "FnDecl";
	name: string;
	args: string[];
	block: BlockNode;
}

export interface BlockNode {
	type: "Block";
	stmts: StatementNode[];
}

export type StatementNode =
	| YouDroppedThisNode
	| YeetNode
	| FnDeclNode
	| WasNode
	| UnreachableNode
	| ExpressionNode;

export type ExpressionNode =
	| ItOnGodNode
	| CallNode
	| NoNode
	| DoxNode
	| SwatNode
	| LiteralNode
	| UsernameNode;

export interface YouDroppedThisNode {
	type: "YouDroppedThis";
	exitCode: number;
}

export interface ItOnGodNode {
	type: "ItOnGod";
	expr: ExpressionNode;
}

export interface YeetNode {
	type: "YeetNode";
	expr: ExpressionNode;
}

export interface CallNode {
	type: "Call";
	name: string;
	args: ExpressionNode[];
}

export interface NoNode {
	type: "No";
	expr: ExpressionNode;
}
export interface DoxNode {
	type: "Dox";
	expr: ExpressionNode;
}
export interface SwatNode {
	type: "Swat";
	expr: ExpressionNode;
}
export interface LiteralNode {
	type: "Literal";
	value: string | number | false;
}
export interface WasNode {
	type: "Was";
	username: string;
	expr: ExpressionNode;
}
export interface UnreachableNode {
	type: "Unreachable";
	message: ExpressionNode;
}
export interface UsernameNode {
	type: "Username";
	name: string;
}
export interface AppNode {
	type: "App";
	stmts: StatementNode[];
}

const lookupExitCode: Record<string, number> = {
	"👑": 0,
	"🔥": 1,
	"💥": 2,
};

class Parser {
	lexer: Lexer;

	constructor(bufferOrLexer: Lexer | string) {
		if (typeof bufferOrLexer === "string") {
			this.lexer = new Lexer(bufferOrLexer);
		} else {
			this.lexer = bufferOrLexer;
		}
	}

	nextToken(kind?: TokenKind) {
		const next = this.lexer.next();
		if (kind) {
			if (next.kind !== kind) {
				throw new SyntaxError(
					`Expected a \"${kind}\" token, got \"${next.text}\"`,
				);
			}
		}
		return next;
	}

	parseApp(): AppNode {
		const block: AppNode = {
			type: "App",
			stmts: [],
		};
		while (this.lexer.peek().kind !== "eof") {
			block.stmts.push(this.parseStatement());
		}
		this.nextToken("eof");
		return block;
	}

	parseFnDecl(): FnDeclNode {
		this.nextToken("bro really");
		let next = this.lexer.peek();
		let name = "";
		let args: string[] = [];
		while (next.kind !== "bet") {
			this.lexer.next();
			if (next.kind === "username") {
				name += "@ ";
				args.push(next.text);
			} else if (next.kind === "word") {
				name += next.text + " ";
			} else {
				throw new SyntaxError(`Unexpected \"${next.text}\"`);
			}
			next = this.lexer.peek();
		}
		const block = this.parseBlock();
		return { type: "FnDecl", name: name.trim(), args, block };
	}

	parseBlock(): BlockNode {
		this.nextToken("bet");
		const block: BlockNode = {
			type: "Block",
			stmts: [],
		};
		while (this.lexer.peek().kind !== "fr") {
			block.stmts.push(this.parseStatement());
		}
		this.nextToken("fr");
		return block;
	}

	passthroughfr<T extends Node>(node: T): T {
		this.nextToken("fr");
		return node;
	}

	parseStatement(): StatementNode {
		switch (this.lexer.peek().kind) {
			case "you dropped this:":
				this.nextToken();
				const exitCode = this.lexer.peek();
				if (
					exitCode.kind === "word" &&
					lookupExitCode[exitCode.text] !== undefined
				) {
					this.nextToken();
					return {
						type: "YouDroppedThis",
						exitCode: lookupExitCode[exitCode.text],
					};
				}
				return {
					type: "Unreachable",
					message: this.parseExpression(),
				};
			case "bro really":
				return this.parseFnDecl();
			case "bet":
				throw new SyntaxError(`"${this.lexer.peek().text}" cant be said rn`);
			case "eof":
				throw new SyntaxError(`Where's the rest?`);
			case "word":
				return this.passthroughfr(this.parseCall(true));
			case "username":
				const username = this.nextToken("username").text;
				const was = this.nextToken("word");
				if (was.text !== "was") {
					return this.passthroughfr(this.parseCall(true));
				}
				const expr = this.passthroughfr(this.parseCall(true));
				return { type: "Was", username, expr };
			default:
				return this.passthroughfr(this.parseCall(true));
		}
	}

	parseExpression(): ExpressionNode {
		switch (this.lexer.peek().kind) {
			case "0.0.0.0":
				this.nextToken("0.0.0.0");
				return { type: "Literal", value: "null" };
			case "no":
				this.nextToken("no");
				return { type: "No", expr: this.parseCall(false) };
			case "cap":
				this.nextToken("cap");
				return { type: "Literal", value: false };
			case "dox":
				this.nextToken("dox");
				return { type: "Dox", expr: this.parseCall(false) };
			case "swat":
				this.nextToken("swat");
				return { type: "Swat", expr: this.parseCall(false) };
			case "L":
				this.nextToken("L");
				const expr = this.parseCall(false);
				this.nextToken("ratio");
				return expr;
			case "string":
			case "number":
				const token = this.nextToken();
				const value = token.kind === "string" ? token.text : +token.text;
				return { type: "Literal", value };
			default:
				throw new SyntaxError(
					`What the fuck does "${this.lexer.peek().text}" mean?`,
				);
		}
	}

	parseCall(canBeOnGod: boolean): ExpressionNode {
		let call_name = "";
		let call_args: ExpressionNode[] = [];

		while (true) {
			const kind = this.lexer.peek().kind;
			if (kind === "word") {
				call_name += this.nextToken().text + " ";
			} else if (kind === "username") {
				call_name += "@ ";
				call_args.push({ type: "Username", name: this.nextToken().text });
			} else if (kind === "fr" || kind === "ratio") {
				break;
			} else {
				call_name += "@ ";
				call_args.push(this.parseExpression());
			}
		}

		if (call_name.trim() === "@") {
			return call_args[0];
		}

		if (call_name.trim() === "it's @ on god") {
			if (!canBeOnGod) {
				throw new SyntaxError("You cant be on god rn");
			}

			return { type: "ItOnGod", expr: call_args[0] };
		}

		return { type: "Call", name: call_name.trim(), args: call_args };
	}
}

export interface Context {
	uses_ips: boolean;
	defined_functions: string[];
	called_functions: string[];
}

type NodeType = (StatementNode | ExpressionNode | BlockNode | AppNode)["type"];
type NodeToJSMap = {
	[K in NodeType]: (
		ctx: Context,
		node: Extract<
			StatementNode | ExpressionNode | BlockNode | AppNode,
			{ type: K }
		>,
	) => string;
};

function usernameToJS(username: string) {
	if (username === "null" || username === "undefined") {
		return "_" + username;
	}

	return username
		.slice(1)
		.replace(/^\d/, "_$&")
		.replace(/[_\.-]/g, (x) => "_" + (["_", ".", "-"].indexOf(x) + 1));
}

function fnNameToJS(name: string) {
	return (
		name
			.replace(/^\d/, "_$&")
			.replace(/ *@ */g, (_x) => `_param_`)
			.replace(/[^a-zA-Z0-9_]/g, "_") +
		(Number(createHash("sha256").update(name).digest("hex")) % 999)
	);
}

const standard_library = {
	"print @": (args: any) => `console.log(${args})`,
	"print @ without new line": (args: any) => `process.stdout.write(${args})`,
	"sum of @ and @": (a: any, b: any) => `${a}+${b}`,
};

const nodeToJS: NodeToJSMap = {
	App: (c, node) => {
		let code = "";
		for (const stmt of node.stmts) {
			code += nodeToJS[stmt.type](c, stmt as any) + ";\n";
		}
		code += `\n${fnNameToJS("ran the app with @")}(process.argv.slice(2))`;
		return code;
	},
	Block: (c, node) => {
		let code = "{";
		for (const stmt of node.stmts) {
			code += nodeToJS[stmt.type](c, stmt as any) + ";\n";
		}
		code += "}";
		return code;
	},
	No: (c, node) => `!(${node.expr})`,
	Username: (c, node) => `${usernameToJS(node.name)}`,
	YouDroppedThis: (c, node) => `process.exit(${node.exitCode})`,
	YeetNode: (c, node) =>
		`throw ${nodeToJS[node.expr.type](c, node.expr as any)}`,
	ItOnGod: (c, node) =>
		`return(${nodeToJS[node.expr.type](c, node.expr as any)})`,
	Unreachable: (c, node) =>
		`throw new Error(${nodeToJS[node.message.type](c, node.message as any)})`,
	Was: (c, node) => {
		const username = usernameToJS(node.username);
		return `let ${username}=${nodeToJS[node.expr.type](c, node.expr as any)}`;
	},
	Literal: (c, node) => JSON.stringify(node.value),
	FnDecl: (c, node) => {
		const args = node.args.map((x) => usernameToJS(x)).join(",");
		const body = nodeToJS.Block(c, node.block);
		return `function ${fnNameToJS(node.name)}(${args})${body}`;
	},
	Call: (c, node) => {
		const args = node.args.map((x) => nodeToJS[x.type](c, x as any));
		if (standard_library[node.name]) {
			return "(" + standard_library[node.name](...args) + ")";
		}
		return `${fnNameToJS(node.name)}(${args.join(",")})`;
	},
	Dox: (c, node) => {
		c.uses_ips = true;
		return `__dox(${nodeToJS[node.expr.type](c, node.expr as any)})`;
	},
	Swat: (c, node) => {
		c.uses_ips = true;
		return `__swat(${nodeToJS[node.expr.type](c, node.expr as any)})`;
	},
};

const code = await fs.readFile(process.argv[2] ?? "index.zoomer", "utf-8");

console.log(code.trim());
console.log("---");
const ctx = {
	uses_ips: false,
	defined_functions: [],
	called_functions: [],
};
let result = nodeToJS.App(ctx, new Parser(code).parseApp());
if (ctx.uses_ips) {
	result = `const __dox_map = new WeakMap();
const __box = Symbol("box");
const __dox = (x) => {
  if(x == null) {
    return null;
  }
  x = { [__box]: x };
  var existing = __dox_map.get(x);
  if(existing) return existing.value;
  const text = [0, 0, 0, 0].map(x=>Math.floor(Math.random()*256)).join(".");
  const ip = new String(text);
  ip._deref = new WeakRef(x);
  __dox_map.set(x, ip);
  ip[Symbol.for("nodejs.util.inspect.custom")] = (_, { stylize }) => stylize(ip, "special");
  return ip;
}
const __swat = (x) => {
  if(x == null) throw new Error("Cannot send the swat team to 0.0.0.0");
  try {
    var existing = x._deref.deref()[__box];
    if(existing) return existing;
    throw new Error("Swat team failed to find the person at the ip address");
  } catch(e) {
    throw new Error("Not a valid IP address");
  }
}
${result}`;
}

const x = transformSync(result, {}).code;

eval(x);
