#!/usr/bin/env node

/**
 * GNU nano Syntax File Generator for Hew
 *
 * Reads the canonical syntax-data.json from the Hew compiler and generates
 * a nano syntax highlighting file at dist/hew.nanorc.
 *
 * Colour mapping:
 *   Control flow keywords    → brightwhite
 *   Declaration keywords     → green
 *   Actor/concurrency        → brightgreen
 *   Supervisor               → brightgreen
 *   Wire protocol            → green
 *   Machine keywords         → green
 *   Other keywords           → green
 *   Strategy/policy constants→ brightmagenta
 *   Primitive types           → brightblue
 *   Generic/collection types  → brightblue
 *   Traits                    → brightblue
 *   Boolean/None              → brightmagenta
 *   Reserved keywords         → green
 *
 * Usage: node tools/downstream/generate-nano.mjs
 *
 * Environment variables:
 *   HEW_SYNTAX_DATA   Path to syntax-data.json
 *                      (default: REPO_ROOT/docs/syntax-data.json)
 */

import { readFileSync, writeFileSync, existsSync, mkdirSync } from 'fs';
import { resolve, dirname } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));
const REPO_ROOT = resolve(__dirname, '../..');

const syntaxDataPath = process.env.HEW_SYNTAX_DATA
  || resolve(REPO_ROOT, 'docs/syntax-data.json');
const outputPath = resolve(REPO_ROOT, 'dist/hew.nanorc');

// -- Validate inputs -------------------------------------------------------

if (!existsSync(syntaxDataPath)) {
  console.error(`Error: syntax-data.json not found at ${syntaxDataPath}`);
  console.error('Set HEW_SYNTAX_DATA env var to the correct path.');
  process.exit(1);
}

console.log(`Reading syntax data from: ${syntaxDataPath}`);
console.log(`Writing output to:        ${outputPath}\n`);

const syntaxData = JSON.parse(readFileSync(syntaxDataPath, 'utf8'));

const kw = syntaxData.keywords;
const types = syntaxData.types;

// -- Helper: build \<(word1|word2|...)\> regex for nano --------------------

function nanoKeywordRegex(words) {
  return `"\\<(${words.join('|')})\\>"`;
}

// -- Build output lines ----------------------------------------------------

const lines = [];

function emit(line) { lines.push(line); }
function blank() { lines.push(''); }

emit('## Hew syntax highlighting for GNU nano');
emit('## Generated from syntax-data.json — do not edit by hand.');
emit(`## syntax-data.json version: ${syntaxData.version}`);
blank();
emit('syntax "hew" "\\.hew$"');
blank();

// Comments
emit('# Comments');
emit('color cyan "//.*$"');
emit('color cyan start="/\\*" end="\\*/"');
blank();

// Strings
emit('# Strings');
emit('color yellow ""([^"\\\\]|\\\\.)*""');
emit("color yellow \"'([^'\\\\]|\\\\.)*'\"");
emit('# Raw strings');
emit('color yellow "r\\"([^\\"])*\\""');
blank();

// Regex literals
emit('# Regex literals');
emit('color yellow "re\\"([^\\"])*\\""');
blank();

// F-strings
emit('# F-strings');
emit('color yellow "f\\"([^"\\\\]|\\\\.)*\\""');
blank();

// Escape sequences
emit('# Escape sequences in strings');
emit('color brightyellow "\\\\[nrt\\\\\\"0]"');
emit('color brightyellow "\\\\x[0-9a-fA-F]{2}"');
blank();

// Numbers
emit('# Numbers');
emit('color brightmagenta "\\<0[xX][0-9a-fA-F][0-9a-fA-F_]*\\>"');
emit('color brightmagenta "\\<0[bB][01][01_]*\\>"');
emit('color brightmagenta "\\<0[oO][0-7][0-7_]*\\>"');
emit('color brightmagenta "\\<[0-9][0-9_]*\\.[0-9][0-9_]*([eE][+-]?[0-9][0-9_]*)?\\>"');

// Duration literals
const durationSuffixes = syntaxData.literal_suffixes?.duration || ['ns', 'us', 'ms', 's', 'm', 'h'];
emit(`color brightmagenta "\\<[0-9][0-9_]*(${durationSuffixes.join('|')})\\>"`);
emit('color brightmagenta "\\<[0-9][0-9_]*\\>"');
blank();

// Boolean & None
emit('# Boolean & None');
const booleans = [...kw.logical, 'None'];
emit(`color brightmagenta ${nanoKeywordRegex(booleans)}`);
blank();

// Control flow keywords
// Actor keywords that serve as control flow are mixed in
emit('# Control flow keywords');
const controlFlow = [...new Set([
  ...kw.control_flow,
  'select', 'join', 'yield', 'cooperate', 'after', 'from', 'await',
  'scope',
])];
// Split across multiple lines for readability if needed
const cfChunks = chunkArray(controlFlow, 10);
for (const chunk of cfChunks) {
  emit(`color brightwhite ${nanoKeywordRegex(chunk)}`);
}
blank();

// Declaration keywords
emit('# Declaration keywords');
const declarations = [...kw.declarations];
const declChunks = chunkArray(declarations, 10);
for (const chunk of declChunks) {
  emit(`color green ${nanoKeywordRegex(chunk)}`);
}
blank();

// Actor & concurrency
emit('# Actor & concurrency');
const actorWords = ['actor', 'init', 'move', 'receive', 'spawn', 'terminate', 'this'];
emit(`color brightgreen ${nanoKeywordRegex(actorWords)}`);
blank();

// Supervisor
emit('# Supervisor');
const supervisorWords = ['supervisor', 'child', 'restart', 'budget', 'strategy'];
emit(`color brightgreen ${nanoKeywordRegex(supervisorWords)}`);
blank();

// Wire protocol
emit('# Wire protocol');
emit(`color green ${nanoKeywordRegex(kw.wire)}`);
blank();

// Machine keywords
emit('# Machine keywords');
emit(`color green ${nanoKeywordRegex(kw.machine)}`);
blank();

// Other keywords
emit('# Other keywords');
emit(`color green ${nanoKeywordRegex(kw.other)}`);
blank();

// Strategy & policy constants
emit('# Strategy & policy constants');
const strategies = ['one_for_one', 'one_for_all', 'rest_for_one'];
const permanence = ['permanent', 'transient', 'temporary'];
emit(`color brightmagenta ${nanoKeywordRegex(strategies)}`);
emit(`color brightmagenta ${nanoKeywordRegex(permanence)}`);
blank();

// Reserved keywords
emit('# Reserved keywords');
emit(`color green ${nanoKeywordRegex(kw.reserved_unused)}`);
blank();

// Types — primitives
emit('# Types — primitives');
const numericTypes = [...types.integer, ...types.float];
emit(`color brightblue ${nanoKeywordRegex(numericTypes)}`);
const primitiveTypes = [...types.primitive];
emit(`color brightblue ${nanoKeywordRegex(primitiveTypes)}`);
blank();

// Types — generic / collection / concurrency
emit('# Types — generic / collection / concurrency');
const genericTypes = [
  ...types.collections,
  'Option', 'Result', 'Ok', 'Err', 'Some',
  'Box', 'Arc', 'Rc', 'Weak',
  ...types.other,
];
emit(`color brightblue ${nanoKeywordRegex(genericTypes)}`);
const concurrencyTypes = [...types.concurrency];
emit(`color brightblue ${nanoKeywordRegex(concurrencyTypes)}`);
blank();

// Traits
emit('# Traits');
const traits = [
  'Send', 'Frozen', 'Copy', 'Drop', 'Clone', 'Eq', 'Ord', 'Hash',
  'Display', 'Debug', 'Default', 'Iterator', 'AsyncIterator',
  'IntoIterator', 'Into', 'From', 'Try', 'Allocator',
];
const traitChunks = chunkArray(traits, 10);
for (const chunk of traitChunks) {
  emit(`color brightblue ${nanoKeywordRegex(chunk)}`);
}
blank();

// Self type
emit('# Self type');
emit('color brightblue "\\<Self\\>"');
blank();

// PascalCase type names
emit('# PascalCase type names');
emit('color brightblue "\\<[A-Z][a-zA-Z0-9_]*\\>"');
blank();

// Function definitions
emit('# Function definitions');
emit('color brightcyan "\\<fn +[a-zA-Z_][a-zA-Z0-9_]*"');
blank();

// Operators
emit('# Operators');
emit('color white "->|=>|<-|\\.\\.[ =]?"');
emit('color white "==[^=]|!=|=~|!~|<=|>="');
emit('color white "<<=|>>=|&=|\\|=|\\^=|\\+=|-=|\\*=|/=|%="');
emit('color white "<<|>>"');
emit('color white "::"');
blank();

// Attributes
emit('# Attributes');
emit('color magenta "#\\[[^\\]]*\\]"');
blank();

// Labels
emit('# Labels');
emit('color red "@[a-zA-Z_][a-zA-Z0-9_]*"');
blank();

// TODO/FIXME in comments
emit('# TODO/FIXME in comments');
emit('color brightyellow,cyan "\\<(TODO|FIXME|XXX|NOTE|HACK)\\>"');

// -- Write output ----------------------------------------------------------

mkdirSync(dirname(outputPath), { recursive: true });
writeFileSync(outputPath, lines.join('\n') + '\n');

console.log(`Generated nano syntax file (${lines.length} lines)`);
console.log('Done.');

// -- Helpers ---------------------------------------------------------------

function chunkArray(arr, size) {
  const chunks = [];
  for (let i = 0; i < arr.length; i += size) {
    chunks.push(arr.slice(i, i + size));
  }
  return chunks;
}
