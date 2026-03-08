#!/usr/bin/env node

/**
 * Tree-Sitter Grammar Keyword Patcher for Hew
 *
 * Reads the canonical syntax-data.json and patches keyword choice() blocks
 * in grammar.js between `// @sync:<category>` markers.
 *
 * For each `// @sync:<category>` marker, finds the next choice(...) call
 * and replaces the keyword strings inside with the matching category from
 * syntax-data.json. Non-string arguments (identifiers, seq(...) calls, etc.)
 * are preserved in their original positions.
 *
 * Supported categories:
 *   primitive_types, wire_types, wire_attributes, overflow_kinds,
 *   restart_permanence, restart_strategies, duration_suffixes,
 *   assignment_operators, boolean_literals
 *
 * Usage: node tools/downstream/patch-tree-sitter-keywords.mjs [GRAMMAR_PATH]
 *
 * Environment variables:
 *   HEW_SYNTAX_DATA     Path to syntax-data.json
 *                        (default: REPO_ROOT/docs/syntax-data.json)
 *   HEW_TREE_SITTER      Path to tree-sitter-hew repo
 *                        (default: REPO_ROOT/../tree-sitter-hew)
 */

import { readFileSync, writeFileSync, existsSync } from 'fs';
import { resolve, dirname } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));
const REPO_ROOT = resolve(__dirname, '../..');

const syntaxDataPath = process.env.HEW_SYNTAX_DATA
  || resolve(REPO_ROOT, 'docs/syntax-data.json');
const treeSitterDir = process.env.HEW_TREE_SITTER
  || resolve(REPO_ROOT, '..', 'tree-sitter-hew');
const grammarPath = process.argv[2]
  || resolve(treeSitterDir, 'grammar.js');

// -- Validate inputs -------------------------------------------------------

if (!existsSync(syntaxDataPath)) {
  console.error(`Error: syntax-data.json not found at ${syntaxDataPath}`);
  console.error('Set HEW_SYNTAX_DATA env var to the correct path.');
  process.exit(1);
}

if (!existsSync(grammarPath)) {
  console.error(`Error: grammar.js not found at ${grammarPath}`);
  console.error('Set HEW_TREE_SITTER env var or pass the path as an argument.');
  process.exit(1);
}

console.log(`Reading syntax data from: ${syntaxDataPath}`);
console.log(`Patching grammar at:      ${grammarPath}\n`);

const syntaxData = JSON.parse(readFileSync(syntaxDataPath, 'utf8'));
let source = readFileSync(grammarPath, 'utf8');

const kw = syntaxData.keywords;
const types = syntaxData.types;

// -- Category → keyword list mapping --------------------------------------

const categoryKeywords = {
  primitive_types: [
    ...types.integer, ...types.float,
    ...types.primitive.filter(t => t !== 'never'), // 'never' is a type, not a parser keyword
  ],
  wire_types: [
    // Wire types are a subset of primitives usable in wire declarations
    'u8', 'u16', 'u32', 'u64',
    'i8', 'i16', 'i32', 'i64',
    'f32', 'f64',
    'bool', 'bytes', 'string',
  ],
  // Only standalone wire attributes — 'default' and 'reserved' appear in seq() expressions
  wire_attributes: kw.wire.filter(w => !['wire', 'default', 'reserved'].includes(w)),
  overflow_kinds: ['block', 'drop_new', 'drop_old', 'fail'],
  restart_permanence: kw.supervisor_config.filter(
    k => ['permanent', 'transient', 'temporary'].includes(k)
  ),
  restart_strategies: kw.supervisor_config.filter(
    k => ['one_for_one', 'one_for_all', 'rest_for_one'].includes(k)
  ),
  duration_suffixes: syntaxData.literal_suffixes?.duration || ['ns', 'us', 'ms', 's', 'm', 'h'],
  assignment_operators: syntaxData.operators.assignment,
  boolean_literals: kw.logical,
};

// -- Patch logic -----------------------------------------------------------

const changes = [];

/**
 * Find all `// @sync:<category>` markers and patch the next choice(...)
 * call by replacing quoted string arguments.
 */
const markerRe = /\/\/\s*@sync:(\w+)/g;
let match;

while ((match = markerRe.exec(source)) !== null) {
  const category = match[1];
  const newKeywords = categoryKeywords[category];

  if (!newKeywords) {
    console.warn(`  ? Unknown @sync category: ${category} (skipped)`);
    continue;
  }

  // Find the next choice( or just the next parenthesized group after the marker
  const afterMarker = source.slice(match.index + match[0].length);

  // Look for `choice(` — might be on same line or next line
  const choiceMatch = afterMarker.match(/\bchoice\s*\(/);
  if (!choiceMatch) {
    // For some categories (like assignment_operators, boolean_literals),
    // the pattern might be a direct choice(...) without the word "choice"
    // e.g., field('operator', choice('=', '+=', ...))
    // Let's look for choice( in the broader context
    const choiceInLine = afterMarker.match(/choice\s*\(/);
    if (!choiceInLine) {
      console.warn(`  ? No choice() found after @sync:${category} marker (skipped)`);
      continue;
    }
  }

  const choiceStart = match.index + match[0].length + afterMarker.indexOf('choice(');
  const choiceContentStart = source.indexOf('(', choiceStart) + 1;

  // Find the matching closing paren, tracking nesting
  let depth = 1;
  let pos = choiceContentStart;
  while (pos < source.length && depth > 0) {
    if (source[pos] === '(') depth++;
    else if (source[pos] === ')') depth--;
    pos++;
  }
  const choiceContentEnd = pos - 1; // position of the closing ')'

  const choiceContent = source.slice(choiceContentStart, choiceContentEnd);

  // Parse the choice content to identify string literals vs complex expressions
  // We want to replace string literals but preserve complex args like:
  //   seq('coalesce', '(', $.identifier, ')')
  //   seq('default', '(', $.expression, ')')
  //   $.identifier
  //   seq('list', '[', $.wire_type, ']')

  const newContent = buildNewChoiceContent(choiceContent, newKeywords, category);

  if (newContent !== null && newContent !== choiceContent) {
    source = source.slice(0, choiceContentStart) + newContent + source.slice(choiceContentEnd);
    changes.push(category);
    // Reset regex lastIndex since source length changed
    markerRe.lastIndex = choiceContentStart + newContent.length;
  }
}

/**
 * Build new choice(...) content by replacing simple string literal arguments
 * with the new keyword list, while preserving complex expressions (seq calls,
 * $. references, etc.).
 */
function buildNewChoiceContent(content, newKeywords, category) {
  // Separate complex arguments (containing seq, $., etc.) from simple strings
  const complexArgs = [];

  // Parse top-level comma-separated args, respecting nesting
  const args = splitTopLevelArgs(content);

  for (const arg of args) {
    const trimmed = arg.trim();
    // A simple string literal is just 'word' or "word"
    if (/^'[^']*'$/.test(trimmed) || /^"[^"]*"$/.test(trimmed)) {
      // Simple string — will be replaced
      continue;
    } else if (trimmed.length > 0) {
      // Complex expression — preserve it
      complexArgs.push(trimmed);
    }
  }

  // Detect the indentation of the choice content
  const indent = detectIndent(content);

  // Build new keyword strings
  const keywordStrings = newKeywords.map(k => `'${k}'`);

  // Determine formatting: if original was single-line, keep single-line
  const isMultiline = content.includes('\n');

  if (isMultiline) {
    // Multi-line format: group keywords onto lines of ~80 chars (matching
    // the original grammar.js style of multiple keywords per line)
    const maxLineLen = 80;
    const kwLines = [];
    let currentLine = indent;

    for (const kwStr of keywordStrings) {
      const candidate = currentLine === indent
        ? currentLine + kwStr
        : currentLine + ', ' + kwStr;
      if (candidate.length > maxLineLen && currentLine !== indent) {
        kwLines.push(currentLine + ',');
        currentLine = indent + kwStr;
      } else {
        currentLine = candidate;
      }
    }
    if (currentLine !== indent) {
      kwLines.push(currentLine + ',');
    }

    // Append complex args, each on its own line
    const complexLines = complexArgs.map(a => `${indent}${a},`);
    const allLines = [...kwLines, ...complexLines];
    const outerIndent = indent.replace(/  $/, '');
    return '\n' + allLines.join('\n') + '\n' + outerIndent;
  } else {
    const allArgs = [...keywordStrings, ...complexArgs];
    return allArgs.join(', ');
  }
}

/**
 * Split a string at top-level commas, respecting parentheses nesting.
 */
function splitTopLevelArgs(content) {
  const args = [];
  let depth = 0;
  let current = '';

  for (let i = 0; i < content.length; i++) {
    const ch = content[i];
    if (ch === '(' || ch === '[') {
      depth++;
      current += ch;
    } else if (ch === ')' || ch === ']') {
      depth--;
      current += ch;
    } else if (ch === ',' && depth === 0) {
      args.push(current);
      current = '';
    } else {
      current += ch;
    }
  }
  if (current.trim().length > 0) {
    args.push(current);
  }
  return args;
}

/**
 * Detect the indentation used in multi-line choice content.
 */
function detectIndent(content) {
  const lineMatch = content.match(/\n(\s+)/);
  return lineMatch ? lineMatch[1] : '      ';
}

// -- Write output ----------------------------------------------------------

writeFileSync(grammarPath, source);

// -- Report ----------------------------------------------------------------

if (changes.length === 0) {
  console.log('No changes needed — grammar.js already in sync.');
} else {
  console.log(`Patched ${changes.length} @sync section(s):`);
  for (const c of changes) {
    console.log(`  ~ @sync:${c}`);
  }
}
console.log('\nDone.');
