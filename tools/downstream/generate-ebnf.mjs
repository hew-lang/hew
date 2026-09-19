#!/usr/bin/env node

// Project the editor grammar into a readable syntax view. Parsing and native
// execution remain owned by the compiler; this file adds no grammar decisions.
import { createHash } from 'node:crypto';
import { readFileSync, writeFileSync, mkdirSync } from 'node:fs';
import { dirname } from 'node:path';

const [source, output, ...extra] = process.argv.slice(2);
if (!source || !output || extra.length) {
  console.error('Usage: generate-ebnf.mjs GRAMMAR_JSON OUTPUT_EBNF');
  process.exit(1);
}
const input = readFileSync(source, 'utf8');
const grammar = JSON.parse(input);
if (grammar.name !== 'hew' || !grammar.rules?.source_file) {
  throw new Error('Expected the Hew tree-sitter grammar with a source_file rule');
}
if (grammar.externals?.length) {
  throw new Error('External scanner tokens need an explicit syntax-view description');
}

function render(node) {
  switch (node.type) {
    case 'SYMBOL':
      if (!(node.name in grammar.rules)) throw new Error(`Undefined rule: ${node.name}`);
      return node.name;
    case 'STRING': return JSON.stringify(node.value);
    case 'PATTERN': return `? regular expression ${JSON.stringify(node.value)} ?`;
    case 'BLANK': return '""';
    case 'SEQ': return node.members.map(render).join(' ');
    case 'CHOICE': {
      const members = node.members.filter(member => member.type !== 'BLANK');
      const choices = members.map(render).join(' | ');
      if (members.length !== node.members.length) return `[ ${choices} ]`;
      return `( ${choices} )`;
    }
    case 'REPEAT': return `{ ${render(node.content)} }`;
    case 'REPEAT1': {
      const item = render(node.content);
      return `${item} { ${item} }`;
    }
    case 'FIELD':
    case 'ALIAS':
    case 'TOKEN':
    case 'IMMEDIATE_TOKEN':
    case 'PREC':
    case 'PREC_LEFT':
    case 'PREC_RIGHT':
    case 'PREC_DYNAMIC':
      return render(node.content);
    default: throw new Error(`Unsupported grammar node: ${node.type}`);
  }
}

const digest = createHash('sha256').update(input).digest('hex');
const lines = [
  '(* Hew editor syntax view — generated; do not edit by hand.',
  '   Source: tree-sitter-hew/src/grammar.json',
  `   Source SHA-256: ${digest}`,
  '   Start rule: source_file',
  '',
  '   This view describes the editor grammar, not native execution support.',
  '   Precedence, lexical priority/adjacency, conflict resolution and tree',
  '   field/alias annotations remain in the tree-sitter source. Regular',
  '   expressions appear as special sequences. Whitespace concatenates terms.',
  '   Compiler parsing, checking and execution decide valid Hew programs.',
  '*)',
  '',
  `(* Globally skipped input: ${grammar.extras.map(render).join(' | ')} *)`,
  '',
];
for (const [name, rule] of Object.entries(grammar.rules)) {
  lines.push(`${name} = ${render(rule)} ;`, '');
}
mkdirSync(dirname(output), { recursive: true });
writeFileSync(output, lines.join('\n'));
console.log(`Generated ${Object.keys(grammar.rules).length} syntax-view rules: ${output}`);
