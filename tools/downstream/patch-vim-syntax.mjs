#!/usr/bin/env node

/**
 * Vim Syntax File Patcher for Hew
 *
 * Reads the canonical syntax-data.json and patches `syn keyword` groups in
 * hew.vim between `" @sync:<category>` markers.
 *
 * For each `" @sync:<category>` marker, replaces the following `syn keyword`
 * line(s) (until the next blank line, comment-only line, or marker) with
 * regenerated keyword lists from syntax-data.json.
 *
 * Supported categories:
 *   control_flow, declarations, actors, supervisor, wire, machine, other,
 *   logical, supervisor_config, reserved_unused, types
 *
 * Usage: node tools/downstream/patch-vim-syntax.mjs [HEW_VIM_PATH]
 *
 * Environment variables:
 *   HEW_SYNTAX_DATA   Path to syntax-data.json
 *                      (default: REPO_ROOT/docs/syntax-data.json)
 *   HEW_VIM_HEW       Path to vim-hew repo
 *                      (default: REPO_ROOT/../vim-hew)
 */

import { readFileSync, writeFileSync, existsSync } from 'fs';
import { resolve, dirname } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));
const REPO_ROOT = resolve(__dirname, '../..');

const syntaxDataPath = process.env.HEW_SYNTAX_DATA
  || resolve(REPO_ROOT, 'docs/syntax-data.json');
const vimHewDir = process.env.HEW_VIM_HEW
  || resolve(REPO_ROOT, '..', 'vim-hew');
const syntaxFilePath = process.argv[2]
  || resolve(vimHewDir, 'syntax/hew.vim');

// -- Validate inputs -------------------------------------------------------

if (!existsSync(syntaxDataPath)) {
  console.error(`Error: syntax-data.json not found at ${syntaxDataPath}`);
  console.error('Set HEW_SYNTAX_DATA env var to the correct path.');
  process.exit(1);
}

if (!existsSync(syntaxFilePath)) {
  console.error(`Error: hew.vim not found at ${syntaxFilePath}`);
  console.error('Set HEW_VIM_HEW env var or pass the path as an argument.');
  process.exit(1);
}

console.log(`Reading syntax data from: ${syntaxDataPath}`);
console.log(`Patching vim syntax at:   ${syntaxFilePath}\n`);

const syntaxData = JSON.parse(readFileSync(syntaxDataPath, 'utf8'));
const originalSource = readFileSync(syntaxFilePath, 'utf8');

const kw = syntaxData.keywords;
const types = syntaxData.types;

// -- Category → { group, keywords[] } mapping -----------------------------
// Each category produces one or more `syn keyword <group> <words...>` lines.

const categoryMap = {
  control_flow: {
    group: 'hewControl',
    keywords: [...new Set([
      ...kw.control_flow,
      // Actor keywords that serve as control flow
      'select', 'join', 'yield', 'cooperate', 'after', 'from', 'await',
      'scope',
    ])],
  },

  declarations: {
    group: 'hewDecl',
    keywords: [...kw.declarations],
  },

  actors: {
    group: 'hewActor',
    keywords: [...kw.actors],
  },

  supervisor: {
    group: 'hewSupervisor',
    keywords: ['supervisor', 'child', 'restart', 'budget', 'strategy'],
  },

  wire: {
    group: 'hewWire',
    keywords: [...kw.wire],
  },

  machine: {
    group: 'hewMachine',
    keywords: [...kw.machine],
  },

  other: {
    group: 'hewOther',
    keywords: [...kw.other],
  },

  logical: {
    group: 'hewBool',
    keywords: [...kw.logical],
    // Extra lines after the keyword line (not from syntax-data)
    extraLines: [
      'syn keyword hewNone        None',
      'syn keyword hewSelf        this',
      'syn keyword hewSelfType    Self',
    ],
  },

  supervisor_config: {
    group: 'hewStrategy',
    keywords: () => {
      const strategies = ['one_for_one', 'one_for_all', 'rest_for_one'];
      const permanence = ['permanent', 'transient', 'temporary'];
      const overflow = ['block', 'drop_new', 'drop_old', 'fail', 'coalesce', 'fallback'];
      return [
        { words: strategies },
        { words: permanence },
        { words: overflow },
      ];
    },
  },

  reserved_unused: {
    group: 'hewReserved',
    keywords: [...kw.reserved_unused],
  },

  types: {
    group: 'hewType',
    keywords: () => {
      const numericPrimitives = [...types.integer, ...types.float];
      const otherPrimitives = [...types.primitive];
      const generic = [
        ...types.collections,
        ...(types.option_result || []).filter(t => t !== 'None'),
        'Box', 'Arc', 'Rc', 'Weak',
      ];
      const concurrency = [...types.concurrency, ...(types.other || [])];
      const traitsList = [
        'Send', 'Frozen', 'Copy', 'Drop', 'Clone', 'Eq', 'Ord', 'Hash',
        'Display', 'Debug', 'Default', 'Iterator', 'AsyncIterator',
        'IntoIterator', 'Into', 'From', 'Try', 'Allocator',
      ];
      return [
        { words: numericPrimitives },
        { words: otherPrimitives },
        { words: generic },
        { words: concurrency },
        { words: traitsList },
      ];
    },
    // Extra lines after the keyword lines (not from syntax-data)
    extraLines: [
      'syn match   hewType        "\\<[A-Z][a-zA-Z0-9_]*\\>"',
    ],
  },
};

// -- Patch logic -----------------------------------------------------------

const lines = originalSource.split('\n');
const output = [];
const changes = [];

let i = 0;
while (i < lines.length) {
  const line = lines[i];
  const markerMatch = line.match(/^"\s*@sync:(\w+)\s*$/);

  if (!markerMatch) {
    output.push(line);
    i++;
    continue;
  }

  const category = markerMatch[1];
  const config = categoryMap[category];

  if (!config) {
    console.warn(`  ? Unknown @sync category: ${category} (preserved as-is)`);
    output.push(line);
    i++;
    continue;
  }

  // Keep the marker line
  output.push(line);
  i++;

  // Skip old `syn keyword`/`syn match` lines until next blank line, marker, or non-syn line
  while (i < lines.length) {
    const nextLine = lines[i];
    // Stop at blank lines, new markers, or non-syn comment lines (that aren't blank)
    if (nextLine.trim() === '') break;
    if (/^"\s*@sync:/.test(nextLine)) break;
    // Stop at lines that aren't syn keyword/match for this group and aren't comments
    if (!nextLine.startsWith('syn ') && !nextLine.startsWith('" ')) break;
    // Skip syn lines and inline comments
    i++;
  }

  // Generate new keyword lines
  const group = config.group;
  const maxLineWidth = 78;
  const prefix = `syn keyword ${group}`;
  // Pad to align keywords at a consistent column (original uses varying padding)
  const paddedPrefix = prefix.length < 23 ? prefix.padEnd(23) : prefix + ' ';

  if (typeof config.keywords === 'function') {
    // Multi-group output (e.g., types, supervisor_config)
    const groups = config.keywords();
    for (const g of groups) {
      output.push(formatKeywordLine(paddedPrefix, g.words, maxLineWidth));
    }
  } else {
    // Chunk keywords into lines of reasonable width
    output.push(formatKeywordLine(paddedPrefix, config.keywords, maxLineWidth));
  }

  // Add extra lines if defined
  if (config.extraLines) {
    for (const extra of config.extraLines) {
      output.push(extra);
    }
  }

  changes.push(category);
}

// -- Format helper ---------------------------------------------------------

function formatKeywordLine(prefix, words, maxWidth) {
  // If all words fit on one line, use one line
  const oneLine = `${prefix} ${words.join(' ')}`;
  if (oneLine.length <= maxWidth) {
    return oneLine;
  }

  // Otherwise split across multiple lines
  const lines = [];
  let current = prefix;
  for (const word of words) {
    const candidate = current + ' ' + word;
    if (candidate.length > maxWidth && current !== prefix) {
      lines.push(current);
      current = prefix + ' ' + word;
    } else {
      current = candidate;
    }
  }
  if (current !== prefix) {
    lines.push(current);
  }
  return lines.join('\n');
}

// -- Write output ----------------------------------------------------------

const result = output.join('\n');

if (result === originalSource) {
  console.log('No changes needed — hew.vim already in sync.');
} else {
  writeFileSync(syntaxFilePath, result);
  console.log(`Patched ${changes.length} @sync section(s):`);
  for (const c of changes) {
    console.log(`  ~ @sync:${c}`);
  }
}

console.log('\nDone.');
