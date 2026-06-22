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
 *   logical, supervisor_config, reserved_unused, contextual, types
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
const contextual = syntaxData.contextual_identifiers;

// -- Hub-derived supervisor + contextual keyword sets ----------------------
// supervisor_config entries that name structural fields are highlighted as
// keywords (hewSupervisor); the rest are restart-strategy / permanence /
// child-kind value constants (hewStrategy). The `supervisor` keyword itself
// is sourced from kw.actors (emitted by the actors category), so it is not
// repeated here.
const supervisorFieldKeywords = ['child', 'restart', 'budget', 'strategy'];
const supervisorConfigFields = kw.supervisor_config.filter(
  w => supervisorFieldKeywords.includes(w));
const supervisorConstants = kw.supervisor_config.filter(
  w => !supervisorFieldKeywords.includes(w));

// Mailbox overflow policy values live in contextual_identifiers, flagged by an
// "overflow" mention in their description. The `overflow` block-header key
// names the field rather than a value, so it is excluded.
const overflowKinds = Object.entries(contextual)
  .filter(([name, desc]) =>
    name !== 'overflow' && typeof desc === 'string' && /overflow/i.test(desc))
  .map(([name]) => name);

// Remaining contextual identifiers: soft keywords that are not reserved and
// not overflow values (within, intensity, initial, repeated, infinity, ...).
const contextualKeywords = Object.keys(contextual)
  .filter(name => name !== 'description' && name !== 'self'
    && !overflowKinds.includes(name));

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
    keywords: [...supervisorConfigFields],
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
    keywords: () => [
      { words: supervisorConstants },
      { words: overflowKinds },
    ],
  },

  reserved_unused: {
    group: 'hewReserved',
    keywords: [...kw.reserved_unused],
  },

  // Contextual identifiers — special meaning in specific parser contexts but
  // NOT reserved keywords (usable as ordinary identifiers elsewhere).
  contextual: {
    group: 'hewContextual',
    keywords: [...contextualKeywords],
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

// -- Verify keyword coverage -----------------------------------------------
// Guard against drift: every keyword the lexer recognises (all_keywords, kept
// in sync with the lexer by a hew-lexer unit test) must be emitted by some
// category. Contextual identifiers, overflow kinds, and types are
// intentionally NOT in all_keywords, so they are excluded from this check.

const coveredKeywords = new Set([
  ...categoryMap.control_flow.keywords,
  ...categoryMap.declarations.keywords,
  ...categoryMap.actors.keywords,
  ...categoryMap.supervisor.keywords,
  ...categoryMap.wire.keywords,
  ...categoryMap.machine.keywords,
  ...categoryMap.other.keywords,
  ...categoryMap.logical.keywords,
  ...categoryMap.reserved_unused.keywords,
  ...supervisorConstants,
]);

const missingKeywords = syntaxData.all_keywords.filter(k => !coveredKeywords.has(k));
if (missingKeywords.length > 0) {
  console.warn('\u26a0 Keywords in all_keywords not emitted by any category:');
  console.warn(`   ${missingKeywords.join(', ')}`);
}

const extraKeywords = [...coveredKeywords].filter(
  k => !syntaxData.all_keywords.includes(k));
if (extraKeywords.length > 0) {
  console.warn('\u26a0 Keywords emitted but not in all_keywords:');
  console.warn(`   ${extraKeywords.join(', ')}`);
}

if (missingKeywords.length === 0 && extraKeywords.length === 0) {
  console.log(
    `Keyword coverage: all ${syntaxData.all_keywords.length} all_keywords emitted.`);
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
