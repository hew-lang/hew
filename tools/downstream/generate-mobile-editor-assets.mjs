#!/usr/bin/env node

/**
 * Mobile editor asset generator for Hew
 *
 * Reads the canonical syntax-data.json from the Hew compiler and generates a
 * mobile-friendly JSON bundle at dist/hew.mobile-editor.json.
 *
 * The bundle is intentionally language-level only: it provides a shared
 * completion catalogue plus grouped lexical/highlighting data that Android and
 * iOS editors can consume without pretending to be a full LSP.
 *
 * Usage: node tools/downstream/generate-mobile-editor-assets.mjs
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
const outputPath = resolve(REPO_ROOT, 'dist/hew.mobile-editor.json');

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
const contextualEntries = Object.entries(syntaxData.contextual_identifiers)
  .filter(([name]) => name !== 'description' && name !== 'self');

const markerTraits = new Set(['Send', 'Frozen', 'Copy']);
const optionResultTypes = new Set(['Option', 'Result']);
const constructors = new Set(['Ok', 'Err', 'Some', 'None']);
const strategyKeywords = pickMembers(
  kw.supervisor_config,
  ['permanent', 'transient', 'temporary'],
  'supervisor permanence keywords',
);
const restartStrategies = pickMembers(
  kw.supervisor_config,
  ['one_for_one', 'one_for_all', 'rest_for_one'],
  'supervisor restart strategies',
);
const actorControlFlow = pickMembers(
  kw.actors,
  ['select', 'join', 'after', 'from', 'await', 'scope', 'cooperate'],
  'actor control-flow keywords',
);
const actorDeclarationKeywords = kw.actors.filter(
  (name) => name !== 'supervisor' && !actorControlFlow.includes(name),
);
const supervisorKeywords = ['supervisor', ...kw.supervisor_config.filter(
  (name) => !strategyKeywords.includes(name) && !restartStrategies.includes(name),
)];
const overflowKinds = contextualEntries
  .filter(([, detail]) => detail.startsWith('Overflow policy:'))
  .map(([name]) => name);
const wireTypes = [
  ...types.integer.filter((name) => name !== 'isize' && name !== 'usize'),
  ...types.float,
  ...types.primitive.filter((name) => !['char', 'void', 'never', 'duration'].includes(name)),
];

const completionItems = dedupeByLabel([
  ...buildKeywordCompletions(),
  ...buildTypeCompletions(),
  ...buildContextualCompletions(),
]);

const bundle = {
  schema_version: 1,
  syntax_version: syntaxData.version,
  description: 'Shared Hew language/editor assets for mobile clients.',
  generated_from: 'docs/syntax-data.json',
  generated_by: 'tools/downstream/generate-mobile-editor-assets.mjs',
  completion_catalog: {
    description: 'Language-level completions. Merge these with local symbols in the client.',
    items: completionItems,
  },
  lexical: {
    all_keywords: syntaxData.all_keywords,
    keywords: kw,
    contextual_identifiers: Object.fromEntries(contextualEntries),
    types,
    operators: syntaxData.operators,
    string_prefixes: syntaxData.string_prefixes,
    comment_styles: syntaxData.comment_styles,
    literal_suffixes: syntaxData.literal_suffixes,
  },
  highlighting: {
    description: 'Grouped tokens for mobile highlighters and other downstream integrations.',
    source_groups: {
      keywords: kw,
      contextual_identifiers: Object.fromEntries(contextualEntries),
      types,
      operators: syntaxData.operators,
      string_prefixes: syntaxData.string_prefixes,
      comment_styles: syntaxData.comment_styles,
      literal_suffixes: syntaxData.literal_suffixes,
    },
    textmate_scopes: [
      scopeGroup('keyword.control.hew', unique([...kw.control_flow, ...actorControlFlow])),
      scopeGroup('keyword.declaration.hew', kw.declarations),
      scopeGroup('keyword.actor.hew', actorDeclarationKeywords),
      scopeGroup('keyword.supervisor.hew', supervisorKeywords),
      scopeGroup('constant.language.strategy.hew', [...strategyKeywords, ...restartStrategies]),
      scopeGroup('keyword.wire.hew', kw.wire),
      scopeGroup('keyword.control.machine.hew', kw.machine),
      scopeGroup('keyword.other.hew', kw.other),
      scopeGroup('constant.language.boolean.hew', kw.logical),
      scopeGroup('constant.language.none.hew', ['None']),
      scopeGroup('keyword.reserved.hew', kw.reserved_unused),
      scopeGroup('storage.type.numeric.hew', [...types.integer, ...types.float]),
      scopeGroup('storage.type.primitive.hew', types.primitive),
      scopeGroup(
        'storage.type.generic.hew',
        [
          ...types.collections,
          ...(types.option_result || []).filter((name) => name !== 'None' && !markerTraits.has(name)),
          ...(types.marker_traits || []).filter((name) => !markerTraits.has(name)),
          ...(types.other || []),
        ],
      ),
      scopeGroup('storage.type.concurrency.hew', types.concurrency),
      scopeGroup(
        'storage.type.trait.hew',
        (types.marker_traits || []).filter((name) => markerTraits.has(name)),
      ),
      scopeGroup('variable.language.contextual.hew', contextualEntries.map(([name]) => name)),
    ],
  },
  downstream: {
    description: 'Pre-grouped buckets that mirror existing downstream grammar tooling.',
    tree_sitter_sync: {
      primitive_types: [
        ...types.integer,
        ...types.float,
        ...types.primitive.filter((name) => name !== 'never'),
      ],
      wire_types: wireTypes,
      wire_attributes: kw.wire.filter((name) => !['wire', 'default', 'reserved'].includes(name)),
      overflow_kinds: overflowKinds,
      restart_permanence: kw.supervisor_config.filter((name) => strategyKeywords.includes(name)),
      restart_strategies: kw.supervisor_config.filter((name) => restartStrategies.includes(name)),
      duration_suffixes: syntaxData.literal_suffixes?.duration || ['ns', 'us', 'ms', 's', 'm', 'h'],
      assignment_operators: syntaxData.operators.assignment,
      boolean_literals: kw.logical,
    },
  },
};

mkdirSync(dirname(outputPath), { recursive: true });
writeFileSync(outputPath, JSON.stringify(bundle, null, 2) + '\n');

console.log(`Mobile editor bundle generated from syntax-data.json v${syntaxData.version}`);
console.log(`Completion items: ${completionItems.length}`);
console.log(`TextMate scope groups: ${bundle.highlighting.textmate_scopes.length}`);

function buildKeywordCompletions() {
  const keywordGroups = [
    {
      group: 'control_flow',
      kind: 'keyword',
      detail: 'Control flow keyword',
      labels: kw.control_flow,
    },
    {
      group: 'declarations',
      kind: 'keyword',
      detail: 'Declaration keyword',
      labels: kw.declarations,
    },
    {
      group: 'actors',
      kind: 'keyword',
      detail: 'Actor and concurrency keyword',
      labels: kw.actors,
    },
    {
      group: 'wire',
      kind: 'keyword',
      detail: 'Wire declaration keyword',
      labels: kw.wire,
    },
    {
      group: 'supervisor_config',
      kind: 'keyword',
      detail: 'Supervisor configuration keyword',
      labels: kw.supervisor_config,
    },
    {
      group: 'logical',
      kind: 'constant',
      detail: 'Boolean literal',
      labels: kw.logical,
    },
    {
      group: 'machine',
      kind: 'keyword',
      detail: 'State machine keyword',
      labels: kw.machine,
    },
    {
      group: 'other',
      kind: 'keyword',
      detail: 'Other language keyword',
      labels: kw.other,
    },
    {
      group: 'reserved_unused',
      kind: 'keyword',
      detail: 'Reserved keyword',
      labels: kw.reserved_unused,
    },
  ];

  return keywordGroups.flatMap(({ group, kind, detail, labels }) => labels.map((label) => ({
    label,
    insert_text: label,
    kind,
    group,
    detail,
  })));
}

function buildTypeCompletions() {
  const typeGroups = [
    { group: 'integer', detail: 'Integer type' },
    { group: 'float', detail: 'Floating-point type' },
    { group: 'primitive', detail: 'Primitive type' },
    { group: 'collections', detail: 'Collection type' },
    { group: 'option_result', detail: 'Option/Result type or constructor' },
    { group: 'concurrency', detail: 'Concurrency type' },
    { group: 'marker_traits', detail: 'Marker trait or shared-pointer type' },
    { group: 'other', detail: 'Other standard type' },
  ];

  return typeGroups.flatMap(({ group, detail }) => (types[group] || []).map((label) => ({
    label,
    insert_text: label,
    kind: typeCompletionKind(label),
    group,
    detail: typeCompletionDetail(label, detail),
  })));
}

function buildContextualCompletions() {
  return contextualEntries.map(([label, detail]) => ({
    label,
    insert_text: label,
    kind: 'contextual_identifier',
    group: 'contextual_identifiers',
    detail,
  }));
}

function typeCompletionKind(label) {
  if (constructors.has(label)) {
    return 'constructor';
  }
  if (optionResultTypes.has(label)) {
    return 'type';
  }
  if (markerTraits.has(label)) {
    return 'trait';
  }
  return 'type';
}

function typeCompletionDetail(label, fallback) {
  if (constructors.has(label)) {
    return 'Option/Result constructor';
  }
  return fallback;
}

function scopeGroup(scope, tokens) {
  return {
    scope,
    tokens: unique(tokens),
  };
}

function unique(values) {
  return [...new Set(values)];
}

function dedupeByLabel(items) {
  const seen = new Set();
  return items.filter((item) => {
    if (seen.has(item.label)) {
      return false;
    }
    seen.add(item.label);
    return true;
  });
}

function pickMembers(source, names, label) {
  const selected = source.filter((name) => names.includes(name));
  if (selected.length !== names.length) {
    const missing = names.filter((name) => !selected.includes(name));
    throw new Error(`Missing ${label} in syntax-data.json: ${missing.join(', ')}`);
  }
  return selected;
}
