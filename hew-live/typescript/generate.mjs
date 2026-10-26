// Both declarations and executable bindings come from the canonical WIT/component.
import { generateHostTypes, transpile, writeFiles } from '@bytecodealliance/jco-transpile';
import { mkdir, writeFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';

export async function generate(directory, component) {
  await mkdir(directory, { recursive: true });
  await writeFiles(await generateHostTypes(fileURLToPath(new URL('../wit', import.meta.url)), {
    name: 'router', worldName: 'router',
  }), { baseDir: resolve(directory, 'wit') });
  if (component) {
    const { files } = await transpile(component, {
      name: 'router', instantiation: 'async', noComponentErrorWrapping: true,
    });
    await writeFiles(files, { baseDir: resolve(directory, 'component') });
  }
  await writeFile(resolve(directory, 'package.json'), '{"type":"module"}\n');
}

if (process.argv[1] && import.meta.url === pathToFileURL(resolve(process.argv[1])).href) {
  const [, , directory, component] = process.argv;
  if (!directory) throw new Error('usage: generate.mjs OUTPUT_DIRECTORY [COMPONENT_FILE]');
  await generate(directory, component);
}
