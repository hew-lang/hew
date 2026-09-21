import { renderStdout, type VmValue } from "../values.js";

interface Field {
  name: string;
  recipe: number;
}
export type StructuralRecipe =
  | { kind: "scalar" | "unit" }
  | { kind: "display"; callee: number }
  | { kind: "identity"; name: string }
  | { kind: "tuple" | "vector" | "map"; members: number[] }
  | { kind: "record"; name: string; fields: Field[] }
  | {
      kind: "enum";
      cases: Array<{
        name: string;
        kind: "unit" | "tuple" | "struct";
        fields: Field[];
      }>;
    };

type Part = string | { recipe: number; value: VmValue };

/** A borrowed traversal. Display callbacks can suspend, so their callers retain
 * this stack and resume it with the returned string. No later sibling is read
 * after a callback fails. */
export class StructuralRenderer {
  private readonly pending: Part[];
  private readonly text: string[] = [];

  constructor(
    private readonly recipes: StructuralRecipe[],
    recipe: number,
    value: VmValue,
  ) {
    this.pending = [{ recipe, value }];
  }

  append(value: VmValue): void {
    if (value.kind !== "string")
      throw new Error("Display returned a non-string value");
    this.text.push(value.value);
  }

  next(): string | { callee: number; value: VmValue } {
    while (this.pending.length) {
      const part = this.pending.pop()!;
      if (typeof part === "string") {
        this.text.push(part);
        continue;
      }
      const { value } = part;
      const recipe = this.recipes[part.recipe];
      if (!recipe) throw new Error("structural rendering recipe is missing");
      switch (recipe.kind) {
        case "display":
          return { callee: recipe.callee, value };
        case "unit":
          this.text.push("()");
          break;
        case "scalar":
          this.text.push(renderStdout(value));
          break;
        case "identity":
          this.text.push(
            `<${recipe.name}@${"id" in value ? value.id : "opaque"}>`,
          );
          break;
        case "tuple": {
          if (value.kind !== "record")
            throw new Error("structural tuple has no fields");
          this.sequence(
            "(",
            ")",
            value.fields.map((value, i) => [
              { value, recipe: recipe.members[i]! },
            ]),
          );
          break;
        }
        case "vector": {
          if (value.kind !== "vector")
            throw new Error("structural vector has no elements");
          this.sequence(
            "[",
            "]",
            value.items.map((value) => [{ value, recipe: recipe.members[0]! }]),
          );
          break;
        }
        case "map": {
          if (value.kind !== "map")
            throw new Error("structural map has no entries");
          this.sequence(
            "{",
            "}",
            [...value.entries.values()].map(({ key, value }) => [
              { value: key, recipe: recipe.members[0]! },
              ": ",
              { value, recipe: recipe.members[1]! },
            ]),
          );
          break;
        }
        case "record": {
          if (value.kind !== "record")
            throw new Error("structural record has no fields");
          this.sequence(
            `${recipe.name} { `,
            " }",
            recipe.fields.map((field, i) => [
              `${field.name}: `,
              { value: value.fields[i]!, recipe: field.recipe },
            ]),
          );
          break;
        }
        case "enum": {
          if (value.kind !== "enum")
            throw new Error("structural enum has no tag");
          const variant = recipe.cases[value.tag];
          if (!variant) throw new Error("structural enum tag has no case");
          if (variant.kind === "unit") {
            this.text.push(variant.name);
            break;
          }
          const named = variant.kind === "struct";
          this.sequence(
            variant.name + (named ? " { " : "("),
            named ? " }" : ")",
            variant.fields.map((field, i) => [
              ...(named ? [`${field.name}: `] : []),
              { value: value.payload[i]!, recipe: field.recipe },
            ]),
          );
          break;
        }
      }
    }
    return this.text.join("");
  }

  private sequence(open: string, close: string, elements: Part[][]): void {
    const parts: Part[] = [open];
    for (const [index, element] of elements.entries()) {
      if (index) parts.push(", ");
      parts.push(...element);
    }
    parts.push(close);
    this.pending.push(...parts.reverse());
  }
}
